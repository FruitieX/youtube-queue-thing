module Frontend where

import Prelude
import Types

import Component as Component
import Control.Coroutine (Await, transform, (\/), (~$))
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff, delay, launchAff_)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free.Trans (FreeT)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(Left), hush)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Monoid (mempty)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Simple.JSON as JSON
import SockJS.Client as SockJS

serverUrl :: SockJS.Url
serverUrl = "http://localhost:8080/sockjs"

-- TODO: Producer / Consumer may not be the best thing for this
-- A producer coroutine that connects to a SockJS server
-- and emits SockJS connections whenever (re)connecting
wsProducer
  :: SockJS.Url
  -> CR.Producer SockJS.Connection (Aff _) Unit
wsProducer url = CRA.produce \emit -> do
  connect emit

  where
    connect = \emit -> do
      sock <- SockJS.connect url
      SockJS.onClose sock $ launchAff_ $ reconnect emit
      emit (Left sock)
      --SockJS.onMessage sock $ \message -> emit (Left message)
    reconnect = \emit -> do
      log "Reconnecting in 1 sec..."
      delay (wrap 1000.0)
      liftEff $ connect emit

-- A consumer coroutine that takes output messages from our component IO
-- and sends them using the websocket
wsSender
  :: forall eff
   . SockJS.Connection
  -> CR.Consumer Message (Aff _) Unit
wsSender sock = CR.consumer \msg -> do
  let json = JSON.writeJSON msg
  log $ "writing JSON: " <> json
  liftEff $ SockJS.send sock json
  pure Nothing

wsConsumer
  :: (FreeT (Await Message) (Aff _) Unit -> Aff _ Unit)
  -> CR.Consumer SockJS.Connection (Aff _) Unit
wsConsumer subscribe = CR.consumer \sock -> do
  subscribe $ wsSender sock
  pure Nothing

wsConsumer'
  :: (Component.Query ~> Aff (HA.HalogenEffects _))
  -> CR.Consumer SockJS.Connection (Aff _) Unit
wsConsumer' query = CR.consumer \sock -> do
  liftEff $ SockJS.onMessage sock \message -> do
    let (decoded :: Maybe Message) = hush $ JSON.readJSON message

    launchAff_ $ do
      maybe mempty (\a -> query $ H.action $ Component.IncomingSockMsg a) decoded
      pure unit
  pure Nothing

main :: Eff _ Unit
main = launchAff_ do
  body <- HA.awaitBody
  io <- runUI Component.component unit body

  CR.runProcess $
    wsProducer serverUrl
      CR.$$ (forever $ transform (\i -> Tuple i i))
        -- Attaches component io to new SockJS connections (outbound messages)
        ~$ (wsConsumer io.subscribe)
        
        -- Attaches new SockJS connections to component io (inbound messages)
        \/ (wsConsumer' io.query)
  pure unit

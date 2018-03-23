module Frontend where

import Prelude
import Types

import Component as Component
import Control.Coroutine (Await(..), joinConsumers, transform, transformConsumer, (\/), (~$))
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff, delay, forkAff, launchAff, launchAff_, runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free.Trans (FreeT)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(Left), hush)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid (mempty)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.Query.HalogenM as HM
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

-- wsProducer
--   :: SockJS.Url
--   -> _
--   -> CR.Producer String (Aff _) Unit
-- wsProducer url io = CRA.produce \emit -> do
--   connect emit
--
--   where
--     connect = \emit -> do
--       sock <- liftEff $ SockJS.connect url
--       SockJS.onMessage sock $ \message -> emit (Left message)
--       _ <- io.subscribe $ wsSender sock
--       liftEff $ SockJS.onClose sock $ launchAff_ $ reconnect emit
--     reconnect = \emit -> do
--       log "Reconnecting in 1 sec..."
--       delay (wrap 1000.0)
--       liftEff $ connect emit

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

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `AddMessage` queries in when it receives inputs from the
-- producer.
-- wsConsumer
--   :: (Component.Query ~> Aff (HA.HalogenEffects _))
--   -> CR.Consumer String (Aff (HA.HalogenEffects _)) Unit
-- wsConsumer query = CR.consumer \msg -> do
--   query $ H.action $ Component.AddMessage msg
--   pure Nothing
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
      --log $ "Received: " <> message
      maybe mempty (\a -> query $ H.action $ Component.IncomingSockMsg a) decoded
      --maybe ?asd $ (query H.action) decoded
      --maybe ?asd (query $ H.action $ Component.IncomingSockMsg) decoded
      pure unit
  pure Nothing

main :: Eff _ Unit
main = launchAff_ do
  body <- HA.awaitBody
  io <- runUI Component.component unit body

  --CR.runProcess (wsProducer serverUrl (\sock -> io.subscribe $ wsSender sock) CR.$$ wsConsumer io.query)
  CR.runProcess $
    wsProducer serverUrl
      CR.$$ (forever $ transform (\i -> Tuple i i))
        ~$ (wsConsumer io.subscribe)
        \/ (wsConsumer' io.query)
  pure unit
  --io.subscribe $ wsSender sock

module Main where

import Prelude
import Types

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.State (State, StateT(..), execStateT, get, runState, runStateT)
import Control.Monad.Trampoline (done)
import Data.Array (cons, deleteAt, elemIndex, findIndex, length, snoc)
import Data.Char.Unicode.Internal (uGencat)
import Data.Either (hush)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid (mempty)
import Data.StrMap (StrMap, delete, empty, fold, insert, keys)
import Data.Traversable (traverse, traverse_)
import Data.UUID (genUUID)
import Global.Unsafe (unsafeStringify)
import Node.HTTP as HTTP
import Node.Stream (end)
import Simple.JSON (readJSON, writeJSON)
import SockJS.Server as SockJS
import Unsafe.Coerce (unsafeCoerce)

initState
  :: forall e
   . AppState
initState =
  { queue: []
  , current: 0.0
  , isPlaying: false
  , shouldPlay: false
  }

handleMessage
  :: forall e
   . AppState
  -> Message
  -> AppState
handleMessage state (PlayPause message) = do
  state { shouldPlay = message.play }
handleMessage state (Skip message) = do
  state { current = state.current + message.skip }
handleMessage state (Seek message) = do
  -- TODO
  state
handleMessage state (Enqueue message) = do
  state { queue = snoc state.queue message.enqueue }
handleMessage state (Dequeue message) = do
  let index = findIndex (\e -> e.id == message.dequeue) state.queue

  let newQueue = (flip deleteAt state.queue) =<< index
  maybe state (state { queue = _ }) newQueue

decodeMessage
  :: forall e
   . SockJS.Message
  -> Maybe Message
decodeMessage message = do
  hush $ readJSON message

type Clients = StrMap SockJS.Connection

broadcast
  :: Ref Clients
  -> String
  -> (Eff _) Unit
broadcast clients message = do
  curClients <- readRef clients
  _ <- traverse (\client -> do
    SockJS.write client message) curClients
  pure unit

onData
  :: Ref AppState
  -> Ref Clients
  -> String
  -> (Eff _) Unit
onData state clients message = do
  curState <- readRef state
  let decoded = decodeMessage message
  let nextState = handleMessage curState <$> decoded
  traverse_ (writeRef state) nextState
  maybe mempty (\s -> broadcast clients $ writeJSON s) nextState
  pure unit

runApp
 :: forall e
  . SockJS.Server
 -> (Eff _) Unit
runApp sockjs = do
  state :: Ref AppState <- newRef initState
  clients :: Ref Clients <- newRef empty

  SockJS.onConnection sockjs \conn -> do
    log "client connected"

    -- Generate UUID for client, add to list of clients
    uuid <- show <$> genUUID
    modifyRef clients (\c -> insert uuid conn c)

    -- Send current state to client
    curState <- readRef state
    SockJS.write conn $ writeJSON curState

    -- Attach event handlers
    SockJS.onData conn $ onData state clients
    SockJS.onClose conn do
      modifyRef clients (\c -> delete uuid c)
      log "client disconnected"

main
  :: forall e
   . Eff _ Unit
main = do
  http <- HTTP.createServer $
    -- dummy Node.HTTP request handler
    \req res -> end (HTTP.responseAsStream res) $ pure unit

  sockjs <- SockJS.createServer
  SockJS.installHandlers sockjs http "/sockjs"

  runApp sockjs

  HTTP.listen http { hostname: "localhost", port: 8080, backlog: Nothing } $
    log "listening on port 8080."

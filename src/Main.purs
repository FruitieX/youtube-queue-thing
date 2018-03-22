module Main where

import Prelude
import Types

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (Ref, modifyRef, newRef, readRef, writeRef)
import Data.Array (concat, deleteAt, length, snoc, take, takeEnd)
import Data.Either (hush)
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Monoid (mempty)
import Data.StrMap (StrMap, delete, empty, insert)
import Data.Traversable (traverse, traverse_)
import Data.UUID (genUUID)
import Node.HTTP as HTTP
import Node.Stream (end)
import Simple.JSON (readJSON, writeJSON)
import SockJS.Server as SockJS

type Clients = StrMap SockJS.Connection

handleMessage
  :: AppState
  -> Message
  -> AppState
handleMessage state (PlayPause {play}) = do
  state
    { play = play }

-- Skip forward / backwards in queue
handleMessage state (Skip {skip}) = do
  -- Concat all videos into one array
  let all = concat [state.history, state.queue]

  -- Get new queue by taking queueLength items from 'all'
  let queueLength = length state.queue - skip
  -- Rest of items from 'all' go into history (max 1000 items)
  let historyLength = max 1000 $ length all - queueLength

  state
    { queue = takeEnd queueLength all
    , history = take historyLength all
    , seek = 0.0
    }

-- Seek in current video
handleMessage state (Seek {seek}) = do
  state
    { seek = seek }

-- Adds video to queue
handleMessage state (Enqueue {enqueue}) = do
  state
    { queue = snoc state.queue enqueue }

-- Removes video from queue
handleMessage state (Dequeue {dequeue}) = do
  --let index = findIndex (\e -> e.id == message.dequeue) state.queue

  --let newQueue = (flip deleteAt state.queue) =<< index
  let newQueue = deleteAt dequeue state.queue
  maybe state (state { queue = _ }) newQueue

-- Ignore State messages, they are meant for server -> client communication only
handleMessage state (State _) = state

decodeMessage
  :: SockJS.Message
  -> Maybe Message
decodeMessage message = do
  hush $ readJSON message

broadcast
  :: Ref Clients
  -> String
  -> (Eff _) Unit
broadcast clients message = do
  curClients <- readRef clients
  _ <- traverse (\client -> do
    SockJS.write client message) curClients
  pure unit

stateMsg
  :: AppState
  -> StateMessage
stateMsg state = { state }

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

  -- Broadcast new state
  maybe mempty (\s -> broadcast clients $ writeJSON $ stateMsg s) nextState
  pure unit

runApp
 :: SockJS.Server
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
    SockJS.write conn $ writeJSON $ stateMsg curState

    -- Attach event handlers
    SockJS.onData conn $ onData state clients
    SockJS.onClose conn do
      modifyRef clients (\c -> delete uuid c)
      log "client disconnected"

main
  :: Eff _ Unit
main = do
  http <- HTTP.createServer $
    -- dummy Node.HTTP request handler
    \req res -> end (HTTP.responseAsStream res) $ pure unit

  sockjs <- SockJS.createServer
  SockJS.installHandlers sockjs http "/sockjs"

  runApp sockjs

  HTTP.listen http { hostname: "localhost", port: 8080, backlog: Nothing } $
    log "listening on port 8080."

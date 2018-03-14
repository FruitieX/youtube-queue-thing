module Main where

import Prelude
import Types

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Global.Unsafe (unsafeStringify)
import Node.HTTP as HTTP
import Node.Stream (end)
import Simple.JSON (readJSON, writeJSON)
import SockJS.Server as SockJS

handleMessage
  :: forall e
   . SockJS.Connection
  -> Message
  -> Eff (console :: CONSOLE | e) Unit
handleMessage conn (CurrentQueue message) = do
  log $ "queue message: " <> unsafeStringify message
handleMessage conn (PlayPause message) = do
  log $ "play/pause message: " <> unsafeStringify message
handleMessage conn (Skip message) = do
  log $ "skip message: " <> unsafeStringify message
handleMessage conn (Seek message) = do
  log $ "seek message: " <> unsafeStringify message

decodeMessage
  :: forall e
   . SockJS.Connection
  -> SockJS.Message
  -> Eff (console :: CONSOLE | e) Unit
decodeMessage conn message = do
  let (decoded :: Maybe Message) = hush $ readJSON message
  case decoded of
    Just decoded' -> do
      handleMessage conn decoded'
    _ -> SockJS.write conn message

handleConnection
 :: forall e
  . Ref AppState
 -> SockJS.Connection
 -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleConnection state conn = do
  log "client connected"
  SockJS.onData conn $ decodeMessage conn
  SockJS.onClose conn $ log "client disconnected"

  curState <- readRef state
  SockJS.write conn $ writeJSON curState

initState
  :: forall e
   . Eff (ref :: REF | e) (Ref AppState)
initState = newRef
  { queue: []
  , current: 0.0
  , isPlaying: false
  , shouldPlay: false
  }

main
  :: forall e
   . Eff (
       http :: HTTP.HTTP,
       console :: CONSOLE,
       ref :: REF
       | e
     ) Unit
main = do
  http <- HTTP.createServer $
    -- dummy Node.HTTP request handler
    \req res -> end (HTTP.responseAsStream res) $ pure unit

  state <- initState
  sockjs <- SockJS.createServer
  SockJS.installHandlers sockjs http "/sockjs"
  SockJS.onConnection sockjs $ handleConnection state

  HTTP.listen http { hostname: "localhost", port: 8080, backlog: Nothing } $
    log "listening on port 8080."

module YouTube where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff, mkEffFn1)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, runEffFn2, runEffFn3)

type Id = String
type Func = String
type Args = Array String

foreign import data Player :: Type

data PlayerState
  = Unstarted
  | Ended
  | Playing
  | Paused
  | Buffering
  | Cued
  | Unknown

parsePlayerState :: Int -> PlayerState
parsePlayerState playerState =
  case playerState of
    -1 -> Unstarted
    0  -> Ended
    1  -> Playing
    2  -> Paused
    3  -> Buffering
    5  -> Cued
    _  -> Unknown

foreign import callPlayer_
  :: forall e. EffFn3 e Player Func Args Unit

callPlayer
  :: forall e
   . Player
  -> Func
  -> Args
  -> Eff e Unit
callPlayer = runEffFn3 callPlayer_

foreign import attachPlayerStateHandler_
  :: forall e
   . EffFn2 e Player (EffFn1 e Int Unit) Unit

attachPlayerStateHandler
  :: forall e
   . Player
  -> (PlayerState -> Eff e Unit)
  -> Eff e Unit
attachPlayerStateHandler player handler =
  runEffFn2 attachPlayerStateHandler_ player $
    mkEffFn1 (handler <<< parsePlayerState)

foreign import initPlayer_
  :: forall e
   . Id
  -> EffFnAff (| e) Player

initPlayer
  :: forall e
   . Id
  -> Aff (| e) Player
initPlayer = fromEffFnAff <<< initPlayer_

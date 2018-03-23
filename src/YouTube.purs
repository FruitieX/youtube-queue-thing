module YouTube where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff, mkEffFn1)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, runEffFn1, runEffFn2, runEffFn3, runEffFn4)

type Func = String
type Args = Array String

foreign import data Player :: Type

foreign import callPlayer_
  :: forall e. EffFn3 e Player Func Args Unit

callPlayer
  :: forall e
   . Player
  -> Func
  -> Args
  -> Eff e Unit
callPlayer = runEffFn3 callPlayer_

foreign import initPlayer_
  :: forall e
   . String
  -> EffFn1 e String Unit
  -> EffFnAff (| e) Player

initPlayer
  :: forall e
   . String
  -> (String -> Eff e Unit)
  -> Aff (| e) Player
initPlayer id onStateChange = fromEffFnAff $
  initPlayer_ id (mkEffFn1 onStateChange)

module YouTube where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)

type Func = String
type Args = Array String

foreign import callPlayer_
  :: forall e
   . EffFn2 e
       Func
       Args
       Unit

callPlayer
  :: forall e
   . Func
  -> Args
  -> Eff e Unit
callPlayer func args =
  runEffFn2 callPlayer_ func args

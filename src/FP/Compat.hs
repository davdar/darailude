module FP.Compat where

import FP.Core
import qualified Prelude

instance (Monad m) => Prelude.Functor m where { fmap = FP.Core.mmap }
instance (Monad m) => Prelude.Applicative m where { pure = FP.Core.return ; (<*>) = FP.Core.mapply }
instance (Monad m) => Prelude.Monad m where { return = FP.Core.return ; (>>=) = (FP.Core.>>=) }

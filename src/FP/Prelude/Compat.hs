module FP.Prelude.Compat where

import FP.Prelude.Core
import qualified Prelude

instance (Functor m) ⇒ Prelude.Functor m where { fmap = FP.Prelude.Core.map }
instance (Monad m) ⇒ Prelude.Applicative m where { pure = FP.Prelude.Core.return ; (<*>) = FP.Prelude.Core.mapply }
instance (Monad m) ⇒ Prelude.Monad m where { return = FP.Prelude.Core.return ; (>>=) = (FP.Prelude.Core.≫=) }

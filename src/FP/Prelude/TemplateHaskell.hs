module FP.Prelude.TemplateHaskell where

import FP.Prelude.Core
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import FP.Prelude.Lens
import FP.Prelude.Effects
import FP.Prelude.Monads ()
import FP.Prelude.DSL
import FP.Prelude.Lib

import qualified Prelude

class MonadQ (m ∷ ★ → ★) where qio ∷ Q a → m a

instance Functor Q where map = mmap
instance Monad Q where {return = Prelude.return;(≫=) = (Prelude.>>=)}
instance MonadIO Q where io = runIO
instance MonadQ Q where qio = id


instance Apply Exp where (⋅) = AppE
instance Tup Exp where tup = TupE
instance Apply Type where (⋅) = AppT
instance Tup Type where tup ts = TupleT (𝕚 $ length ts) ⋅| ts
instance Tup Pat where tup = TupP
instance Arrow Type where f ⇨ x = ArrowT ⋅ f ⋅ x

makeList ∷ [Exp] → Exp
makeList xs = foldrOn xs (ConE '[]) $ \ e es → ConE '(:) ⋅ e ⋅ es

makeString ∷ 𝕊 → Exp
makeString = LitE ∘ StringL ∘ chars
      
conName ∷ Con → Name
conName (NormalC n _) = n
conName (RecC n _) = n
conName (InfixC _ n _) = n
conName (ForallC _ _ c) = conName c

tyVarBndrName ∷ TyVarBndr → Name
tyVarBndrName (PlainTV name) = name
tyVarBndrName (KindedTV name _) = name

sclause ∷ [Pat] → Exp → Clause
sclause p b = Clause p (NormalB b) []

smatch ∷ Pat → Exp → Match
smatch p b = Match p (NormalB b) []

coerceSimpleCon ∷ Con → Maybe (Name, [Type])
coerceSimpleCon (NormalC name strictTypes) = Just (name, map snd strictTypes)
coerceSimpleCon (RecC name varStrictTypes) = Just (name, map ff varStrictTypes)
  where ff (_,_,x) = x
coerceSimpleCon (InfixC (_, typeL) name (_, typeR)) = Just (name, [typeL, typeR])
coerceSimpleCon (ForallC _ _ _) = Nothing

tyConIL ∷ Prism Info Dec
tyConIL = Prism
  { view = \case
      TyConI d → Just d
      _ → Nothing
  , inject = TyConI
  }

dataDL ∷ Prism Dec (Cxt, Name, [TyVarBndr], [Con], [Name])
dataDL = Prism
  { view = \case
      DataD cx t args cs ders → Just (cx, t, args, cs, ders)
      _ → Nothing
  , inject = \ (cx, t, args, cs, ders) → DataD cx t args cs ders
  }

newtypeDL ∷ Prism Dec (Cxt, Name, [TyVarBndr], Con, [Name])
newtypeDL = Prism
  { view = \case
      NewtypeD cx t args c ders → Just (cx, t, args, c, ders)
      _ → Nothing
  , inject = \ (cx, t, args, c, ders) → NewtypeD cx t args c ders
  }

coerceADT ∷ Dec → Maybe (Cxt, Name, [TyVarBndr], [Con], [Name])
coerceADT d =
  view dataDL d
  <|>
  (ff ^∘ view newtypeDL) d
  where
    ff (cx, t, args, c, ders) = (cx, t, args, [c], ders)

coerceSingleConADT ∷ Dec → Maybe (Cxt, Name, [TyVarBndr], Con, [Name])
coerceSingleConADT dec = do
  (cx, t, args, cs, ders) ← coerceADT dec
  c ← view singleL cs
  return(cx, t, args, c, ders)

recCL ∷ Prism Con (Name, [VarStrictType])
recCL = Prism
  { view = \case
      RecC n fs → Just (n, fs)
      _ → Nothing
  , inject = \ (n, fs) → RecC n fs
  }

module FP.Deriving.JoinLattice where

import FP.Core
import FP.TemplateHaskell
import Language.Haskell.TH
import FP.Lib

-- makeJoinLatticeLogic [C, D] ty [a, b] con [f1ty, f2ty] := [|
--   instance (JoinLattice f1ty, JoinLattice f2ty) => JoinLattice (ty a b) where
--     bot = con null null
--     con x1 x2 \/ con y1 y2 = con (x1 \/ y1) (x2 \/ y2)
-- |]
makeJoinLatticeLogic :: (Monad m, MonadQ m) => Cxt -> Name -> [TyVarBndr] -> Name -> [Type] -> m [Dec]
makeJoinLatticeLogic cx ty tyargs con fieldtys = do
  xs <- qio $ mapOnM fieldtys $ const $ newName $ chars "x"
  ys <- qio $ mapOnM fieldtys $ const $ newName $ chars "y"
  return $ list $ single $
    InstanceD 
      (list $ uniques $ concat [cx , map (AppT $ ConT ''JoinLattice) fieldtys])
      (ConT ''JoinLattice #@ (ConT ty #@| map (VarT ∘ tyVarBndrName) tyargs))
      [ FunD 'bot $ list $ single $ sclause [] $ 
          ConE con #@| (mapOn fieldtys $ const $ VarE 'bot)
      , FunD '(⊔) $ list $ single $ sclause [ConP con $ map VarP xs, ConP con $ map VarP ys] $
          ConE con #@| mapOn (unsafeCoerce justL $ zipMatch xs ys) (uncurry $ \ x y -> VarE '(⊔) #@ VarE x #@ VarE y)
      ]

makeJoinLattice :: Name -> Q [Dec]
makeJoinLattice name = do
  (cx, ty, tyargs, c, _) <- botMaybe ∘ (coerceSingleConADT *∘ view tyConIL) *$ qio $ reify name
  (con, fieldtys) <- botMaybe $ coerceSimpleCon c
  makeJoinLatticeLogic cx ty tyargs con fieldtys


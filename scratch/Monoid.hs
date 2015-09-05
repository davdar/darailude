module FP.Deriving.Monoid where

import FP.Core
import FP.TemplateHaskell
import Language.Haskell.TH
import FP.Lib

-- makeMonoidLogic [C, D] ty [a, b] con [f1ty, f2ty] := [|
--   instance (C, D, Monoid f1ty, Monoid f2ty) => Monoid (ty a b) where
--     null = con null null
--     con x1 x2 ++ con y1 y2 = con (x1 ++ y1) (x2 ++ y2)
-- |]
makeMonoidLogic :: (Monad m, MonadQ m) => Cxt -> Name -> [TyVarBndr] -> Name -> [Type] -> m [Dec]
makeMonoidLogic cx ty tyargs con fieldtys = do
  xys <- qio $ mapOnM fieldtys $ const $ newName (chars "x") `mpair` newName (chars "y")
  let xs = map fst xys
      ys = map snd xys
  return $ list $ single $
    InstanceD 
      (list $ uniques $ concat [cx , map (AppT $ ConT ''Monoid) fieldtys])
      (ConT ''Monoid #@ (ConT ty #@| map (VarT ∘ tyVarBndrName) tyargs))
      [ FunD 'null $ list $ single $ sclause [] $ 
          ConE con #@| (mapOn fieldtys $ const $ VarE 'null)
      , FunD '(++) $ list $ single $ sclause [ConP con $ map VarP xs, ConP con $ map VarP ys] $
          ConE con #@| mapOn xys (uncurry $ \ x y -> VarE '(++) #@ VarE x #@ VarE y)
      ]

makeMonoid :: Name -> Q [Dec]
makeMonoid name = do
  (cx, ty, tyargs, c, _) <- botMaybe ∘ (coerceSingleConADT *∘ view tyConIL) *$ qio $ reify name
  (con, fieldtys) <- botMaybe $ coerceSimpleCon c
  makeMonoidLogic cx ty tyargs con fieldtys

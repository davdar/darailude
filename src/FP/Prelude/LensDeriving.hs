module FP.Prelude.LensDeriving where

import FP.Prelude.DSL
import FP.Prelude.Core
import FP.Prelude.Lens
import FP.Prelude.TemplateHaskell
import FP.Prelude.Lib
import Language.Haskell.TH

-- makeLensLogic [C, D] ty [a, b] field fieldty := [|
--   fieldL :: (C, D) => Lens (ty a b) fieldty
--   fieldL := lens field (\ x s -> s { field = x })
-- |]
makeLensLogic :: (Monad m, MonadQ m) => Cxt -> Name -> [TyVarBndr] -> Name -> Type -> m [Dec]
makeLensLogic cx ty tyargs field fieldty = do
  let lensName = mkName $ nameBase field ⧺ chars "L"
  x <- qio $ newName $ chars "x"
  s <- qio $ newName $ chars "s"
  return
    [ SigD lensName $ 
        ForallT tyargs cx $
          ConT ''Lens ⋅ (ConT ty ⋅| map (VarT ∘ tyVarBndrName) tyargs) ⋅ fieldty
    , FunD lensName
        [ sclause [] $ 
            VarE 'lens ⋅ VarE field ⋅ LamE [VarP s, VarP x] (RecUpdE (VarE s) [(field, VarE x)])
        ]
    ]

makeLenses :: Name -> Q [Dec]
makeLenses name = do
  (cx, ty, tyargs, c, _) <- returnMaybe abortIO ∘ (coerceSingleConADT *∘ view tyConIL) *$ qio $ reify name
  (_, fields) <- returnMaybe abortIO $ view recCL c
  concat ^$ mapMOn fields $ \ (field, _, fieldty) -> do
    makeLensLogic cx ty tyargs field fieldty

-- makePrismLogic [C, D] ty [a, b] Con [fty, gty] := [|
--   fieldL :: (C, D) => Prism (ty a b) (fty, bty)
--   fieldL := Prism 
--     { view = \ v -> case v of
--         Con f g -> Just (f, g)
--         _ -> Nothing
--     , inject = Con
--     }
-- |]
makePrismLogic :: (Monad m, MonadQ m) => Cxt -> Name -> [TyVarBndr] -> Name -> [Type] -> ℕ -> m [Dec]
makePrismLogic cx ty tyargs con args numcons = do
  let lensName = mkName $ chars (𝕤 $ mapHead lowerChar $ nameBase con) ⧺ chars "L"
  x <- qio $ newName $ chars "x"
  argVars <- qio $ mapMOn args $ const $ newName $ chars "a"
  return
    [ SigD lensName $ 
        ForallT tyargs cx $ (⋅|) (ConT ''Prism)
          [ ConT ty ⋅| map (VarT ∘ tyVarBndrName) tyargs
          , tup args
          ]
    , FunD lensName
        [ sclause [] $ (⋅|) (ConE 'Prism)
            [ LamE [tup $ map VarP argVars] $ ConE con ⋅| map VarE argVars
            , LamE [VarP x] $ 
                CaseE (VarE x) $ concat
                  [ list $ single $ smatch (ConP con $ map VarP argVars) $ 
                      ConE 'Just ⋅ tup (map VarE argVars)
                  , if numcons <= 𝕟 1 then [] else list $ single $ smatch WildP $ ConE 'Nothing
                  ]
            ]
        ]
    ]

makePrisms :: Name -> Q [Dec]
makePrisms name = do
  (cx, ty, tyargs, cs, _) <- returnMaybe abortIO ∘ (coerceADT *∘ view tyConIL) *$ qio $ reify name
  scs <- mapM (returnMaybe abortIO ∘ coerceSimpleCon) cs
  concat ^$ mapMOn scs $ \ (cname, args) -> do
    makePrismLogic cx ty tyargs cname args $ length scs


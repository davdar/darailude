module FP.Deriving.Prism where

import FP.Core
import FP.TH
import Language.Haskell.TH
import Data.Char

-- makePrismLogic [C, D] ty [a, b] Con [fty, gty] := [|
--   fieldL :: (C, D) => Prism (ty a b) (fty, bty)
--   fieldL := Prism 
--     { view = \ v -> case v of
--         Con f g -> Just (f, g)
--         _ -> Nothing
--     , inject = Con
--     }
-- |]
makePrismLogic :: (Monad m, MonadQ m) => Cxt -> Name -> [TyVarBndr] -> Name -> [Type] -> Int -> m [Dec]
makePrismLogic cx ty tyargs con args numcons = do
  let lensName = mkName $ lowerCase (nameBase con) ++ toChars "L"
  x <- liftQ $ newName $ toChars "x"
  argVars <- liftQ $ mapOnM args $ const $ newName $ toChars "a"
  return
    [ SigD lensName $ 
        ForallT tyargs cx $ app (ConT ''Prism)
          [ ConT ty #@| map (VarT . tyVarBndrName) tyargs
          , tup args
          ]
    , FunD lensName
        [ sclause [] $ app (ConE 'Prism)
            [ LamE [tup $ map VarP argVars] $ ConE con #@| map VarE argVars
            , LamE [VarP x] $ 
                CaseE (VarE x) $ concat
                  [ single $ smatch (ConP con $ map VarP argVars) $ 
                      ConE 'Just #@ tup (map VarE argVars)
                  , if numcons <= toi 1 then [] else single $ smatch WildP $ ConE 'Nothing
                  ]
            ]
        ]
    ]
  where lowerCase = mapHead toLower

makePrisms :: Name -> Q [Dec]
makePrisms name = do
  (cx, ty, tyargs, cs, _) <- maybeZero . (coerceADT *. view tyConIL) *$ liftQ $ reify name
  scs <- mapM (maybeZero . coerceSimpleCon) cs
  concat ^$ mapOnM scs $ \ (cname, args) -> do
    makePrismLogic cx ty tyargs cname args $ length scs

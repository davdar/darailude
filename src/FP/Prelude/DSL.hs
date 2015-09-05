module FP.Prelude.DSL where

import FP.Prelude.Core

-- 3: arrows
infixr 3 ⇨         -- (Arrow a) ⇒ a → a → a

-- 9: application
infixl 9 ⋅         -- (⋅) ∷ (Apply a) ⇒ a → a → a

-- # DSLs

class Arrow a where (⇨) ∷ a → a → a
class Apply a where (⋅) ∷ a → a → a
class Tup a where tup ∷ [a] → a

(⋅|) ∷ (Apply e) ⇒ e → [e] → e
(⋅|) = foldl (⋅)


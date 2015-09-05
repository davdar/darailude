module FP.Prelude.Morphism where

import FP.Prelude.Core

-- 3: arrows
infixr 3 ↝         -- (★ → ★) → (★ → ★) → ★ → ★
infixr 3 ⇝         -- ((★ → ★) → (★ → ★)) → ((★ → ★) → (★ → ★)) → (★ → ★) → ★ → ★

-- 7: composition
infixr 7 ⌾         -- (⌾) ∷ (Category t) ⇒ t b c → t a b → t a c

type m ↝ n = ∀ a. m a → n a
type t ⇝ u = ∀ m. t m ↝ u m
class Isomorphism a b where {isoTo ∷ a → b;isoFrom ∷ b → a}
class Isomorphism2 t u where {isoTo2 ∷ t ↝ u;isoFrom2 ∷ u ↝ t}
class Isomorphism3 v w where {isoTo3 ∷ v ⇝ w;isoFrom3 ∷ w ⇝ v}

instance Isomorphism 𝕊 [ℂ] where {isoTo = chars;isoFrom = 𝕤}

class Category t where {refl ∷ t a a;(⌾) ∷ t b c → t a b → t a c}

instance Category (→) where {refl = id;(⌾) = (∘)}


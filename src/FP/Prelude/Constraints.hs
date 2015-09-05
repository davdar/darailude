module FP.Prelude.Constraints
  ( module FP.Prelude.Constraints
  , module GHC.Exts
  ) where

import GHC.Exts (type Constraint)
import FP.Prelude.Core
import FP.Prelude.Lattice

-- 3: arrows
infixr 3 :⇒:       -- Constraint → Constraint → Constraint

-- 5: products
infixr 5 :∧:       -- (:∧:) ∷ Constraint → Constraint → Constraint

-- 7: composition
infixr 7 :∘:       -- (:∘:) ∷ (Constraint → Constraint) → (Constraint → Constraint) → Constraint → Constraint

data W (c ∷ Constraint) where W ∷ (c) ⇒ W c

with ∷ W c → (c ⇒ a) → a
with W x = x

class Universal a
instance Universal a

class (c1 a,c2 a) ⇒ (c1 :∧: c2) a
instance (c1 a,c2 a) ⇒ (c1 :∧: c2) a

class (t (u a)) ⇒ (t :∘: u) a
instance (t (u a)) ⇒ (t :∘:  u) a

class c1 :⇒: c2 where impl ∷ (c1) ⇒ W c2

-- # Functorial

class Functorial c t where functorial ∷ (c a) ⇒ W (c (t a))

class Bifunctorial c t where bifunctorial ∷ (c a,c b) ⇒ W (c (t a b))

instance Bifunctorial Eq (,) where bifunctorial = W
instance Bifunctorial Ord (,) where bifunctorial = W
instance Bifunctorial POrd (,) where bifunctorial = W

instance (JoinLattice a) ⇒ Functorial JoinLattice ((,) a) where functorial = W
instance (Monoid a) ⇒ Functorial Monoid ((,) a) where functorial = W

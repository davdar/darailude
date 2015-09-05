module FP.Prelude.Lattice where

import FP.Prelude.Core

import qualified Prelude

-- 4: sums
infixr 4 ⊔         -- (⊔) ∷ (Join a) ⇒ a → a → a
infix  4 ⊟         -- (⊟) ∷ (Difference a) ⇒ a → a → a

-- 5: products
infixr 5 ⊓         -- (⊓) ∷ (Meet a) ⇒ a → a → a

-- 6: relations
infix 6 ⊑⊒         -- (⊑⊒) ∷ (POrd a) ⇒ a → a → PartialOrdering
infix 6 ⊑          -- (⊑) ∷ (POrd a) ⇒ a → a → 𝔹
infix 6 ⊒          -- (⊒) ∷ (POrd a) ⇒ a → a → 𝔹
infix 6 ><         -- (><) ∷ (POrd a) ⇒ a → a → 𝔹

-- # Partial Orders

data PartialOrdering = PLT | PEQ | PGT | PUN

class POrd a where (⊑⊒) ∷ a → a → PartialOrdering

fromOrdering ∷ Ordering → PartialOrdering
fromOrdering = \case {LT → PLT;EQ → PEQ;GT → PGT}

(⊑) ∷ (POrd a) ⇒ a → a → 𝔹
x ⊑ y = case x ⊑⊒ y of {PLT → True;PEQ → True;PGT → False;PUN → False}

(⊒) ∷ (POrd a) ⇒ a → a → 𝔹
x ⊒ y = case x ⊑⊒ y of {PLT → False;PEQ → True;PGT → True;PUN → False}

(><) ∷ (POrd a) ⇒ a → a → 𝔹
x >< y = case x ⊑⊒ y of {PLT → False;PEQ → False;PGT → False;PUN → True}

poCompareFromLte ∷ (a → a → 𝔹) → a → a → PartialOrdering
poCompareFromLte lte x y = case (lte x y,lte y x) of
  (True,True) → PEQ
  (True,False) → PLT
  (False,True) → PGT
  (False,False) → PUN

-- # Lattice Kinds

class Bot a where bot ∷ a 
class Join a where (⊔) ∷ a → a → a
class Top a where top ∷ a
class Meet a where (⊓) ∷ a → a → a
class Dual a where dual ∷ a → a
class Difference a where (⊟) ∷ a → a → a   

class (Bot a,Join a) ⇒ JoinLattice a
class (Top a,Meet a) ⇒ MeetLattice a
class (JoinLattice a,MeetLattice a) ⇒ Lattice a
class (Lattice a,Dual a) ⇒ DualLattice a

-- # Integer

instance POrd ℤ where (⊑⊒) = fromOrdering ∘∘ compare
instance Join ℤ where (⊔) = Prelude.max
instance Meet ℤ where (⊓) = Prelude.min

-- # Natural

instance POrd ℕ where (⊑⊒) = fromOrdering ∘∘ compare

instance Bot ℕ where bot = 𝕟 0
instance Join ℕ where (⊔) = Prelude.max
instance Meet ℕ where (⊓) = Prelude.min
instance JoinLattice ℕ

-- # Int

instance POrd 𝕀 where (⊑⊒) = fromOrdering ∘∘ compare

instance Bot 𝕀 where bot = Prelude.minBound
instance Join 𝕀 where (⊔) = Prelude.max
instance Top 𝕀 where top = Prelude.maxBound
instance Meet 𝕀 where (⊓) = Prelude.min
instance JoinLattice 𝕀
instance MeetLattice 𝕀
instance Lattice 𝕀

-- # Double

instance POrd 𝔻 where (⊑⊒) = fromOrdering ∘∘ compare

instance Bot 𝔻 where bot = 𝕤read "Infinity"
instance Join 𝔻 where (⊔) = Prelude.max
instance Top 𝔻 where top = 𝕤read "-Infinity"
instance Meet 𝔻 where (⊓) = Prelude.min
instance JoinLattice 𝔻
instance MeetLattice 𝔻
instance Lattice 𝔻

-- # Bool

instance POrd 𝔹 where (⊑⊒) = fromOrdering ∘∘ (⋚)
instance Bot 𝔹 where bot = False
instance Join 𝔹 where (⊔) = (∨)
instance Top 𝔹 where top = True
instance Meet 𝔹 where (⊓) = (∧)
instance Dual 𝔹 where dual = not
instance JoinLattice 𝔹
instance MeetLattice 𝔹
instance Lattice 𝔹
instance DualLattice 𝔹

-- # Product

instance (POrd a,POrd b) ⇒ POrd (a,b) where (⊑⊒) = poCompareFromLte $ \(a₁,b₁)(a₂,b₂)→(a₁⊑a₂)∧(b₁⊑b₂)
instance (Bot a,Bot b) ⇒ Bot (a,b) where bot = (bot,bot)
instance (Join a,Join b) ⇒ Join (a,b) where (a₁,b₁) ⊔ (a₂,b₂) = (a₁ ⊔ a₂,b₁ ⊔ b₂)
instance (Top a,Top b) ⇒ Top (a,b) where top = (top,top)
instance (Meet a,Meet b) ⇒ Meet (a,b) where (a₁,b₁) ⊓ (a₂,b₂) = (a₁ ⊓ a₂,b₁ ⊓ b₂)
instance (Dual a,Dual b) ⇒ Dual (a,b) where dual (a,b) = (dual a,dual b)
instance (Difference a,Difference b) ⇒ Difference (a,b) where (a₁,b₁) ⊟ (a₂,b₂) = (a₁ ⊟ a₂,b₁ ⊟ b₂)
instance (JoinLattice a,JoinLattice b) ⇒ JoinLattice (a,b)
instance (MeetLattice a,MeetLattice b) ⇒ MeetLattice (a,b)
instance (Lattice a,Lattice b) ⇒ Lattice (a,b)
instance (DualLattice a,DualLattice b) ⇒ DualLattice (a,b)

-- # Function

instance (Bot b) ⇒ Bot (a → b) where bot = const bot
instance (Join b) ⇒ Join (a → b) where (f ⊔ g) x = f x ⊔ g x
instance (Top b) ⇒ Top (a → b) where top = const top
instance (Meet b) ⇒ Meet (a → b) where (f ⊓ g) x = f x ⊓ g x
instance (Dual b) ⇒ Dual (a → b) where dual f = dual ∘ f
instance (Difference b) ⇒ Difference (a → b) where (f ⊟ g) x = f x ⊟ g x

instance (JoinLattice b) ⇒ JoinLattice (a → b)
instance (MeetLattice b) ⇒ MeetLattice (a → b)
instance (Lattice b) ⇒ Lattice (a → b)
instance (DualLattice b) ⇒ DualLattice (a → b)

-- # Set

instance POrd (𝒫 a) where (⊑⊒) = poCompareFromLte isSubsetOf
instance Bot (𝒫 a) where bot = EmptySet
instance Join (𝒫 a) where (⊔) = unionSet
instance Meet (𝒫 a) where (⊓) = intersectionSet
instance Difference (𝒫 a) where (⊟) = differenceSet
instance JoinLattice (𝒫 a)

-- # Dict

instance (POrd v) ⇒ POrd (k ⇰ v) where (⊑⊒) = poCompareFromLte $ isSubdictOfBy (⊑)
instance Bot (k ⇰ v) where bot = EmptyDict
instance (Join v) ⇒ Join (k ⇰ v) where (⊔) = unionWithDict (⊔)
instance (Meet v) ⇒ Meet (k ⇰ v) where (⊓) = intersectionWithDict (⊓)
instance (Join v) ⇒ JoinLattice (k ⇰ v)

-- # Operations

joins ∷ (JoinLattice a,ToStream a t) ⇒ t → a 
joins = iter (⊔) bot

meets ∷ (MeetLattice a,ToStream a t) ⇒ t → a 
meets = iter (⊓) top

-- # AddBot

data AddBot a = Bot | AddBot a
  deriving (Eq,Ord)

instance Functor AddBot where
  map _ Bot = Bot
  map f (AddBot x) = AddBot $ f x

instance Bot (AddBot a) where bot = Bot
instance (Join a) ⇒ Join (AddBot a) where
  Bot ⊔ x = x
  x ⊔ Bot = x
  AddBot x ⊔ AddBot y = AddBot $ x ⊔ y
instance (Top a) ⇒ Top (AddBot a) where top = AddBot top
instance (Meet a) ⇒ Meet (AddBot a) where
  Bot ⊓ _ = Bot
  _ ⊓ Bot = Bot
  AddBot x ⊓ AddBot y = AddBot $ x ⊓ y
instance (Join a) ⇒ JoinLattice (AddBot a)
instance (MeetLattice a) ⇒ MeetLattice (AddBot a)
instance (Join a,MeetLattice a) ⇒ Lattice (AddBot a)

elimAddBot ∷ b → (a → b) → AddBot a → b
elimAddBot b _ Bot = b
elimAddBot _ f (AddBot x) = f x

-- # AddTop

data AddTop a = Top | AddTop a
  deriving (Eq,Ord)

instance Functor AddTop where
  map _ Top = Top
  map f (AddTop x) = AddTop $ f x

instance (Bot a) ⇒ Bot (AddTop a) where bot = AddTop bot
instance (Join a) ⇒ Join (AddTop a) where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  AddTop x ⊔ AddTop y = AddTop $ x ⊔ y
instance Top (AddTop a) where top = Top
instance (Meet a) ⇒ Meet (AddTop a) where
  Top ⊓ x = x
  x ⊓ Top = x
  AddTop x ⊓ AddTop y = AddTop $ x ⊓ y
instance (JoinLattice a) ⇒ JoinLattice (AddTop a)
instance (Meet a) ⇒ MeetLattice (AddTop a)
instance (JoinLattice a,Meet a) ⇒ Lattice (AddTop a)

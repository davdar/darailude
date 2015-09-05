module FP.Prelude.Core
  ( module FP.Prelude.Core

  , module Control.Exception
  , module Data.Char
  , module Data.Coerce
  , module Data.List
  , module Language.Haskell.TH
  , module Numeric.Natural
  , module Prelude
  , module System.IO.Unsafe
  ) where

import Control.Exception (assert)
import Data.Char (isSpace,isAlphaNum,isLetter,isDigit,isSpace)
import Data.Coerce (Coercible,coerce)
import Data.List (sort,sortBy)
import Language.Haskell.TH (Q)
import Prelude 
  ( Int,Integer,Double,Char
  , Eq(..),Ord(..),Ordering(..)
  , Show(..),Read(..),read
  , Bool(..),otherwise
  , Maybe(..)
  , Either(..)
  , ($),seq
  , IO
  )
import Numeric.Natural (Natural)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import qualified Prelude             as Prelude

-- 0: low/right associativity application
{- infixr 0 $ -}   -- ($) ∷ (a → b) → a → b
infixr 0 ♯$        -- / strict application
                   -- \ (♯$) ∷ (a → b) → a → b
infixr 0 ^$        -- / alias for `map`
                   -- \ (^$) ∷ (Functor t) ⇒ (a → b) → t a → t b
infixr 0 *$        -- / alias for `extend`
                   -- \ (*$) ∷ (Monad m) ⇒ (a → m b) → m a → m b
infixr 0 ^*$       -- / alias for `mapM`
                   -- \ (^*$) ∷ (FunctorM t,Monad m) ⇒ (a → m b) → t a → m (t b)

-- 1: semicolons
infixr 1 ≫=        -- (≫=) ∷ (Monad m) ⇒ m a → (a → m b) → m b
infixr 1 ≫         -- (≫) ∷ (Monad m) ⇒ m a → m b → m b
infixr 1 ≫=*       -- (≫=*) ∷ 𝒫 a → (a → 𝒫 b) → 𝒫 b
infixr 1 >>=       -- alias for (≫=)
infixr 1 >>        -- alias for (≫)

-- 3: arrows
infixr 3 ⇰         -- ★ → ★ → ★
infixr 3 ↦         -- (↦) ∷ (Ord k) ⇒ k → v → k ⇰ v

-- 4: sums
infixr 4 ⨄         -- ★ → ★
infixr 4 ∨         -- (∨) ∷ 𝔹 → 𝔹 → 𝔹
infixr 4 ⧺         -- (⧺) ∷ (Monoid a) ⇒ a → a → a
infixr 4 +         -- (+) ∷ (Additive a) ⇒ a → a → a
infix  4 -         -- (-) ∷ (Subtractive a) ⇒ a → a → a

-- 5: products
infixr 5 ∧         -- (∧) ∷ 𝔹 → 𝔹 → 𝔹
infixr 5 ×         -- (×) ∷ (Multiplicative a) ⇒ a → a → a
infix  5 /         -- (/) ∷ (Divisible a) ⇒ a → a → a
infix  5 ⌿         -- (⌿) ∷ (TruncateDivisible a) ⇒ a → a → a
infixr 5 <×>       -- (<×>) ∷ (Monad m) ⇒ m a → m a → m a

-- 6: relations
infix 6 ≟          -- / alias for `(==)`
                   -- \ (≟) ∷ (Eq a) ⇒ a → a → 𝔹
infix 6 ⋚          -- / alias for `compare`
                   -- \ (⋚) ∷ (Ord a) ⇒ a → a → Ordering
infix 6 ≤          -- (≤) ∷ (Ord a) ⇒ a → a → 𝔹
infix 6 ≥          -- (≥) ∷ (Ord a) ⇒ a → a → 𝔹
infix 6 ∈          -- (∈) ∷ a → 𝒫 a → 𝔹

-- 7: composition
infixr 7 ∘         -- (∘) ∷ (b → c) → (a → b) → a → c
infixr 7 ∘∘        -- (∘∘) ∷ (c → c) → (a → b → c) → a → b → d
infixr 7 ^∘        -- (^∘) ∷ (Functor t) ⇒ (b → c) → (a → t b) → a → t c
infixr 7 *∘        -- (*∘) ∷ (Monad m) ⇒ (b → m c) → (a → m b) → a → m c
infixr 7 ^*∘       -- (^*∘) ∷ (FunctorM t,Monad m) ⇒ (b → m c) → (a → m b) → t a → m (t c) 

-- 8: indexing structures and exponentials
infixl 8 #         -- (#) ∷ k ⇰ v → k → Maybe v
infixl 8 #!        -- (#!) ∷ k ⇰ v → k → v
infixl 8 ^         -- (^) ∷ (Exponential a) ⇒ a → a → a

-- 9: application
infixl 9 ♯⋅        -- (♯⋅) ∷ (a → b) → a → b
infixl 9 ^⋅        -- (^⋅) ∷ (Functor t) ⇒ (a → b) → t a → t b 
infixl 9 *⋅        -- (*⋅) ∷ (Monad m) ⇒ (a → m b) → (m a → m b)
infixl 9 ^*⋅       -- (^*⋅) ∷ (FunctorM t,Monad m) ⇒ (a → m b) → t a → m (t b) 

--------------
-- Universe --
--------------

type ℤ = Integer
type ℕ = Natural
type 𝕀 = Int
type 𝔻 = Double
type ℂ = Char
type 𝕊 = Text.Text
-- - ()
type 𝔹 = Bool
type (⨄) = Either
-- - (,)
-- - (→)
data Stream a where Stream ∷ ∀ s a. s → (s → Maybe (a,s)) → Stream a
-- - []
data 𝒫 a where {EmptySet ∷ 𝒫 a;Set ∷ (Ord a) ⇒ Set.Set a → 𝒫 a}
data k ⇰ v where {EmptyDict ∷ k ⇰ v;Dict ∷ (Ord k) ⇒ Map.Map k v → k ⇰ v}

-------------
-- Classes --
-------------

-- # Equality and Order

(≟) ∷ (Eq a) ⇒ a → a → 𝔹
(≟) = (==)

(⋚) ∷ (Ord a) ⇒ a → a → Ordering
(⋚) = compare

(≤) ∷ (Ord a) ⇒ a → a → 𝔹
x ≤ y = case x ⋚ y of {LT → True;EQ → True;GT → False}

(≥) ∷ (Ord a) ⇒ a → a → 𝔹
x ≥ y = case x ⋚ y of {LT → False;EQ → True;GT → True}

-- Arithmatic

class Peano a where {suc ∷ a → a;zero ∷ a}
class (Peano a) ⇒ Additive a where (+) ∷ a → a → a
class (Additive a) ⇒ Subtractive a where (-) ∷ a → a → a
class (Additive a) ⇒ Multiplicative a where {one ∷ a;(×) ∷ a → a → a}
class (Multiplicative a) ⇒ Divisible a where (/) ∷ a → a → a
class (Multiplicative a) ⇒ TruncateDivisible a where (⌿) ∷ a → a → a
class (Multiplicative a) ⇒ Exponential a where (^) ∷ a → a → a

negate ∷ (Subtractive a) ⇒ a → a 
negate x = zero - x

inverse ∷ (Divisible a) ⇒ a → a 
inverse x = one / x

-- # Monoid

class Monoid a where {null ∷ a;(⧺) ∷ a → a → a}

-- # Functor

class Functor (t ∷ ★ → ★) where map ∷ (a → b) → (t a → t b)

mapOn ∷ (Functor t) ⇒ t a → (a → b) → t b 
mapOn = flip map

(^⋅) ∷ (Functor t) ⇒ (a → b) → t a → t b 
(^⋅) = map

(^$) ∷ (Functor t) ⇒ (a → b) → t a → t b 
(^$) = map

(^∘) ∷ (Functor t) ⇒ (b → c) → (a → t b) → a → t c 
g ^∘ f = map g ∘ f

-- # Monad

class (Functor m) ⇒ Monad (m ∷ ★ → ★) where {return ∷ a → m a;(≫=) ∷ m a → (a → m b) → m b}

(*⋅) ∷ (Monad m) ⇒ (a → m b) → (m a → m b)
(*⋅) = extend

(*$) ∷ (Monad m) ⇒ (a → m b) → (m a → m b)
(*$) = extend

(*∘) ∷ (Monad m) ⇒ (b → m c) → (a → m b) → (a → m c)
(g *∘ f) x = g *$ f x

kleisli ∷ (Monad m) ⇒ (a → b) → (a → m b)
kleisli = (∘) return

extend ∷ (Monad m) ⇒ (a → m b) → (m a → m b)
extend = flip (≫=)

(≫) ∷ (Monad m) ⇒ m a → m b → m b
aM ≫ bM = aM ≫= const bM

void ∷ (Monad m) ⇒ m a → m ()
void = mmap (const ())

mjoin ∷ (Monad m) ⇒ m (m a) → m a
mjoin = extend id

mmap ∷ (Monad m) ⇒ (a → b) → m a → m b
mmap f aM = return ∘ f *$ aM

(<×>) ∷ (Monad m) ⇒ m a → m b → m (a,b)
aM <×> bM = do
  a ← aM
  b ← bM
  return (a,b)

mapply ∷ (Monad m) ⇒ m (a → b) → m a → m b
mapply fM aM = do
  f ← fM
  a ← aM
  return $ f a

when ∷ (Monad m) ⇒ 𝔹 → m () → m ()
when True = id
when False = const $ return ()

whenM ∷ (Monad m) ⇒ m 𝔹 → m () → m ()
whenM bM aM = do
  b ← bM
  when b aM

whenMaybe ∷ (Monad m) ⇒ Maybe a → (a → m ()) → m ()
whenMaybe Nothing _ = return ()
whenMaybe (Just x) f = f x

whenMaybeM ∷ (Monad m) ⇒ m (Maybe a) → (a → m ()) → m ()
whenMaybeM xMM f = do
  xM ← xMM
  whenMaybe xM f

returnMaybe ∷ (Monad m) ⇒ m a → Maybe a → m a
returnMaybe xM Nothing = xM
returnMaybe _ (Just x) = return x

-- # FunctorM

class FunctorM (t ∷ ★ → ★) where mapM ∷ (Monad m) ⇒ (a → m b) → t a → m (t b)

mapMOn ∷ (FunctorM t,Monad m) ⇒ t a → (a → m b) → m (t b) 
mapMOn = flip mapM

sequence ∷ (FunctorM t,Monad m) ⇒ t (m a) → m (t a) 
sequence = mapM id

(^*⋅) ∷ (FunctorM t,Monad m) ⇒ (a → m b) → t a → m (t b) 
(^*⋅) = mapM

(^*$) ∷ (FunctorM t,Monad m) ⇒ (a → m b) → t a → m (t b) 
(^*$) = mapM

(^*∘) ∷ (FunctorM t,Monad m) ⇒ (b → m c) → (a → m b) → t a → m (t c) 
(g ^*∘ f) aT = mapM g *$ f ^*$ aT

-- # Conversion

class ToStream a t | t → a where stream ∷ t → Stream a

class ToInteger a where 𝕫 ∷ a → ℤ
class ToNatural a where 𝕟 ∷ a → ℕ
class ToInt a where 𝕚 ∷ a → 𝕀
class ToDouble a where 𝕕 ∷ a → 𝔻

-----------
-- Types --
-----------

-- # Integer (ℤ)

instance Peano ℤ where {zero = 0;suc = Prelude.succ}
instance Additive ℤ where (+) = (Prelude.+)
instance Subtractive ℤ where (-) = (Prelude.-)
instance Multiplicative ℤ where {one = 1;(×) = (Prelude.*)}
instance TruncateDivisible ℤ where (⌿) = Prelude.div

instance ToInteger ℤ where 𝕫 = id
instance ToNatural ℤ where 𝕟 = Prelude.fromIntegral
instance ToInt ℤ where 𝕚 = Prelude.fromIntegral
instance ToDouble ℤ where 𝕕 = Prelude.fromIntegral

-- # Natural (ℕ)

instance Peano ℕ where {zero = 𝕟 0;suc = Prelude.succ}
instance Additive ℕ where (+) = (Prelude.+)
instance Subtractive ℕ where (-) = (Prelude.-)
instance Multiplicative ℕ where {one = 𝕟 1;(×) = (Prelude.*)}
instance TruncateDivisible ℕ where (⌿) = Prelude.div

instance ToInteger ℕ where 𝕫 = Prelude.fromIntegral
instance ToNatural ℕ where 𝕟 = id
instance ToInt ℕ where 𝕚 = Prelude.fromIntegral
instance ToDouble ℕ where 𝕕 = Prelude.fromIntegral

-- # Int (𝕀)

instance Peano 𝕀 where {zero = 𝕚 0;suc = Prelude.succ}
instance Additive 𝕀 where (+) = (Prelude.+)
instance Subtractive 𝕀 where (-) = (Prelude.-)
instance Multiplicative 𝕀 where {one = 𝕚 1;(×) = (Prelude.*)}
instance TruncateDivisible 𝕀 where (⌿) = Prelude.div

instance ToInteger 𝕀 where 𝕫 = Prelude.fromIntegral
instance ToNatural 𝕀 where 𝕟 = Prelude.fromIntegral
instance ToInt 𝕀 where 𝕚 = id
instance ToDouble 𝕀 where 𝕕 = Prelude.fromIntegral

-- # Double (𝔻)

instance Peano 𝔻 where {zero = 𝕕 0;suc = Prelude.succ}
instance Additive 𝔻 where (+) = (Prelude.+)
instance Subtractive 𝔻 where (-) = (Prelude.-)
instance Multiplicative 𝔻 where {one = 𝕕 1;(×) = (Prelude.*)}
instance Divisible 𝔻 where (/) = (Prelude./)

instance ToDouble 𝔻 where 𝕕 = id

-- # Char (ℂ)

instance ToStream ℂ ℂ where stream = ssingle

-- # String (𝕊)

instance Monoid 𝕊 where {null = Text.empty;(⧺) = Text.append}
instance ToStream ℂ 𝕊 where stream = stream ∘ Text.unpack

chars ∷ 𝕊 → [ℂ]
chars = Text.unpack

𝕤show ∷ (Show a) ⇒ a → 𝕊
𝕤show = 𝕤 ∘ show

𝕤read ∷ (Read a) ⇒ 𝕊 → a
𝕤read = read ∘ chars

-- # Unit ()

-- # Bool (𝔹)

not ∷ 𝔹 → 𝔹
not True  = False
not False = True

(∧) ∷ 𝔹 → 𝔹 → 𝔹
True  ∧ x = x
False ∧ _ = False

(∨) ∷ 𝔹 → 𝔹 → 𝔹
True  ∨ _ = True
False ∨ x = x

fif ∷ 𝔹 → a → a → a
fif True x _ = x
fif False _ y = y

-- # Sum

-- no Monoid
instance Functor ((⨄) a) where map = mmap
instance Monad ((⨄) a) where {return = Right;abM ≫= k = elimSum Left k abM}
instance FunctorM ((⨄) a) where 
  mapM _ (Left x) = return $ Left x
  mapM f (Right y) = Right ^$ f y

elimSum ∷ (a → c) → (b → c) → a ⨄ b → c
elimSum f g aorb = case aorb of
  Left x → f x
  Right y → g y

mapSum ∷ (a → a') → (b → b') → a ⨄ b → a' ⨄ b'
mapSum f _ (Left x) = Left $ f x
mapSum _ g (Right y) = Right $ g y

mapLeft ∷ (a → c) → a ⨄ b → c ⨄ b
mapLeft f = mapSum f id

mapRight ∷ (b → c) → a ⨄ b → a ⨄ c
mapRight g = mapSum id g

-- # Product

instance (Monoid a,Monoid b) ⇒ Monoid (a,b) where{null = (null,null);(a₁,b₁) ⧺ (a₂,b₂) = (a₁ ⧺ a₂,b₁ ⧺ b₂)}
instance Functor ((,) a) where map f (x,y) = (x,f y)
instance (Monoid a) ⇒ Monad ((,) a) where {return = (null,); (x,y) ≫= k = let (x',z) = k y in (x ⧺ x',z)}
instance FunctorM ((,) a) where mapM f (x,y) = (x,) ^$ f y

fst ∷ (a,b) → a
fst (x,_) = x

snd ∷ (a,b) → b
snd (_,y) = y

swap ∷ (a,b) → (b,a)
swap (x,y) = (y,x)

mapPair ∷ (a → a') → (b → b') → (a,b) → (a',b')
mapPair f g (a,b) = (f a,g b)

mapFst ∷ (a → a') → (a,b) → (a',b)
mapFst f = mapPair f id

mapSnd ∷ (b → b') → (a,b) → (a,b')
mapSnd f = mapPair id f

-- # Function

instance (Monoid b) ⇒ Monoid (a → b) where {null = const null;(f ⧺ g) x = f x ⧺ g x}
instance Functor ((→) a) where map = (∘)
instance Monad ((→) a) where {return = const;(f ≫= k) x = k (f x) x }
-- no FunctorM

id ∷ a → a
id x = x

(♯$) ∷ (a → b) → a → b
f ♯$ x = x `seq` f x

(♯⋅) ∷ (a → b) → a → b
f ♯⋅ x = x `seq` f x

(∘) ∷ (b → c) → (a → b) → (a → c)
(g ∘ f) x = g (f x)

(∘∘) ∷ (c → d) → (a → b → c) → (a → b → d)
(∘∘) = (∘) ∘ (∘)

const ∷ b → (a → b)
const x = \ _ → x

flip ∷ (a → b → c) → (b → a → c)
flip f y x = f x y

curry ∷ ((a,b) → c) → a → b → c
curry f x y = f (x,y)

uncurry ∷ (a → b → c) → (a,b) → c
uncurry f (x,y) = f x y

rotateR ∷ (a → b → c → d) → (c → a → b → d)
rotateR f c a b = f a b c

rotateL ∷ (a → b → c → d) → (b → c → a → d)
rotateL f b c a = f a b c

mirror ∷ (a → b → c → d) → (c → b → a → d)
mirror f c b a = f a b c

on ∷ (b → b → c) → (a → b) → (a → a → c)
on p f x y = p (f x) (f y)

-- # Maybe

-- no Monoid
instance Functor Maybe where map = mmap
instance Monad Maybe where
  return = Just
  Nothing ≫= _ = Nothing
  Just x ≫= k = k x
instance FunctorM Maybe where 
  mapM _ Nothing = return Nothing
  mapM f (Just x) = Just ^$ f x

elimMaybe ∷ b → (a → b) → Maybe a → b
elimMaybe y f aM = case aM of
  Nothing → y
  Just a → f a

elimMaybeOn ∷ Maybe a → b → (a → b) → b
elimMaybeOn = rotateR elimMaybe

ifNothing ∷ a → Maybe a → a
ifNothing x = elimMaybe x id

-- # Stream

instance (Eq a) ⇒ Eq (Stream a) where (==) = (≟) `on` list
instance (Ord a) ⇒ Ord (Stream a) where compare = compare `on` list
instance (Show a) ⇒ Show (Stream a) where show = chars ∘ (⧺) "stream " ∘ 𝕤show ∘ list

instance Monoid (Stream a) where
  null = Stream () $ \ () → Nothing
  Stream s₁₀ f₁ ⧺ Stream s₂₀ f₂ = Stream (Left s₁₀) $ \ s →
    let doRight s₂ = case f₂ s₂ of
          Nothing → Nothing
          Just (x,s₂') → Just (x,Right s₂')
    in case s of
      Left s₁ → case f₁ s₁ of
        Nothing → doRight s₂₀
        Just (x,s₁') → Just (x,Left s₁')
      Right s₂ → doRight s₂
instance Functor Stream where map g (Stream s₀ f) = Stream s₀ $ map (mapFst g) ∘ f
instance Monad Stream where {return = ssingle;xs ≫= k = concat $ map k xs}
instance FunctorM Stream where mapM f = stream ^∘ mapM f ∘ list
instance ToStream a (Stream a) where stream = id

ssingle ∷ a → Stream a
ssingle x = Stream (Just x) $ map (,Nothing)

suncons ∷ Stream a → Maybe (a,Stream a)
suncons (Stream s f) = case f s of
  Nothing → Nothing
  Just (x,s') → Just (x,Stream s' f)

isEmpty ∷ (ToStream a t) ⇒ t → 𝔹
isEmpty (stream → Stream s f) = case f s of
  Nothing → True
  Just _ → False

foldlk ∷ (ToStream a t) ⇒ (b → a → (b → b) → b) → b → t → b
foldlk f i₀ (stream → Stream s₀ g) = loop i₀ s₀
  where
    loop i s = case g s of
      Nothing → i
      Just (x,s') → f i x $ \ i' → loop i' s'

foldlkOn ∷ [a] → b → (b → a → (b → b) → b) → b
foldlkOn = mirror foldlk

foldl ∷ (ToStream a t) ⇒ (b → a → b) → b → t → b
foldl f = foldlk $ \ i x k → k ♯$ f i x

foldlOn ∷ [a] → b → (b → a → b) → b
foldlOn = mirror foldl

foldr ∷ (ToStream a t) ⇒ (a → b → b) → b → t → b
foldr f = foldlk $ \ i x k → f x $ k i

foldrOn ∷ [a] → b → (a → b → b) → b
foldrOn = mirror foldr

iter ∷ (ToStream a t) ⇒ (a → b → b) → b → t → b
iter = foldl ∘ flip

iterOn ∷ (ToStream a t) ⇒ t → b → (a → b → b) → b
iterOn = mirror iter

sum ∷ (ToStream a t,Additive a) ⇒ t → a
sum = iter (+) zero

product ∷ (ToStream a t,Multiplicative a) ⇒ t → a
product = iter (×) one

concat ∷ (ToStream a t,Monoid a) ⇒ t → a
concat = foldr (⧺) null

mfoldl ∷ (Monad m,ToStream a t) ⇒ (b → a → m b) → b → t → m b
mfoldl f = foldl (\ bM a → bM ≫= \ b → f b a) ∘ return

miter ∷ (Monad m,ToStream a t) ⇒ (a → b → m b) → b → t → m b
miter f = iter (\ a bM → bM ≫= f a) ∘ return

mfoldr ∷ (Monad m,ToStream a t) ⇒ (a → b → m b) → b → t → m b
mfoldr f = foldr (extend ∘ f) ∘ return

foreach ∷ (Monad m,ToStream a t) ⇒ (a → m ()) → t → m ()
foreach f = foldl (\ m a → m ≫ f a) $ return ()

foreachOn ∷ (Monad m,ToStream a t) ⇒ t → (a → m ()) → m () 
foreachOn = flip foreach

exec ∷ (Monad m,ToStream (m ()) t) ⇒ t → m () 
exec = foreach id

𝕤 ∷ (ToStream ℂ t) ⇒ t → 𝕊
𝕤 = Text.pack ∘ list

list ∷ (ToStream a t) ⇒ t → [a]
list = foldr (:) [] ∘ stream

set ∷ (ToStream a t,Ord a) ⇒ t → 𝒫 a
set = iter insert emptySet ∘ stream

dict ∷ (ToStream (k,v) t,Ord k) ⇒ t → k ⇰ v
dict = iter (uncurry insertDict) emptyDict ∘ stream

-- # List

instance Monoid [a] where
  null = []
  [] ⧺ ys = ys
  x:xs ⧺ ys = x:(xs ⧺ ys)
instance Functor [] where
  map _ [] = []
  map f (x:xs) = f x:map f xs
instance Monad [] where
  return = (:[])
  [] ≫= _ = []
  x:xs ≫= k = k x ⧺ (xs ≫= k)
instance FunctorM [] where
  mapM _ [] = return []
  mapM f (x:xs) = do
    y ← f x
    ys ← mapM f xs
    return $ y:ys
instance ToStream a [a] where
  stream xs = Stream xs uncons

single ∷ a → [a]
single = (:[])

cons ∷ a → [a] → [a]
cons = (:)

uncons ∷ [a] → Maybe (a,[a])
uncons [] = Nothing
uncons (x:xs) = Just (x,xs)

zip ∷ [a] → [b] → Maybe [(a,b)]
zip [] [] = Just []
zip (x:xs) (y:ys) = do
  xys ← zip xs ys
  return $ (x,y):xys
zip _ _ = Nothing

unzip ∷ [(a,b)] → ([a],[b])
unzip xys = (map fst xys,map snd xys)

partition ∷ [a ⨄ b] → ([a],[b])
partition [] = ([],[])
partition (Left x:xys) = let (xs,ys) = partition xys in (x:xs,ys)
partition (Right y:xys) = let (xs,ys) = partition xys in (xs,y:ys)

reverse ∷ [a] → [a]
reverse = foldl (flip (:)) []

-- # Set

instance Eq (𝒫 a) where (==) = elimPrim21Set True (Set.null) (≟)
instance Ord (𝒫 a) where compare = elimPrim22Set EQ (\ s → compare Set.empty s) (\ s → compare s Set.empty) compare
instance (Show a) ⇒ Show (𝒫 a) where show = chars ∘ (⧺) "set " ∘ 𝕤show ∘ list ∘ streamFromSet
instance Monoid (𝒫 a) where {null = emptySet;(⧺) = unionSet}
instance ToStream a (𝒫 a) where stream = elimPrimSet null $ stream ∘ Set.toList

streamFromSet ∷ 𝒫 a → Stream a
streamFromSet = elimPrimSet null $ stream ∘ Set.toList

elimPrimSet ∷ b → ((Ord a) ⇒ Set.Set a → b) → 𝒫 a → b
elimPrimSet i f = \case
  EmptySet → i
  Set x → f x

elimPrimOnSet ∷ 𝒫 a → b → ((Ord a) ⇒ Set.Set a → b) → b
elimPrimOnSet s i f = elimPrimSet i f s

elimPrim22Set ∷
     b
  → ((Ord a) ⇒ Set.Set a → b)
  → ((Ord a) ⇒ Set.Set a → b)
  → ((Ord a) ⇒ Set.Set a → Set.Set a → b)
  → 𝒫 a → 𝒫 a → b
elimPrim22Set i f1 f2 ff s1 s2 =
  elimPrimOnSet s1 (elimPrimOnSet s2 i f1) $ \ p1 →
    elimPrimOnSet s2 (f2 p1) $ \ p2 →
      ff p1 p2

elimPrim21Set ∷
     b
  → ((Ord a) ⇒ Set.Set a → b)
  → ((Ord a) ⇒ Set.Set a → Set.Set a → b)
  → 𝒫 a
  → 𝒫 a
  → b
elimPrim21Set i f = elimPrim22Set i f f

toPrimSet ∷ 𝒫 a → Set.Set a
toPrimSet = elimPrimSet Set.empty id

learnSet ∷ 𝒫 a → b → ((Ord a) ⇒ b) → b
learnSet s i f = elimPrimOnSet s i $ const f

emptySet ∷ 𝒫 a
emptySet = EmptySet

insert ∷ (Ord a) ⇒ a → 𝒫 a → 𝒫 a
insert x = elimPrimSet (Set $ Set.singleton x) $ Set ∘ Set.insert x

unionSet ∷ 𝒫 a → 𝒫 a → 𝒫 a
unionSet = elimPrim21Set EmptySet Set $ Set ∘∘ Set.union

intersectionSet ∷ 𝒫 a → 𝒫 a → 𝒫 a
intersectionSet = elimPrim21Set EmptySet (const EmptySet) $ Set ∘∘ Set.intersection

differenceSet ∷ 𝒫 a → 𝒫 a → 𝒫 a
differenceSet = elimPrim22Set EmptySet (const EmptySet) Set $ Set ∘∘ (Set.\\)

isSubsetOf ∷ 𝒫 a → 𝒫 a → 𝔹
isSubsetOf = elimPrim22Set True (const True) (const False) $ Set.isSubsetOf

(∈) ∷ a → 𝒫 a → 𝔹
(∈) x = elimPrimSet False $ Set.member x

elem ∷ a → 𝒫 a → Bool
elem = (∈)

elemOf ∷ 𝒫 a → a → 𝔹
elemOf = flip elem

remove ∷ 𝒫 a → Maybe (a,𝒫 a)
remove = elimPrimSet Nothing $ map (mapSnd Set) ∘ Set.minView

mapSet ∷ (Ord b) ⇒ (a → b) → 𝒫 a → 𝒫 b
mapSet f = elimPrimSet EmptySet $ Set ∘ Set.map f

mapSetOn ∷ (Ord b) ⇒ 𝒫 a → (a → b) → 𝒫 b
mapSetOn = flip mapSet

extendSet ∷ (a → 𝒫 b) → 𝒫 a → 𝒫 b
extendSet f = iter unionSet emptySet ∘ map f ∘ stream

(≫=*) ∷ 𝒫 a → (a → 𝒫 b) → 𝒫 b
(≫=*) = flip extendSet

-- # Dict

instance (Eq v) ⇒ Eq (k ⇰ v) where (==) = elimPrim21Dict True (Map.null) (≟)
instance (Ord v) ⇒ Ord (k ⇰ v) where compare = elimPrim22Dict EQ (\ m → Map.empty ⋚ m) (\ m → m ⋚ Map.empty) (⋚)
instance (Show k,Show v) ⇒ Show (k ⇰ v) where show = chars ∘ (⧺) "dict " ∘ 𝕤show ∘ list ∘ streamFromDict
instance Functor ((⇰) k) where map f = elimPrimDict EmptyDict $ Dict ∘ Map.map f
instance (Monoid v) ⇒ Monoid (k ⇰ v) where {null = EmptyDict;(⧺) = unionWithDict (⧺)}
instance ToStream (k,v) (k ⇰ v) where stream = elimPrimDict null $ stream ∘ Map.toList

streamFromDict ∷ k ⇰ v → Stream (k,v)
streamFromDict = elimPrimDict null $ stream ∘ Map.toList

elimPrimDict ∷ b → ((Ord k) ⇒ Map.Map k v → b) → k ⇰ v → b
elimPrimDict i f = \case {EmptyDict → i;Dict p → f p}

elimPrimOnDict ∷ k ⇰ v → b → ((Ord k) ⇒ Map.Map k v → b) → b
elimPrimOnDict m i f = elimPrimDict i f m

elimPrim22Dict ∷
     b
  → ((Ord k) ⇒ Map.Map k v → b)
  → ((Ord k) ⇒ Map.Map k v → b)
  → ((Ord k) ⇒ Map.Map k v → Map.Map k v → b)
  → k ⇰ v → k ⇰ v → b
elimPrim22Dict i f1 f2 ff s1 s2 =
  elimPrimOnDict s1 (elimPrimOnDict s2 i f1) $ \ p1 →
    elimPrimOnDict s2 (f2 p1) $ \ p2 →
      ff p1 p2

elimPrim21Dict ∷
     b
  → ((Ord k) ⇒ Map.Map k v → b)
  → ((Ord k) ⇒ Map.Map k v → Map.Map k v → b)
  → k ⇰ v
  → k ⇰ v
  → b
elimPrim21Dict i f = elimPrim22Dict i f f

toPrimDict ∷ k ⇰ v → Map.Map k v
toPrimDict = elimPrimDict Map.empty id

learnDict ∷ k ⇰ v → b → ((Ord k) ⇒ b) → b
learnDict m i f = elimPrimOnDict m i $ const f

emptyDict ∷ k ⇰ v
emptyDict = EmptyDict

keys ∷ k ⇰ v → 𝒫 k
keys = elimPrimDict emptySet $ Set ∘ Map.keysSet

values ∷ k ⇰ v → [v]
values = elimPrimDict null $ Map.elems

type Old a = a
type New a = a

insertWithDict ∷ (Ord k) ⇒ (Old v → New v → v) → k → v → k ⇰ v → k ⇰ v
insertWithDict f k v = elimPrimDict (Dict $ Map.singleton k v) $ Dict ∘ Map.insertWith (flip f) k v

insertDict ∷ (Ord k) ⇒ k → v → k ⇰ v → k ⇰ v
insertDict = insertWithDict $ const id

(↦) ∷ (Ord k) ⇒ k → v → k ⇰ v
k ↦ v = Dict $ Map.singleton k v

lookup ∷ k → k ⇰ v → Maybe v
lookup k = elimPrimDict Nothing $ Map.lookup k

index ∷ k ⇰ v → k → Maybe v
index = flip lookup

(#) ∷ k ⇰ v → k → Maybe v
(#) = index

(#!) ∷ k ⇰ v → k → v
m #! k = ifNothing (error "unsafe (#!)") $ m # k

removeDict ∷ k ⇰ v → Maybe ((k,v),k ⇰ v)
removeDict = elimPrimDict Nothing $ map (mapSnd Dict) ∘ Map.minViewWithKey

unionWithDict ∷ (Old v → New v → v) → k ⇰ v → k ⇰ v → k ⇰ v
unionWithDict f = elimPrim21Dict EmptyDict Dict $ Dict ∘∘ Map.unionWith (flip f)

unionWithDictOn ∷ k ⇰ v → k ⇰ v → (Old v → New v → v) → k ⇰ v
unionWithDictOn d₁ d₂ f = unionWithDict f d₁ d₂

isSubdictOfBy ∷ (v → v → 𝔹) → k ⇰ v → k ⇰ v → 𝔹
isSubdictOfBy lte = elimPrim22Dict True (const True) (const False) $ Map.isSubmapOfBy lte

intersectionWithDict ∷ (Old v → New v → v) → k ⇰ v → k ⇰ v → k ⇰ v
intersectionWithDict f = elimPrim21Dict EmptyDict (const EmptyDict) $ Dict ∘∘ Map.intersectionWith (flip f)

modifyDict ∷ (v → v) → k → k ⇰ v → k ⇰ v
modifyDict f k m = learnDict m EmptyDict $ case lookup k m of
  Nothing → m
  Just x → insertDict k (f x) m

onlyKeys ∷ (Ord k) ⇒ 𝒫 k → k ⇰ v → k ⇰ v
onlyKeys s m = iterOn (streamFromSet s) EmptyDict $ \ k → elimMaybe id (insertDict k) $ lookup k m

filterDict ∷ (v → 𝔹) → k ⇰ v → k ⇰ v
filterDict f = elimPrimDict EmptyDict $ Dict ∘ Map.filter f

-- # Errors

error ∷ 𝕊 → a
error msg = Prelude.error (chars msg)

undefined ∷ a
undefined = error "undefined"

-- # IO

class MonadIO (m ∷ ★ → ★) where io ∷ IO a → m a
instance MonadIO IO where io = id

instance Functor IO where map = mmap
instance Monad IO where {return=Prelude.return;(≫=) = (Prelude.>>=)}

print ∷ (MonadIO m) ⇒ 𝕊 → m ()
print = io ∘ Text.putStr

printLn ∷ (MonadIO m) ⇒ 𝕊 → m ()
printLn = io ∘ Text.putStrLn

failIO ∷ (MonadIO m) ⇒ 𝕊 → m a
failIO = io ∘ Prelude.fail ∘ chars

abortIO ∷ (MonadIO m) ⇒ m a
abortIO = io $ exitWith $ ExitFailure $ 𝕚 1

readFile :: (MonadIO m) ⇒ 𝕊 → m 𝕊
readFile = io ∘ Text.readFile ∘ chars

writeFile :: (MonadIO m) ⇒ 𝕊 → 𝕊 → m ()
writeFile fn = io ∘ Text.writeFile (chars fn)

trace ∷ 𝕊 → a → a
trace s x = unsafePerformIO $ do
  printLn s
  return x

traceM ∷ (Monad m) ⇒ 𝕊 → m ()
traceM msg = trace msg $ return ()

-- # Rebindable Syntax

ifThenElse ∷ 𝔹 → a → a → a
ifThenElse = fif

fromInteger ∷ ℤ → ℤ
fromInteger = id

fromString ∷ [ℂ] → 𝕊
fromString = 𝕤

fail ∷ [ℂ] → m a
fail = Prelude.error

(>>=) ∷ (Monad m) ⇒ m a → (a → m b) → m b
(>>=) = (≫=)

(>>) ∷ (Monad m) ⇒ m a → m b → m b
(>>) = (≫)

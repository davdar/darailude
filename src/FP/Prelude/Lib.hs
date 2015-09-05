module FP.Prelude.Lib where

import FP.Prelude.Core
import FP.Prelude.Effects
import FP.Prelude.Monads ()
import FP.Prelude.Lattice

import qualified Data.Char as Char
import qualified Data.Text as Text

-- # Char

lowerChar ∷ ℂ → ℂ
lowerChar = Char.toLower

upperChar ∷ ℂ → ℂ
upperChar = Char.toUpper

-- # String

lower ∷ 𝕊 → 𝕊
lower = Text.toLower

upper ∷ 𝕊 → 𝕊
upper = Text.toUpper

alignLeft ∷ ℕ → 𝕊 → 𝕊
alignLeft n s = s ⧺ 𝕤 (replicate (n - (length s ⊔ n)) ' ')

alignRight ∷ ℕ → 𝕊 → 𝕊
alignRight n s = 𝕤 (replicate (n - (length s ⊔ n)) ' ') ⧺ s

-- # Bool

cond ∷ (a → 𝔹) → (a → b) → (a → b) → a → b
cond p ft ff x = if p x then ft x else ff x

-- # Function

applyTo ∷ a → (a → b) → b
applyTo x f = f x

-- # Stream

data MStream m a where MStream ∷ s → (s → m (a,s)) → MStream m a

streamState ∷ s → MStream (StateT s Maybe) a → Stream a
streamState s₀ (MStream ss₀ f) = Stream (ss₀,s₀) $ \ (ss,s) → do
  (s',(a,ss')) ← runStateT (f ss) s
  return (a,(ss',s'))

length ∷ (ToStream a t) ⇒ t → ℕ
length = iter (const suc) (𝕟 0)

nth ∷ (ToStream a t) ⇒ ℕ → t → Maybe a
nth n t = case foldlk ff (Right zero) t of
  Left x → Just x
  Right _ → Nothing
  where
    ff (Right i) x' k = if i == n then Left x' else k $ Right $ suc i
    ff (Left _) _ _ = error "internal error"

srepeat ∷ a → Stream a
srepeat x = Stream () $ \ () → Just (x,())

scount ∷ Stream ℕ
scount = Stream (𝕟 0) $ \ i → Just(i,suc i)
  
szip ∷ Stream a → Stream b → Stream (a,b)
szip xs₁₀ xs₂₀ = Stream (xs₁₀,xs₂₀) $ \ (xs₁,xs₂) → do
  (x,xs₁') ← suncons xs₁
  (y,xs₂') ← suncons xs₂
  return ((x,y),(xs₁',xs₂'))

withIndex ∷ Stream a → Stream (ℕ,a)
withIndex = szip scount
  
-- # List

head ∷ [a] → Maybe a
head = fst ^∘ uncons

tail ∷ [a] → Maybe [a]
tail = snd ^∘ uncons

bigProduct ∷ [[a]] → [[a]]
bigProduct [] = [[]]
bigProduct (xs:xss) = do
  let xss' = bigProduct xss
  x ← xs
  map (x:) xss'

mapHead ∷ (a → a) → [a] → [a]
mapHead f xs₀ = list $ streamState True $ MStream xs₀ $ \ xs → do
  (x,xs') ← abortMaybe $ uncons xs
  isfirst ← get
  put False
  return $ if isfirst then (f x,xs') else (x,xs')

mapTail ∷ (a → a) → [a] → [a]
mapTail f xs₀ = list $ streamState True $ MStream xs₀ $ \ xs → do
  (x,xs') ← abortMaybe $ uncons xs
  isfirst ← get
  put False
  return $ if isfirst then (x,xs') else (f x,xs')

firstN ∷ ℕ → [a] → [a]
firstN n₀ xs₀ = list $ streamState (𝕟 0) $ MStream xs₀ $ \ xs → do
  n ← next
  when (n == n₀) abort
  abortMaybe $ uncons xs

filterMap ∷ ∀ a b. (a → Maybe b) → [a] → [b]
filterMap f xs₀ = list $ Stream xs₀ $ loop
  where
    loop ∷ [a] → Maybe (b,[a])
    loop xs = do
      (x,xs') ← uncons xs
      case f x of
        Nothing → loop xs'
        Just y → return (y,xs')

filter ∷ (a → Bool) → [a] → [a]
filter f = filterMap $ \ x → if f x then Just x else Nothing

prefixUntil ∷ ∀ a. (a → 𝔹) → [a] → ([a],[a])
prefixUntil _ [] = ([],[])
prefixUntil p (x:xs) 
  | p x = ([],x:xs)
  | otherwise =
      let (pre,post) = prefixUntil p xs
      in (x:pre,post)

uniques ∷ ∀ a. (Ord a) ⇒ [a] → [a]
uniques xs₀ = list $ streamState null $ MStream xs₀ loop
  where
    loop ∷ [a] → StateT (𝒫 a) Maybe (a,[a])
    loop xs = do
      (x,xs') ← abortMaybe $ uncons xs
      seen ← get
      if x ∈ seen 
        then loop xs' 
        else do
          modify $ insert x
          return (x,xs')

replicate ∷ ℕ → a → [a]
replicate n₀ x = list $ streamState (𝕟 0) $ MStream () $ \ () → do
  n ← next
  when (n == n₀) abort
  return (x,())

intersperse ∷ a → [a] → [a]
intersperse xᵢ xs₀ = list $ streamState (True,Nothing) $ MStream xs₀ $ \ xs → do
  (isFirst,delay) ← get
  if isFirst 
    then do
      put (False,delay)
      abortMaybe $ uncons xs
    else case delay of
      Nothing → do
        (x,xs') ← abortMaybe $ uncons xs
        put (isFirst,Just x)
        return (xᵢ,xs')
      Just x → do
        put (isFirst,Nothing)
        return (x,xs)

buildN ∷ ℕ → a → (a → a) → [a]
buildN n₀ x₀ f = list $ streamState (𝕟 0) $ MStream x₀ $ \ x → do
  n ← next
  when (n == n₀) abort
  return (x,f x)

upTo ∷ ℕ → [ℕ]
upTo n = buildN n zero suc

-- # Iteration

applyN ∷ ℕ → b → (b → b) → b
applyN n i f = iter (const f) i (upTo n)

appendN ∷ (Monoid a) ⇒ ℕ → a → a 
appendN n x = applyN n null $ (x ⧺) 

applyUntil ∷ (a → a) → (a → a → 𝔹) → a → a
applyUntil f p x =
  let x' = f x in
  x' `seq` if p x x' then x else applyUntil f p x'

applyUntilHistory ∷ (a → a) → (a → a → 𝔹) → a → [a]
applyUntilHistory f p x₀ = list $ Stream (Just x₀) $ \ xM → case xM of
  Nothing → Nothing
  Just x → Just
    ( x
    , let x' = f x in
      x' `seq` if p x x' then Nothing else Just x'
    )

poiter ∷ (POrd a) ⇒ (a → a) → a → a
poiter f = applyUntil f $ \ x x' → x' ⊑ x

poiterHistory ∷ (POrd a) ⇒ (a → a) → a → [a]
poiterHistory f = applyUntilHistory f (\ x x' → x' ⊑ x)

collect ∷ (Join a,POrd a) ⇒ (a → a) → a → a
collect f = poiter $ \ x → x ⊔ f x

collectN ∷ (Join a,POrd a) ⇒ ℕ → (a → a) → a → a
collectN n f x0 = applyN n x0 $ \ x → x ⊔ f x

collectHistory ∷ (Join a,POrd a) ⇒ (a → a) → a → [a]
collectHistory f = poiterHistory $ \ x → x ⊔ f x

collectHistoryN ∷ (Join a,POrd a) ⇒ ℕ → (a → a) → a → [a]
collectHistoryN n f i = buildN n i $ \ x → x ⊔ f x

diffs ∷ (JoinLattice a,Difference a,ToStream a t) ⇒ t → Stream a
diffs (stream → Stream s₀ f) = streamState bot $ MStream s₀ $ \ s → do
  xₚ ← get
  (x,s') ← abortMaybe $ f s
  put x
  return (x ⊟ xₚ,s')

collectDiffs ∷ (POrd a,JoinLattice a,Difference a) ⇒ (a → a) → a → Stream a
collectDiffs f = diffs ∘ collectHistory f

-- # Monads

many ∷ (Monad m,MonadMonoid m) ⇒ m a → m [a]
many aM = mconcat
  [ oneOrMore aM
  , return []
  ]

oneOrMore ∷ (Monad m,MonadMonoid m) ⇒ m a → m [a]
oneOrMore = uncurry (:) ^∘ oneOrMoreSplit

oneOrMoreSplit ∷ (Monad m,MonadMonoid m) ⇒ m a → m (a,[a])
oneOrMoreSplit aM = do
  x ← aM
  xs ← many aM
  return (x,xs)

twoOrMoreSplit ∷ (Monad m,MonadMonoid m) ⇒ m a → m (a,a,[a])
twoOrMoreSplit aM = do
  x1 ← aM
  (x2,xs) ← oneOrMoreSplit aM
  return (x1,x2,xs)

manySepBy ∷ (Monad m,MonadMonoid m) ⇒ m () → m a → m [a]
manySepBy uM xM = mconcat
  [ return []
  , do
      x ← xM
      xs ← manyPrefBy uM xM
      return $ x:xs
  ]

manyPrefBy ∷ (Monad m,MonadMonoid m) ⇒ m () → m a → m [a]
manyPrefBy uM xM = mconcat
  [ return []
  , do
      uM
      x ← xM
      xs ← manyPrefBy uM xM
      return $ x:xs
  ]

-- # Endo

newtype Endo a = Endo {runEndo ∷ a → a}
instance Monoid (Endo a) where
  null = Endo id
  (⧺) ∷ Endo a → Endo a → Endo a
  (⧺) = coerce ((∘) ∷ (a → a) → (a → a) → (a → a))

compose ∷ ∀ a. [a → a] → a → a 
compose fs = coerce $ concat (coerce fs ∷ [Endo a])

-- # KleisliEndo

data KleisliEndo m a = KleisliEndo { runKleisliEndo ∷ a → m a }

instance (Monad m) ⇒ Monoid (KleisliEndo m a) where
  null = KleisliEndo return
  g ⧺ f = KleisliEndo $ runKleisliEndo g *∘ runKleisliEndo f

kcompose ∷ (Monad m) ⇒ [a → m a] → a → m a
kcompose = runKleisliEndo ∘ concat ∘ map KleisliEndo

-- # Compose

newtype (t ⊡ u) a = Compose { runCompose ∷ t (u a) }
  deriving 
  ( Eq,Ord,POrd
  , Bot,Join,JoinLattice,Top,Meet,MeetLattice,Lattice
  )

onComposeIso ∷ (t (u a) → t (u b)) → (t ⊡ u) a → (t ⊡ u) b 
onComposeIso f (Compose x) = Compose $ f x

-- # Annotated

data Annotated t a = Annotated 
  { annotatedTag ∷ t
  , annotatedValue ∷ a 
  }

-- # Fixed

newtype Fixed f = Fixed { unfold ∷ f (Fixed f) }

-- # AnnotatedFixed

data AnnotatedFixed t f = AnnotatedFixed 
  { annotatedFixedTag ∷ t
  , unfoldAnnotated ∷ f (AnnotatedFixed t f)
  }

-- # First

data First a = NotFirst | First a
instance Monoid (First a) where
  null = NotFirst
  NotFirst ⧺ xM = xM
  First x ⧺ _ = First x

first ∷ Maybe a → First a
first Nothing = NotFirst
first (Just x) = First x

maybeFirst ∷ First a → Maybe a
maybeFirst NotFirst = Nothing
maybeFirst (First x) = Just x

-- # Last

data Last a = NotLast | Last a
instance Monoid (Last a) where
  null = NotLast
  _ ⧺ Last x = Last x
  xM ⧺ NotLast = xM

last ∷ Maybe a → Last a
last Nothing = NotLast
last (Just a) = Last a

maybeLast ∷ Last a → Maybe a
maybeLast NotLast = Nothing
maybeLast (Last a) = Just a

-- Proxy

data P a = P

-- -- instance (Functor t, Functor u) ⇒ Functor (t :.: u) where map = onComposeIso . map . map
-- -- instance (Functorial JoinLattice t, Functorial JoinLattice u) ⇒ Functorial JoinLattice (t :.: u) where
-- --   functorial ∷ forall a. (JoinLattice a) ⇒ W (JoinLattice ((t :.: u) a))
-- --   functorial =
-- --     with (functorial ∷ W (JoinLattice (u a))) $
-- --     with (functorial ∷ W (JoinLattice (t (u a)))) $
-- --     W
-- -- instance (Eq a) ⇒ Eq (Stamped a f) where (==) = (==) `on` stampedID
-- -- instance (Ord a) ⇒ Ord (Stamped a f) where compare = compare `on` stampedID
-- -- 
-- -- instance (Functorial Eq f) ⇒ Eq (Fix f) where
-- --   Fix x == Fix y = with (functorial ∷ W (Eq (f (Fix f)))) $ x == y
-- -- instance (Functorial Eq f, Functorial Ord f) ⇒ Ord (Fix f) where
-- --   Fix x `compare` Fix y = with (functorial ∷ W (Ord (f (Fix f)))) $ x `compare` y
-- -- 
-- -- stripStampedFix ∷ (Functor f) ⇒ StampedFix a f → Fix f
-- -- stripStampedFix (StampedFix _ f) = Fix $ map stripStampedFix f
-- -- instance (Eq a)           ⇒ Eq (StampedFix a f)           where (==)     = (==)     `on` stampedFixID
-- -- instance (Ord a)          ⇒ Ord (StampedFix a f)          where compare  = compare  `on` stampedFixID
-- -- instance (POrd a) ⇒ POrd (StampedFix a f) where pcompare = pcompare `on` stampedFixID
-- -- 
-- -- -- }}}
-- -- 
-- -- 
-- -- -- ListSetWithTop {{{
-- -- 
-- -- listSetWithTopElim ∷ b → (ListSet a → b) → ListSetWithTop a → b
-- -- listSetWithTopElim i f = \case { ListSetTop → i ; ListSetNotTop xs → f xs }
-- -- 
-- -- instance Buildable a (ListSetWithTop a) where 
-- --   nil = ListSetNotTop nil 
-- --   _ & ListSetTop = ListSetTop
-- --   x & ListSetNotTop xs = ListSetNotTop $ x & xs
-- -- instance Bot (ListSetWithTop a) where bot = ListSetNotTop nil
-- -- instance Join (ListSetWithTop a) where
-- --   ListSetTop ⊔ _ = ListSetTop
-- --   _ ⊔ ListSetTop = ListSetTop
-- --   ListSetNotTop x ⊔ ListSetNotTop y = ListSetNotTop $ x ⧺ y
-- -- instance Top (ListSetWithTop a) where top = ListSetTop
-- -- instance Meet (ListSetWithTop a) where
-- --   ListSetTop /\ x = x
-- --   x /\ ListSetTop = x
-- --   ListSetNotTop x /\ ListSetNotTop y = ListSetNotTop $ x ⧺ y
-- -- instance (Ord a) ⇒ Difference (ListSetWithTop a) where
-- --   ListSetTop \\ ListSetTop = nil
-- --   ListSetTop \\ ListSetNotTop _ = ListSetTop
-- --   ListSetNotTop _ \\ ListSetTop = nil
-- --   ListSetNotTop xs \\ ListSetNotTop ys = fromSet $ toSet xs \\ toSet ys
-- -- instance Monoid (ListSetWithTop a) where { null = bot ; (⧺) = (⊔) }
-- -- instance JoinLattice (ListSetWithTop a)
-- -- instance MeetLattice (ListSetWithTop a)
-- -- instance MonadBot ListSetWithTop where mbot = bot
-- -- instance MonadPlus ListSetWithTop where (<+>) = (⊔)
-- -- instance MonadTop ListSetWithTop where mtop = top
-- -- instance MonadAppend ListSetWithTop where (<⧺>) = (⊔)
-- -- instance Unit ListSetWithTop where unit = ListSetNotTop . single
-- -- instance Bind ListSetWithTop where
-- --   ListSetTop >>= _ = ListSetTop
-- --   ListSetNotTop xs >>= f = joins $ map f xs
-- -- instance Functor ListSetWithTop where map = mmap
-- -- instance Product ListSetWithTop where (<*>) = mpair
-- -- instance Applicative ListSetWithTop where (<@>) = mapply
-- -- instance Monad ListSetWithTop
-- -- 
-- -- -- }}}
-- -- 
-- -- -- SetWithTop {{{
-- -- 
-- -- data SetWithTop a = SetTop | SetNotTop (Set a) deriving (Eq, Ord)
-- -- 
-- -- setWithTopElim ∷ b → (Set a → b) → SetWithTop a → b
-- -- setWithTopElim b _ SetTop = b
-- -- setWithTopElim _ f (SetNotTop x) = f x
-- -- 
-- -- setFromListWithTop ∷ (Ord a) ⇒ ListSetWithTop a → SetWithTop a
-- -- setFromListWithTop ListSetTop = SetTop
-- -- setFromListWithTop (ListSetNotTop xs) = SetNotTop $ fromList $ toList xs
-- -- 
-- -- listFromSetWithTop ∷ SetWithTop a → ListSetWithTop a
-- -- listFromSetWithTop SetTop = ListSetTop
-- -- listFromSetWithTop (SetNotTop xs) = ListSetNotTop $ fromSet xs
-- -- 
-- -- instance (Ord a) ⇒ POrd (SetWithTop a) where
-- --   SetTop       `pcompare` SetTop       = PEQ
-- --   SetTop       `pcompare` _            = PGT
-- --   _            `pcompare` SetTop       = PLT
-- --   SetNotTop xs `pcompare` SetNotTop ys = xs `pcompare` ys
-- -- instance Bot (SetWithTop a) where bot = SetNotTop empty
-- -- instance (Ord a) ⇒ Buildable a (SetWithTop a) where
-- --   nil = bot
-- --   _ & SetTop = SetTop
-- --   x & SetNotTop xs = SetNotTop $ x & xs
-- -- instance Difference (SetWithTop a) where
-- --   SetTop \\ SetTop = bot
-- --   SetTop \\ SetNotTop _ = SetTop
-- --   SetNotTop _ \\ SetTop = bot
-- --   SetNotTop xs \\ SetNotTop ys = SetNotTop $ xs \\ ys
-- -- instance Join (SetWithTop a) where
-- --   SetTop ⊔ _ = SetTop
-- --   _ ⊔ SetTop = SetTop
-- --   SetNotTop x ⊔ SetNotTop y = SetNotTop $ x ⊔ y
-- -- instance Top (SetWithTop a) where top = SetTop
-- -- instance Meet (SetWithTop a) where
-- --   SetTop /\ x = x
-- --   x /\ SetTop = x
-- --   SetNotTop x /\ SetNotTop y = SetNotTop $ x /\ y
-- -- instance MonadBot SetWithTop where mbot = bot
-- -- instance MonadPlus SetWithTop where (<+>) = (⊔)
-- -- instance MonadTop SetWithTop where mtop = top
-- -- instance Product SetWithTop where 
-- --   SetTop <*> _ = SetTop
-- --   _ <*> SetTop = SetTop
-- --   SetNotTop xs <*> SetNotTop ys = SetNotTop $ xs <*> ys
-- -- instance Bind SetWithTop where
-- --   SetTop >>= _ = SetTop
-- --   SetNotTop xs >>= f = joins $ setMap f xs
-- -- 
-- -- instance JoinLattice (SetWithTop a)
-- -- instance MeetLattice (SetWithTop a)
-- -- 
-- -- -- }}}
-- -- 
-- maybeToList ∷ ∀ m a. (Functor m) ⇒ MaybeT m a → ListT m a
-- maybeToList aM = ListT $ ff ^$ runMaybeT aM
--   where
--     ff ∷ Maybe a → [a]
--     ff Nothing = []
--     ff (Just a) = [a]

-- -- # Set --
-- 
-- transposeSet ∷ 𝒫 (𝒫 a) → 𝒫 (𝒫 a)
-- transposeSet aMM = loop $ list $ stream aMM
--   where
--     loop ∷ [(𝒫 a)] → 𝒫 (𝒫 a)
--     loop [] = EmptySet
--     loop (s:ss) =
--       learnSet s (loop ss) $
--       set $ map set $ transpose $ map list $ s:ss
-- 
-- setBigProduct ∷ 𝒫 (𝒫 a) → 𝒫 (𝒫 a)
-- setBigProduct s = case remove s of
--   Nothing → set $ single bot
--   Just (xs,xss) → learnSet xs null $
--     let xss' = setBigProduct xss
--     in xs ≫=* \ x →
--        mapSet (insert x) xss'
-- 

module FP.Prelude.Monads where

import FP.Prelude.Core
import FP.Prelude.Effects
import FP.Prelude.Constraints
import FP.Prelude.Morphism
import FP.Prelude.Lattice

-- E and I effects can be implemented by combining unit, discard and commute
-- All effects are implemented this way except for continuation effects
class FunctorUnit (t ∷ (★ → ★) → (★ → ★)) where funit ∷ (Functor m) ⇒ m ↝ t m
class FunctorDiscard (t ∷ (★ → ★) → (★ → ★)) where fdiscard ∷ (Functor m) ⇒ t (t m) ↝ t m

-- For commuting effects
class FunctorFunctor (t ∷ (★ → ★) → (★ → ★)) where fmap ∷ m ↝ n → t m ↝ t n
class FunctorIsoFunctor (t ∷ (★ → ★) → (★ → ★)) where fisomap ∷ (m ↝ n,n ↝ m) → (t m ↝ t n)

-- # ID

newtype ID a = ID { runID ∷ a }
  deriving
  ( Eq,Ord
  , POrd,Bot,Join,Meet,Top,JoinLattice,Monoid
  )

instance Functor ID where 
  map ∷ (a → b) → ID a → ID b
  map f = ID ∘ f ∘ runID
instance FunctorM ID where 
  mapM ∷ (Monad m) ⇒ (a → m b) → ID a → m (ID b)
  mapM f = map ID ∘ f ∘ runID
instance Monad ID where 
  return ∷ a → ID a
  return = ID
  (≫=) ∷ ID a → (a → ID b) → ID b
  aM ≫= k = k $ runID aM

instance Functorial Bot ID where functorial = W
instance Functorial Join ID where functorial = W
instance Functorial Meet ID where functorial = W
instance Functorial Top ID where functorial = W
instance Functorial JoinLattice ID where functorial = W
instance Functorial Monoid ID where functorial = W

-- # Failure

-- Base Effect

type Failure = FailureT ID

failure ∷ Maybe a → Failure a
failure = abortMaybe

runFailure ∷ Failure a → Maybe a
runFailure = runID ∘ runFailureT

-- Commuting with self

commuteFailure ∷ (Functor m) ⇒ FailureT (FailureT m) ↝ FailureT (FailureT m)
commuteFailure aMM = FailureT $ FailureT $ ff ^$ runFailureT $ runFailureT aMM
  where
    ff ∷ Maybe (Maybe a) → Maybe (Maybe a)
    ff Nothing = Just Nothing
    ff (Just Nothing) = Nothing
    ff (Just (Just a)) = Just (Just a)
  
-- Functor and Monad

instance (Functor m) ⇒ Functor (FailureT m) where
  map ∷ (a → b) → FailureT m a → FailureT m b
  map f = FailureT ∘ map (map f) ∘ runFailureT
instance (Monad m) ⇒ Monad (FailureT m) where
  return ∷ a → FailureT m a
  return = FailureT ∘ return ∘ Just
  (≫=) ∷ FailureT m a → (a → FailureT m b) → FailureT m b
  aM ≫= k = FailureT $ do
    aM' ← runFailureT aM
    case aM' of
      Nothing → return Nothing
      Just a → runFailureT $ k a

-- Higher Functor

instance FunctorUnit FailureT where 
  funit ∷ (Functor m) ⇒ m ↝ FailureT m
  funit = FailureT ∘ map Just
instance FunctorDiscard FailureT where
  fdiscard ∷ (Functor m) ⇒ FailureT (FailureT m) ↝ FailureT m
  fdiscard = FailureT ∘ ff ^∘ runFailureT ∘ runFailureT
    where
      ff ∷ Maybe (Maybe a) → Maybe a
      ff Nothing = Nothing
      ff (Just aM) = aM
instance FunctorFunctor FailureT where
  fmap ∷ (m ↝ n) → FailureT m ↝ FailureT n
  fmap f = FailureT ∘ f ∘ runFailureT

-- MonadMonoid and MonadJoin

instance (MonadMonoid m) ⇒ MonadMonoid (FailureT m) where
  mzero ∷ FailureT m a
  mzero = FailureT mzero
  (<⧺>) ∷ FailureT m a → FailureT m a → FailureT m a
  aM₁ <⧺> aM₂ = FailureT $ runFailureT aM₁ <⧺> runFailureT aM₂
instance (MonadBot m) ⇒ MonadBot (FailureT m) where
  mbot ∷ FailureT m a
  mbot = FailureT mbot
instance (MonadJoin m) ⇒ MonadJoin (FailureT m) where
  (<⊔>) ∷ FailureT m a → FailureT m a → FailureT m a
  aM₁ <⊔> aM₂ = FailureT $ runFailureT aM₁ <⊔> runFailureT aM₂
instance (MonadJoinLattice m) ⇒ MonadJoinLattice (FailureT m)

-- Failure Effect

instance (Functor m) ⇒ MonadFailure (FailureT m) where
  failureE ∷ FailureT (FailureT m) ↝ FailureT m
  failureE = fdiscard ∘ commuteFailure
  failureI ∷ FailureT m ↝ FailureT (FailureT m)
  failureI = commuteFailure ∘ funit

-- Maybe Failure Effect

instance MonadFailure Maybe where
  failureE ∷ FailureT Maybe ↝ Maybe
  failureE = runFailure ∘ failureE ∘ fmap failure
  failureI ∷ Maybe ↝ FailureT Maybe
  failureI = fmap runFailure ∘ failureI ∘ failure

-- # Error

-- Base Effect

type Error e = ErrorT e ID

runError ∷ Error e a → e ⨄ a
runError = runID ∘ runErrorT

-- Commuting with self

errorCommute ∷ (Functor m) ⇒ ErrorT e (ErrorT e m) ↝ ErrorT e (ErrorT e m)
errorCommute = ErrorT ∘ ErrorT ∘ ff ^∘ runErrorT ∘ runErrorT
  where
    ff ∷ e ⨄ (e ⨄ a) → e ⨄ (e ⨄ a)
    ff (Left e) = Right (Left e)
    ff (Right (Left e)) = Left e
    ff (Right (Right a)) = Right $ Right a

-- Functor and Monad

instance (Functor m) ⇒ Functor (ErrorT e m) where
  map ∷ (a → b) → ErrorT e m a → ErrorT e m b
  map f aM = ErrorT $ mapRight f ^$ runErrorT aM
instance (Monad m) ⇒ Monad (ErrorT e m) where
  return ∷ a → ErrorT e m a
  return = ErrorT ∘ return ∘ Right
  (≫=) ∷ ErrorT e m a → (a → ErrorT e m b) → ErrorT e m b
  aM ≫= k = ErrorT $ do
    aeM ← runErrorT aM
    case aeM of
      Left e → return $ Left e
      Right a → runErrorT $ k a

-- Higher Functor

instance FunctorUnit (ErrorT e) where
  funit ∷ (Functor m) ⇒ m ↝ ErrorT e m
  funit aM = ErrorT $ Right ^$ aM
instance FunctorDiscard (ErrorT e) where
  fdiscard ∷ (Functor m) ⇒ ErrorT e (ErrorT e m) ↝ ErrorT e m
  fdiscard = ErrorT ∘ ff ^∘ runErrorT ∘ runErrorT
    where
      ff (Left e) = Left e
      ff (Right ea) = ea
instance FunctorFunctor (ErrorT e) where
  fmap ∷ m ↝ n → ErrorT e m ↝ ErrorT e n
  fmap f = ErrorT ∘ f ∘ runErrorT

-- MonadMonoid and MonadJoin

instance (MonadMonoid m) ⇒ MonadMonoid (ErrorT e m) where
  mzero ∷ ErrorT e m a
  mzero = ErrorT mzero
  (<⧺>) ∷ ErrorT e m a → ErrorT e m a → ErrorT e m a
  aM₁ <⧺> aM₂ = ErrorT $ runErrorT aM₁ <⧺> runErrorT aM₂
instance (MonadBot m) ⇒ MonadBot (ErrorT e m) where
  mbot ∷ ErrorT e m a
  mbot = ErrorT mbot
instance (MonadJoin m) ⇒ MonadJoin (ErrorT e m) where
  (<⊔>) ∷ ErrorT e m a → ErrorT e m a → ErrorT e m a
  aM₁ <⊔> aM₂ = ErrorT $ runErrorT aM₁ <⊔> runErrorT aM₂

-- Error Effect

instance (Functor m) ⇒ MonadError e (ErrorT e m) where
  errorE ∷ ErrorT e (ErrorT e m) ↝ ErrorT e m
  errorE = fdiscard ∘ errorCommute
  errorI ∷ ErrorT e m ↝ ErrorT e (ErrorT e m)
  errorI = errorCommute ∘ funit

-- Sum Error Effect

instance MonadError e ((⨄) e) where
  errorE ∷ ErrorT e ((⨄) e) ↝ (⨄) e
  errorE = runError ∘ errorE ∘ fmap throwSum
  errorI ∷ (⨄) e ↝ ErrorT e ((⨄) e)
  errorI = fmap runError ∘ errorI ∘ throwSum

-- # Reader

-- Base Effect

type Reader r = ReaderT r ID

reader ∷ (r → a) → Reader r a
reader f = ReaderT $ ID ∘ f

runReader ∷ Reader r a → r → a
runReader = runID ∘∘ runReaderT

runReaderWith ∷ r → Reader r a → a
runReaderWith = flip runReader

-- Commuting with self

readerCommute ∷ ReaderT r₁ (ReaderT r₂ m) ↝ ReaderT r₂ (ReaderT r₁ m)
readerCommute aMM = ReaderT $ \ r₂ → ReaderT $ \ r₁ → runReaderTWith r₂ $ runReaderTWith r₁ aMM

-- Functor and Monad

instance (Functor m) ⇒ Functor (ReaderT r m) where
  map ∷ (a → b) → ReaderT r m a → ReaderT r m b
  map f aM = ReaderT $ map f ∘ runReaderT aM
instance (Monad m) ⇒ Monad (ReaderT r m) where
  return ∷ a → ReaderT r m a
  return = ReaderT ∘ const ∘ return
  (≫=) ∷ ReaderT r m a → (a → ReaderT r m b) → ReaderT r m b
  aM ≫= k = ReaderT $ \ r → runReaderTWith r ∘ k *$ runReaderTWith r aM

-- Higher Functor

instance FunctorUnit (ReaderT r) where
  funit ∷ (Functor m) ⇒ m ↝ ReaderT r m
  funit = ReaderT ∘ const
instance FunctorDiscard (ReaderT r) where
  fdiscard ∷ (Functor m) ⇒ ReaderT r (ReaderT r m) ↝ ReaderT r m 
  fdiscard aMM = ReaderT $ \ r → runReaderTWith r $ runReaderTWith r aMM
instance FunctorFunctor (ReaderT r) where
  fmap ∷ (m ↝ n) → (ReaderT r m ↝ ReaderT r n)
  fmap f aM = ReaderT $ \ r → f $ runReaderTWith r aM

-- MonadMonoid and Join

instance (MonadMonoid m) ⇒ MonadMonoid (ReaderT r m) where
  mzero ∷ ReaderT r m a
  mzero = ReaderT $ const mzero
  (<⧺>) ∷ ReaderT r m a → ReaderT r m a → ReaderT r m a
  aM₁ <⧺> aM₂ = ReaderT $ \ r → runReaderT aM₁ r <⧺> runReaderT aM₂ r
instance (MonadBot m) ⇒ MonadBot (ReaderT r m) where
  mbot ∷ ReaderT r m a
  mbot = ReaderT $ const mbot
instance (MonadJoin m) ⇒ MonadJoin (ReaderT r m) where
  (<⊔>) ∷ ReaderT r m a → ReaderT r m a → ReaderT r m a
  aM₁ <⊔> aM₂ = ReaderT $ \ r → runReaderT aM₁ r <⊔> runReaderT aM₂ r
instance (MonadJoinLattice m) ⇒ MonadJoinLattice (ReaderT r m)

-- Reader Effect

instance (Functor m) ⇒ MonadReader r (ReaderT r m) where
  readerE ∷ ReaderT r (ReaderT r m) ↝ ReaderT r m
  readerE = fdiscard ∘ readerCommute
  readerI ∷ ReaderT r m ↝ ReaderT r (ReaderT r m)
  readerI = readerCommute ∘ funit

-- Base Reader Effect

instance MonadReader r ((→) r) where
  readerE ∷ ReaderT r ((→) r) ↝ (→) r
  readerE = runReader ∘ readerE ∘ fmap reader
  readerI ∷ (→) r ↝ ReaderT r ((→) r)
  readerI = fmap runReader ∘ readerI ∘ reader

-- # Writer

-- Base Effect

type Writer o = WriterT o ID

writer ∷ (o,a) → Writer o a
writer = WriterT ∘ ID

runWriter ∷ Writer o a → (o,a)
runWriter = runID ∘ runWriterT

execWriter ∷ Writer o a → o
execWriter = fst ∘ runWriter

-- Commuting with self

writerCommute ∷ ∀ m o₁ o₂. (Functor m) ⇒ WriterT o₁ (WriterT o₂ m) ↝ WriterT o₂ (WriterT o₁ m)
writerCommute aMM = WriterT $ WriterT $ ff ^$ runWriterT $ runWriterT aMM
  where
    ff ∷ (o₂,(o₁,a)) → (o₁,(o₂,a))
    ff (o₂,(o₁,a)) = (o₁,(o₂,a))

-- Functor and Monad

instance (Functor m) ⇒ Functor (WriterT o m) where 
  map ∷ (a → b) → WriterT o m a → WriterT o m b
  map f = WriterT ∘ mapSnd f ^∘ runWriterT
instance (Monad m,Monoid o) ⇒ Monad (WriterT o m) where
  return ∷ a → WriterT o m a
  return = WriterT ∘ return ∘ (null,)
  (≫=) ∷ WriterT o m a → (a → WriterT o m b) → WriterT o m b
  aM ≫= k = WriterT $ do
    (o₁,a) ← runWriterT aM
    (o₂,b) ← runWriterT $ k a
    o' ← return ♯$ o₁ ⧺ o₂
    return (o',b)

-- Higher Functor

instance (Monoid o) ⇒ FunctorUnit (WriterT o) where
  funit ∷ (Functor m) ⇒ m ↝ WriterT o m
  funit = WriterT ∘ map (null,)
instance FunctorDiscard (WriterT o) where
  fdiscard ∷ (Functor m) ⇒ WriterT o (WriterT o m) ↝ WriterT o m
  fdiscard = snd ^∘ runWriterT
instance FunctorFunctor (WriterT o) where
  fmap ∷ (m ↝ n) → (WriterT o m ↝ WriterT o n)
  fmap f aM = WriterT $ f $ runWriterT aM

-- MonadMonoid and MonadJoin

instance (MonadMonoid m,Monoid o) ⇒ MonadMonoid (WriterT o m) where
  mzero ∷ WriterT o m a
  mzero = WriterT mzero
  (<⧺>) ∷ WriterT o m a → WriterT o m a → WriterT o m a
  aM₁ <⧺> aM₂ = WriterT $ runWriterT aM₁ <⧺> runWriterT aM₂
instance (MonadBot m,Monoid o) ⇒ MonadBot (WriterT o m) where
  mbot ∷ WriterT o m a
  mbot = WriterT mbot
instance (MonadJoin m,Monoid o) ⇒ MonadJoin (WriterT o m) where
  (<⊔>) ∷ WriterT o m a → WriterT o m a → WriterT o m a
  aM₁ <⊔> aM₂ = WriterT $ runWriterT aM₁ <⊔> runWriterT aM₂
instance (MonadJoinLattice m,Monoid o) ⇒ MonadJoinLattice (WriterT o m)

-- Monoid Functor

instance (Functorial Monoid m,Monoid o,Monoid a) ⇒ Monoid (WriterT o m a) where
  null ∷ WriterT o m a
  null =
    with (functorial ∷ W (Monoid (m (o,a)))) $
    WriterT null
  (⧺) ∷ WriterT o m a → WriterT o m a → WriterT o m a
  aM₁ ⧺ aM₂ =
    with (functorial ∷ W (Monoid (m (o,a)))) $
    WriterT $ runWriterT aM₁ ⧺ runWriterT aM₂
instance (Functorial Monoid m,Monoid o) ⇒ Functorial Monoid (WriterT o m) where functorial = W

-- Writer Effect

instance (Functor m,Monoid o) ⇒ MonadWriter o (WriterT o m) where
  writerE ∷ WriterT o (WriterT o m) ↝ WriterT o m
  writerE = fdiscard ∘ writerCommute
  writerI ∷ WriterT o m ↝ WriterT o (WriterT o m)
  writerI = writerCommute ∘ funit

-- Base Writer Effect

instance (Monoid o) ⇒ MonadWriter o ((,) o) where
  writerE ∷ WriterT o ((,) o) ↝ ((,) o)
  writerE = runWriter ∘ writerE ∘ fmap writer
  writerI ∷ ((,) o) ↝ WriterT o ((,) o)
  writerI = fmap runWriter ∘ writerI ∘ writer

-- # State

-- Base Effect

type State s = StateT s ID

runStateWith ∷ s → State s a → (s,a)
runStateWith = runID ∘∘ runStateTWith

evalStateWith ∷ s → State s a → a
evalStateWith = snd ∘∘ runStateWith

execStateWith ∷ s → State s a → s
execStateWith = fst ∘∘ runStateWith

-- Commuting with self

stateCommute ∷ ∀ m s₁ s₂. (Functor m) ⇒ StateT s₁ (StateT s₂ m) ↝ StateT s₂ (StateT s₁ m)
stateCommute aMM = StateT $ \ s₂ → StateT $ \ s₁ → ff ^$ runStateTWith s₂ $ runStateTWith s₁ aMM
  where
    ff ∷ (s₂,(s₁,a)) → (s₁,(s₂,a))
    ff (s₂,(s₁,a)) = (s₁,(s₂,a))

-- Functor and Monad

instance (Functor m) ⇒ Functor (StateT s m) where 
  map ∷ (a → b) → StateT s m a → StateT s m b
  map f aM = StateT $ \ s → mapSnd f ^$ runStateT aM s
instance (Monad m) ⇒ Monad (StateT s m) where
  return ∷ a → StateT s m a
  return x = StateT $ \ s → return (s,x)
  (≫=) ∷ StateT s m a → (a → StateT s m b) → StateT s m b
  aM ≫= k = StateT $ \ s → do
    (s',a) ← runStateT aM s
    runStateT (k a) s'

-- Higher Functor

instance FunctorUnit (StateT s) where 
  funit ∷ (Functor m) ⇒ m ↝ StateT s m
  funit aM = StateT $ \ s → (s,) ^$ aM
instance FunctorDiscard (StateT s) where 
  fdiscard ∷ (Functor m) ⇒ StateT s (StateT s m) ↝ StateT s m
  fdiscard aMM = StateT $ \ s → runStateTWith s $ snd ^$ runStateTWith s aMM
instance FunctorFunctor (StateT s) where
  fmap ∷ (m ↝ n) → StateT s m ↝ StateT s n
  fmap f aM = StateT $ f ∘ runStateT aM

-- MonadMonoid and MonadJoin

instance (MonadMonoid m) ⇒ MonadMonoid (StateT s m) where
  mzero ∷ StateT s m a
  mzero = StateT $ const mzero
  (<⧺>) ∷ StateT s m a → StateT s m a → StateT s m a
  aM₁ <⧺> aM₂ = StateT $ \ s → runStateT aM₁ s <⧺> runStateT aM₂ s
instance (MonadBot m) ⇒ MonadBot (StateT s m) where
  mbot ∷ StateT s m a
  mbot = StateT $ const mbot
instance (MonadJoin m) ⇒ MonadJoin (StateT s m) where
  (<⊔>) ∷ StateT s m a → StateT s m a → StateT s m a
  aM₁ <⊔> aM₂ = StateT $ \ s → runStateT aM₁ s <⊔> runStateT aM₂ s
instance (MonadJoinLattice m) ⇒ MonadJoinLattice (StateT s m)
instance (MonadTop m) ⇒ MonadTop (StateT s m) where 
  mtop ∷ StateT s m a
  mtop = StateT $ const mtop

-- Monoid Functor

instance (Functorial Monoid m,Monoid s,Monoid a) ⇒ Monoid (StateT s m a) where
  null ∷ StateT s m a
  null =
    with (functorial ∷ W (Monoid (m (s,a)))) $
    StateT $ \ _ → null
  (⧺) ∷ StateT s m a → StateT s m a → StateT s m a
  aM₁ ⧺ aM₂ =
    with (functorial ∷ W (Monoid (m (s,a)))) $
    StateT $ \ s → runStateT aM₁ s ⧺ runStateT aM₂ s
instance (Functorial Monoid m,Monoid s) ⇒ Functorial Monoid (StateT s m) where functorial = W

-- JoinLattice Functor

instance (Functorial Bot m,Bot s,Bot a) ⇒ Bot (StateT s m a) where
  bot ∷ StateT s m a
  bot = 
    with (functorial ∷ W (Bot (m (s,a)))) $
    StateT $ \ _ → bot
instance (Functorial Join m,Join s,Join a) ⇒ Join (StateT s m a) where
  (⊔) ∷ StateT s m a → StateT s m a → StateT s m a
  aM₁ ⊔ aM₂ = 
    with (functorial ∷ W (Join (m (s,a)))) $
    StateT $ \ s → runStateT aM₁ s ⊔ runStateT aM₂ s
instance (Functorial Bot m,Functorial Join m,JoinLattice s,JoinLattice a) ⇒ JoinLattice (StateT s m a)
instance (Functorial Bot m,Functorial Join m,JoinLattice s) ⇒ Functorial JoinLattice (StateT s m) where functorial = W

-- State Effect

instance (Functor m) ⇒ MonadState s (StateT s m) where
  stateE ∷ StateT s (StateT s m) ↝ StateT s m
  stateE = fdiscard ∘ stateCommute
  stateI ∷ StateT s m ↝ StateT s (StateT s m) 
  stateI = stateCommute ∘ funit

-- # NondetT

-- Commuting with self

pluck ∷ [a] → [[a]] → Maybe ([a],[[a]])
pluck [] _ = Nothing
pluck (x:xs) [] = Just ([x],[xs])
pluck (x₁:xs₁) (xs₂:xss) = case pluck xs₂ xss of
  Nothing → Nothing
  Just (ys₂,xss') → Just (x₁:ys₂,xs₁:xss')

transpose ∷ [[a]] → [[a]]
transpose [] = [[]]
transpose (xs:xss) =
  case pluck xs xss of
    Nothing → []
    Just (ys,xss') → ys:transpose xss'

nondetCommute ∷ (Functor m) ⇒ NondetT (NondetT m) ↝ NondetT (NondetT m)
nondetCommute = NondetT ∘ NondetT ∘ map transpose ∘ runNondetT ∘ runNondetT

-- Functor and Monad

instance (Functor m) ⇒ Functor (NondetT m) where
  map ∷ (a → b) → NondetT m a → NondetT m b
  map f = NondetT ∘ map (map f) ∘ runNondetT
instance (Monad m,Functorial Monoid m) ⇒ Monad (NondetT m) where
  return ∷ a → NondetT m a
  return = NondetT ∘ return ∘ single
  (≫=) ∷ NondetT m a → (a → NondetT m b) → NondetT m b
  aM ≫= k = NondetT $ do
    xs ← runNondetT aM
    runNondetT $ concat $ k ^$ xs

-- Higher Functor

instance FunctorUnit NondetT where
  funit ∷ (Functor m) ⇒ m ↝ NondetT m
  funit = NondetT ∘ map return
instance FunctorDiscard NondetT where
  fdiscard ∷ (Functor m) ⇒ NondetT (NondetT m) ↝ NondetT m
  fdiscard = NondetT ∘ concat ^∘ runNondetT ∘ runNondetT
instance FunctorFunctor NondetT where
  fmap ∷ m ↝ n → NondetT m ↝ NondetT n
  fmap f = NondetT ∘ f ∘ runNondetT

-- Monad Monoid

instance (Monad m,Functorial Monoid m) ⇒ MonadMonoid (NondetT m) where {mzero = null;(<⧺>) = (⧺)}
instance MonadMonoid [] where {mzero = null;(<⧺>) = (⧺)}

-- Monoid Functor

instance (Functorial Monoid m) ⇒ Monoid (NondetT m a) where
  null ∷ NondetT m a
  null = 
    with (functorial ∷ W (Monoid (m [a]))) $
    NondetT null
  (⧺) ∷ NondetT m a → NondetT m a → NondetT m a
  xs ⧺ ys = 
    with (functorial ∷ W (Monoid (m [a]))) $
    NondetT $ runNondetT xs ⧺ runNondetT ys
instance (Functorial Monoid m) ⇒ Functorial Monoid (NondetT m) where functorial = W

-- Nondet Effect

instance (Functor m) ⇒ MonadNondet (NondetT m) where
  nondetE ∷ NondetT (NondetT m) ↝ NondetT m
  nondetE = fdiscard ∘ nondetCommute
  nondetI ∷ NondetT m ↝ NondetT (NondetT m)
  nondetI = nondetCommute ∘ funit

-- # ContT

-- Base Effect

type Cont r = ContT r ID

cont ∷ ((a → r) → r) → Cont r a
cont k = ContT $ \ k' → ID $ k $ \ a → runID $ k' a

runCont ∷ Cont r a → (a → r) → r
runCont aM f = runID $ runContT aM (ID ∘ f)

evalCont ∷ Cont r r → r
evalCont aM = runCont aM id

-- Functor and Monad

instance Functor (ContT r m) where map = mmap
instance Monad (ContT r m) where
  return ∷ a → ContT r m a
  return a = ContT $ \ k → k a
  (≫=) ∷ ContT r m a → (a → ContT r m b) → ContT r m b
  aM ≫= kM = ContT $ \ (k ∷ b → m r) → runContT aM $ \ a → runContT (kM a) k

-- Higher Functor

instance FunctorIsoFunctor (ContT r) where
  fisomap ∷ (m ↝ n,n ↝ m) → (ContT r m ↝ ContT r n)
  fisomap (to,from) aM = ContT $ \ (k ∷ a → n r) → to $ runContT aM $ \ a → from $ k a

-- Cont Effect

instance (Monad m) ⇒ MonadCont r (ContT r m) where
  contE ∷ ContT r (ContT r m) ↝ ContT r m
  contE aMM = ContT $ \ (k ∷ a → m r) → 
    evalContT $ runContT aMM $ \ a → ContT $ \ (k' ∷ r → m r) → k' *$ k a
  contI ∷ ContT r m ↝ ContT r (ContT r m)
  contI aM = ContT $ \ (k₁ ∷ a → ContT r m r) → ContT $ \ (k₂ ∷ r → m r) →
    k₂ *$ runContT aM $ \ a → evalContT (k₁ a)

-- # OpaqueContT

-- Base Effect

type OpaqueCont k r = OpaqueContT k r ID

opaqueCont ∷ (k r ID a → r) → OpaqueCont k r a
opaqueCont nk = OpaqueContT $ ID ∘ nk

runOpaqueCont ∷ OpaqueCont k r a → k r ID a → r
runOpaqueCont = runID ∘∘ runOpaqueContT

metaCont ∷ (Isomorphism3 (k r) (ContFun r)) ⇒ ((a → r) → r) → OpaqueCont k r a
metaCont nk = opaqueCont $ \ (k ∷ k r ID a) → nk $ (∘) runID ∘ runContFun $ isoTo3 k

runMetaCont ∷ (Isomorphism3 (ContFun r) (k r)) ⇒ OpaqueCont k r a → (a → r) → r
runMetaCont aM k = runOpaqueCont aM $ isoTo3 $ ContFun $ ID ∘ k

evalOpaqueCont ∷ (Isomorphism3 (ContFun r) (k r)) ⇒ OpaqueCont k r r → r
evalOpaqueCont aM = runMetaCont aM id

-- Functor and Monad

instance (Monad m,Isomorphism3 (ContFun r) (k r)) ⇒ Functor (OpaqueContT k r m) where map = mmap
instance (Monad m,Isomorphism3 (ContFun r) (k r)) ⇒ Monad (OpaqueContT k r m) where
  return ∷ a → OpaqueContT k r m a
  return a = OpaqueContT $ \ k → runContFun (isoFrom3 k) a
  (≫=) ∷ OpaqueContT k r m a → (a → OpaqueContT k r m b) → OpaqueContT k r m b
  aM ≫= kM = OpaqueContT $ \ (k ∷ k r m a) → runMetaContT aM $ \ a → runOpaqueContT (kM a) k

-- Higher Functor

instance (Isomorphism3 (ContFun r) (k r)) ⇒ FunctorIsoFunctor (OpaqueContT k r) where
  fisomap ∷ (m ↝ n,n ↝ m) → OpaqueContT k r m ↝ OpaqueContT k r n
  fisomap tofrom = opaque ∘ fisomap tofrom ∘ meta

-- OpaqueCont Effect

class Balloon k r | k → r where
  inflate ∷ (Monad m) ⇒ k r m ↝ k r (OpaqueContT k r m)
  deflate ∷ (Monad m) ⇒ k r (OpaqueContT k r m) ↝ k r m

instance (Monad m,Isomorphism3 (ContFun r) (k r),Balloon k r) ⇒ MonadOpaqueCont k r (OpaqueContT k r m) where
  opaqueContE ∷ OpaqueContT k r (OpaqueContT k r m) a → OpaqueContT k r m a
  opaqueContE kk = OpaqueContT $ \ (k ∷ k r m a ) → runMetaContTWith return $ runOpaqueContT kk $ inflate k
  opaqueContI ∷ OpaqueContT k r m a → OpaqueContT k r (OpaqueContT k r m) a
  opaqueContI aM = OpaqueContT $ \ k₁ → metaContT $ \ (k₂ ∷ r → m r) → k₂ *$ runOpaqueContT aM $ deflate k₁

instance (Monad m,Isomorphism3 (ContFun r) (k r)) ⇒ MonadCont r (OpaqueContT k r m) where
  contE ∷ ContT r (OpaqueContT k r m) ↝ OpaqueContT k r m
  contE aMM = metaContT $ \ (k ∷ a → m r) →
    runMetaContTWith return $ runContT aMM $ \ a → metaContT $ \ (k' ∷ r → m r) → k' *$ k a
  contI ∷ OpaqueContT k r m ↝ ContT r (OpaqueContT k r m)
  contI aM = ContT $ \ (k₁ ∷ a → OpaqueContT k r m r) → metaContT $ \ (k₂ ∷ r → m r) →
    k₂ *$ runMetaContT aM $ \ a → runMetaContT (k₁ a) return

----------------------
-- Monads Commuting --
----------------------

-- # Failure // *

-- ## Failure // Error [ISO]

failureErrorCommute ∷ (Functor m) ⇒ FailureT (ErrorT e m) ↝ ErrorT e (FailureT m)
failureErrorCommute = ErrorT ∘ FailureT ∘ map ff ∘ runErrorT ∘ runFailureT
  where
    ff ∷ (e ⨄ Maybe a) → Maybe (e ⨄ a)
    ff (Left e) = Just (Left e)
    ff (Right Nothing) = Nothing
    ff (Right (Just x)) = Just (Right x)

errorFailureCommute ∷ (Functor m) ⇒ ErrorT e (FailureT m) ↝ FailureT (ErrorT e m)
errorFailureCommute = FailureT ∘ ErrorT ∘ map ff ∘ runFailureT ∘ runErrorT
  where
    ff ∷ Maybe (e ⨄ a) → (e ⨄ Maybe a)
    ff Nothing = Right Nothing
    ff (Just (Left e)) = Left e
    ff (Just (Right x)) = Right (Just x)

instance (Functor m,MonadFailure m) ⇒ MonadFailure (ErrorT e m) where
  failureE ∷ FailureT (ErrorT e m) ↝ ErrorT e m
  failureE = fmap failureE ∘ failureErrorCommute
  failureI ∷ ErrorT e m ↝ FailureT (ErrorT e m)
  failureI = errorFailureCommute ∘ fmap failureI

instance (Functor m,MonadError e m) ⇒ MonadError e (FailureT m) where
  errorE ∷ ErrorT e (FailureT m) ↝ FailureT m
  errorE = fmap errorE ∘ errorFailureCommute
  errorI ∷ FailureT m ↝ ErrorT e (FailureT m)
  errorI = failureErrorCommute ∘ fmap errorI

-- ## Failure // Reader [ISO]

failureReaderCommute ∷ (Functor m) ⇒ FailureT (ReaderT r m) ↝ ReaderT r (FailureT m)
failureReaderCommute aMRM = ReaderT $ \ r → FailureT $ runReaderTWith r $ runFailureT aMRM

readerFailureCommute ∷ (Functor m) ⇒ ReaderT r (FailureT m) ↝ FailureT (ReaderT r m)
readerFailureCommute aRMM = FailureT $ ReaderT $ \ r → runFailureT $ runReaderTWith r aRMM

instance (Functor m,MonadFailure m) ⇒ MonadFailure (ReaderT r m) where
  failureE ∷ FailureT (ReaderT r m) ↝ ReaderT r m
  failureE = fmap failureE ∘ failureReaderCommute
  failureI ∷ ReaderT r m ↝ FailureT (ReaderT r m)
  failureI = readerFailureCommute ∘ fmap failureI

instance (Functor m,MonadReader r m) ⇒ MonadReader r (FailureT m) where
  readerE ∷ ReaderT r (FailureT m) ↝ FailureT m
  readerE = fmap readerE ∘ readerFailureCommute
  readerI ∷ FailureT m ↝ ReaderT r (FailureT m)
  readerI = failureReaderCommute ∘ fmap readerI

-- ## Failure // Writer [ADJ]

failureWriterCommute ∷ (Functor m) ⇒ FailureT (WriterT o m) ↝ WriterT o (FailureT m)
failureWriterCommute aMRM = WriterT $ FailureT $ ff ^$ runWriterT $ runFailureT aMRM
  where
    ff ∷ (o,Maybe a) → Maybe (o,a)
    ff (_,Nothing) = Nothing
    ff (o,Just a) = Just (o,a)

writerFailureCommute ∷ (Monoid o,Functor m) ⇒ WriterT o (FailureT m) ↝ FailureT (WriterT o m)
writerFailureCommute aRMM = FailureT $ WriterT $ ff ^$ runFailureT $ runWriterT aRMM
  where
    ff ∷ (Monoid o) ⇒ Maybe (o,a) → (o,Maybe a)
    ff Nothing = (null,Nothing)
    ff (Just (o,a)) = (o,Just a)

instance (Monoid o,Functor m,MonadFailure m) ⇒ MonadFailure (WriterT o m) where
  failureE ∷ FailureT (WriterT o m) ↝ WriterT o m
  failureE = fmap failureE ∘ failureWriterCommute
  failureI ∷ WriterT o m ↝ FailureT (WriterT o m)
  failureI = writerFailureCommute ∘ fmap failureI

instance (Monoid o,Functor m,MonadWriter o m) ⇒ MonadWriter o (FailureT m) where
  writerE ∷ WriterT o (FailureT m) ↝ FailureT m
  writerE = fmap writerE ∘ writerFailureCommute
  writerI ∷ FailureT m ↝ WriterT o (FailureT m)
  writerI = failureWriterCommute ∘ fmap writerI

-- ## Failure // State [ADJ]

failureStateCommute ∷ ∀ s m. (Functor m) ⇒ FailureT (StateT s m) ↝ StateT s (FailureT m)
failureStateCommute aMSM = StateT $ \ s₁ → FailureT $ ff ^$ runStateTWith s₁ $ runFailureT aMSM
  where
    ff ∷ (s,Maybe a) → Maybe (s,a)
    ff (_,Nothing) = Nothing
    ff (s₂,Just a) = Just (s₂,a)

stateFailureCommute ∷ ∀ s m. (Functor m) ⇒ StateT s (FailureT m) ↝ FailureT (StateT s m)
stateFailureCommute aSMM = FailureT $ StateT $ \ s₁ → ff s₁ ^$ runFailureT $ runStateTWith s₁ aSMM
  where
    ff ∷ s → (Maybe (s,a)) → (s,Maybe a)
    ff s₁ Nothing = (s₁,Nothing)
    ff _ (Just (s₂,a)) = (s₂,Just a)

instance (Functor m,MonadFailure m) ⇒ MonadFailure (StateT s m) where
  failureE ∷ FailureT (StateT s m) ↝ StateT s m
  failureE = fmap failureE ∘ failureStateCommute
  failureI ∷ StateT s m ↝ FailureT (StateT s m)
  failureI = stateFailureCommute ∘ fmap failureI

instance (Functor m,MonadState s m) ⇒ MonadState s (FailureT m) where
  stateE ∷ StateT s (FailureT m) ↝ FailureT m
  stateE = fmap stateE ∘ stateFailureCommute
  stateI ∷ FailureT m ↝ StateT s (FailureT m)
  stateI = failureStateCommute ∘ fmap stateI

-- ## Failure // Nondet [ADJ]

failureNondetCommute ∷ (Functor m) ⇒ FailureT (NondetT m) ↝ NondetT (FailureT m)
failureNondetCommute = NondetT ∘ FailureT ∘ map ff ∘ runNondetT ∘ runFailureT
  where
    ff ∷ [Maybe a] → Maybe [a]
    ff [] = Just []
    ff (Nothing:_) = Nothing
    ff (Just x:xMs) = (x:) ^$ ff xMs

nondetFailureCommute ∷ (Functor m) ⇒ NondetT (FailureT m) ↝ FailureT (NondetT m)
nondetFailureCommute = FailureT ∘ NondetT ∘ map ff ∘ runFailureT ∘ runNondetT
  where
    ff ∷ Maybe [a] → [Maybe a]
    ff Nothing = [Nothing]
    ff (Just xs) = map Just xs

instance (Functor m,MonadFailure m) ⇒ MonadFailure (NondetT m) where
  failureE ∷ FailureT (NondetT m) ↝ NondetT m
  failureE = fmap failureE ∘ failureNondetCommute
  failureI ∷ NondetT m ↝ FailureT (NondetT m)
  failureI = nondetFailureCommute ∘ fmap failureI

instance (Functor m,MonadNondet m) ⇒ MonadNondet (FailureT m) where
  nondetE ∷ NondetT (FailureT m) ↝ FailureT m
  nondetE = fmap nondetE ∘ nondetFailureCommute
  nondetI ∷ FailureT m ↝ NondetT (FailureT m)
  nondetI = failureNondetCommute ∘ fmap nondetI

-- ## Failure // Cont

-- TODO: 

-- ## Failure // OpaqueCont

-- TODO: 

-- # Error // *

-- ## Error // Reader [ISO]

errorReaderCommute ∷ ErrorT e (ReaderT r m) ↝ ReaderT r (ErrorT e m)
errorReaderCommute aMRM = ReaderT $ \ r → ErrorT $ runReaderTWith r $ runErrorT aMRM

readerErrorCommute ∷ ReaderT r (ErrorT e m) ↝ ErrorT e (ReaderT r m)
readerErrorCommute aRMM = ErrorT $ ReaderT $ \ r → runErrorT $ runReaderTWith r aRMM

instance (Functor m,MonadError e m) ⇒ MonadError e (ReaderT r m) where
  errorE ∷ ErrorT e (ReaderT r m) ↝ ReaderT r m
  errorE = fmap errorE ∘ errorReaderCommute
  errorI ∷ ReaderT r m ↝ ErrorT e (ReaderT r m)
  errorI = readerErrorCommute ∘ fmap errorI

instance (Functor m,MonadReader r m) ⇒ MonadReader r (ErrorT e m) where
  readerE ∷ ReaderT r (ErrorT e m) ↝ ErrorT e m
  readerE = fmap readerE ∘ readerErrorCommute
  readerI ∷ ErrorT e m ↝ ReaderT r (ErrorT e m)
  readerI = errorReaderCommute ∘ fmap readerI

-- ## Error // Writer [ADJ]

errorWriterCommute ∷ ∀ e o m. (Functor m) ⇒ ErrorT e (WriterT o m) ↝ WriterT o (ErrorT e m)
errorWriterCommute = WriterT ∘ ErrorT ∘ ff ^∘ runWriterT ∘ runErrorT
  where
    ff ∷ (o,e ⨄ a) → (e ⨄ (o,a))
    ff (_,Left e) = Left e
    ff (e,Right a) = Right (e,a)

writerErrorCommute ∷ (Functor m,Monoid o) ⇒ WriterT o (ErrorT e m) ↝ ErrorT e (WriterT o m)
writerErrorCommute = ErrorT ∘ WriterT ∘ ff ^∘ runErrorT ∘ runWriterT
  where
    ff ∷ (Monoid o) ⇒ (e ⨄ (o,a)) → (o,e ⨄ a)
    ff (Left e) = (null,Left e)
    ff (Right (o,a)) = (o,Right a)

instance (Functor m,MonadError e m,Monoid o) ⇒ MonadError e (WriterT o m) where
  errorE ∷ ErrorT e (WriterT o m) ↝ WriterT o m
  errorE = fmap errorE ∘ errorWriterCommute
  errorI ∷ WriterT o m ↝ ErrorT e (WriterT o m)
  errorI = writerErrorCommute ∘ fmap errorI

instance (Functor m,MonadWriter o m,Monoid o) ⇒ MonadWriter o (ErrorT e m) where
  writerE ∷ WriterT o (ErrorT e m) ↝ ErrorT e m
  writerE = fmap writerE ∘ writerErrorCommute
  writerI ∷ ErrorT e m ↝ WriterT o (ErrorT e m)
  writerI = errorWriterCommute ∘ fmap writerI

-- ## Error // State [ADJ]

errorStateCommute ∷ (Functor m) ⇒ ErrorT e (StateT s m) ↝ StateT s (ErrorT e m)
errorStateCommute aMRM = StateT $ \ s → ErrorT $ ff ^$ runStateTWith s $ runErrorT aMRM
  where
    ff ∷ (s,e ⨄ a) → e ⨄ (s,a)
    ff (_,Left e) = Left e
    ff (s,Right a) = Right (s,a)

stateErrorCommute ∷ (Functor m) ⇒ StateT s (ErrorT e m) ↝ ErrorT e (StateT s m)
stateErrorCommute aRMM = ErrorT $ StateT $ \ s → ff s ^$ runErrorT $ runStateTWith s aRMM
  where
    ff ∷ s → e ⨄ (s,a) → (s,e ⨄ a)
    ff s (Left e) = (s,Left e)
    ff _ (Right (s,a)) = (s,Right a)

instance (Functor m,MonadError e m) ⇒ MonadError e (StateT s m) where
  errorE ∷ ErrorT e (StateT s m) ↝ StateT s m
  errorE = fmap errorE ∘ errorStateCommute
  errorI ∷ StateT s m ↝ ErrorT e (StateT s m)
  errorI = stateErrorCommute ∘ fmap errorI

instance (Functor m,MonadState s m) ⇒ MonadState s (ErrorT e m) where
  stateE ∷ StateT s (ErrorT e m) ↝ ErrorT e m
  stateE = fmap stateE ∘ stateErrorCommute
  stateI ∷ ErrorT e m ↝ StateT s (ErrorT e m)
  stateI = errorStateCommute ∘ fmap stateI

-- ## Error // Nondet [ADJ]

errorNondetCommute ∷ (Functor m) ⇒ ErrorT e (NondetT m) ↝ NondetT (ErrorT e m)
errorNondetCommute = NondetT ∘ ErrorT ∘ map ff ∘ runNondetT ∘ runErrorT
  where
    ff ∷ [e ⨄ a] → e ⨄ [a]
    ff [] = Right []
    ff (Left e:_) = Left e
    ff (Right x:xsM) = (x:) ^$ ff xsM

nondetErrorCommute ∷ (Functor m) ⇒ NondetT (ErrorT e m) ↝ ErrorT e (NondetT m)
nondetErrorCommute = ErrorT ∘ NondetT ∘ map ff ∘ runErrorT ∘ runNondetT
  where
    ff ∷ e ⨄ [a] → [e ⨄ a]
    ff (Left e) = [Left e]
    ff (Right xs) = map Right xs

instance (Functor m,MonadError e m) ⇒ MonadError e (NondetT m) where
  errorE ∷ ErrorT e (NondetT m) ↝ NondetT m
  errorE = fmap errorE ∘ errorNondetCommute
  errorI ∷ NondetT m ↝ ErrorT e (NondetT m)
  errorI = nondetErrorCommute ∘ fmap errorI

instance (Functor m,MonadNondet m) ⇒ MonadNondet (ErrorT e m) where
  nondetE ∷ NondetT (ErrorT e m) ↝ ErrorT e m
  nondetE = fmap nondetE ∘ nondetErrorCommute
  nondetI ∷ ErrorT e m ↝ NondetT (ErrorT e m)
  nondetI = errorNondetCommute ∘ fmap nondetI

-- ## Error // Cont

-- TODO: 

-- ## Error // OpaqueCont

-- TODO: 

-- # Reader // *

-- ## Reader // Writer [ISO]

readerWriterCommute ∷ ReaderT r (WriterT w m) ↝ WriterT w (ReaderT r m)
readerWriterCommute aRWM = WriterT $ ReaderT $ \ r → runWriterT $ runReaderTWith r aRWM

writerReaderCommute ∷ WriterT w (ReaderT r m) ↝ ReaderT r (WriterT w m)
writerReaderCommute aWRM = ReaderT $ \ r → WriterT $ runReaderTWith r $ runWriterT aWRM

instance (Monoid w,Functor m,MonadReader r m) ⇒ MonadReader r (WriterT w m) where
  readerE ∷ ReaderT r (WriterT w m) ↝ WriterT w m
  readerE = fmap readerE ∘ readerWriterCommute
  readerI ∷ WriterT w m ↝ ReaderT r (WriterT w m)
  readerI = writerReaderCommute ∘ fmap readerI

instance (Monoid w,Functor m,MonadWriter w m) ⇒ MonadWriter w (ReaderT r m) where
  writerE ∷ WriterT w (ReaderT r m) ↝ ReaderT r m
  writerE = fmap writerE ∘ writerReaderCommute
  writerI ∷ ReaderT r m ↝ WriterT w (ReaderT r m)
  writerI = readerWriterCommute ∘ fmap writerI

-- ## Reader // State [ISO]

readerStateCommute ∷ (Functor m) ⇒ ReaderT r (StateT s m) ↝ StateT s (ReaderT r m)
readerStateCommute aRSM = StateT $ \ s → ReaderT $ \ r → runStateTWith s $ runReaderTWith r aRSM

stateReaderCommute ∷ (Functor m) ⇒ StateT s (ReaderT r m) ↝ ReaderT r (StateT s m)
stateReaderCommute aSRM = ReaderT $ \ r → StateT $ \ s → runReaderTWith r $ runStateTWith s aSRM

instance (Functor m,MonadReader r m) ⇒ MonadReader r (StateT s m) where
  readerE ∷ ReaderT r (StateT s m) ↝ StateT s m
  readerE = fmap readerE ∘ readerStateCommute
  readerI ∷ StateT s m ↝ ReaderT r (StateT s m)
  readerI = stateReaderCommute ∘ fmap readerI

instance (Functor m,MonadState s m) ⇒ MonadState s (ReaderT r m) where
  stateE ∷ StateT s (ReaderT r m) ↝ ReaderT r m
  stateE = fmap stateE ∘ stateReaderCommute
  stateI ∷ ReaderT r m ↝ StateT s (ReaderT r m)
  stateI = readerStateCommute ∘ fmap stateI

-- ## Reader // Nondet [ISO]

readerNondetCommute ∷ (Functor m) ⇒ ReaderT r (NondetT m) ↝ NondetT (ReaderT r m)
readerNondetCommute aM = NondetT $ ReaderT $ \ r → runNondetT $ runReaderTWith r aM

nondetReaderCommute ∷ (Functor m) ⇒ NondetT (ReaderT r m) ↝ ReaderT r (NondetT m)
nondetReaderCommute aM = ReaderT $ \ r → NondetT $ runReaderTWith r $ runNondetT aM

instance (Functor m,MonadReader r m) ⇒ MonadReader r (NondetT m) where
  readerE ∷ ReaderT r (NondetT m) ↝ NondetT m
  readerE = fmap readerE ∘ readerNondetCommute
  readerI ∷ NondetT m ↝ ReaderT r (NondetT m)
  readerI = nondetReaderCommute ∘ fmap readerI

instance (Functor m,MonadNondet m) ⇒ MonadNondet (ReaderT r m) where
  nondetE ∷ NondetT (ReaderT r m) ↝ ReaderT r m
  nondetE = fmap nondetE ∘ nondetReaderCommute
  nondetI ∷ ReaderT r m ↝ NondetT (ReaderT r m)
  nondetI = readerNondetCommute ∘ fmap nondetI

-- ## Reader // Cont

-- TODO

-- ## Reader // OpaqueCont

-- TODO

-- # Writer // *

-- ## Writer // State [ISO]

writerStateCommute ∷ ∀ o s m. (Functor m) ⇒ WriterT o (StateT s m) ↝ StateT s (WriterT o m)
writerStateCommute aRMM = StateT $ \ s₁ → WriterT $ ff ^$ runStateTWith s₁ $ runWriterT aRMM
  where
    ff ∷ (s,(o,a)) → (o,(s,a))
    ff (s,(o,a)) = (o,(s,a))

stateWriterCommute ∷ ∀ o s m. (Functor m) ⇒ StateT s (WriterT o m) ↝ WriterT o (StateT s m)
stateWriterCommute aMRM = WriterT $ StateT $ ff ^∘ runWriterT ∘ runStateT aMRM
  where
    ff ∷ (o,(s,a)) → (s,(o,a))
    ff (o,(s,a)) = (s,(o,a))

instance (Monoid o,Functor m,MonadWriter o m) ⇒ MonadWriter o (StateT s m) where
  writerE ∷ WriterT o (StateT s m) ↝ StateT s m
  writerE = fmap writerE ∘ writerStateCommute
  writerI ∷ StateT s m ↝ WriterT o (StateT s m)
  writerI = stateWriterCommute ∘ fmap writerI

instance (Functor m,Monoid o,MonadState s m) ⇒ MonadState s (WriterT o m) where
  stateE ∷ StateT s (WriterT o m) ↝ WriterT o m
  stateE = fmap stateE ∘ stateWriterCommute
  stateI ∷ WriterT o m ↝ StateT s (WriterT o m)
  stateI = writerStateCommute ∘ fmap stateI

-- ## Writer // Nondet [ADJ]

writerNondetCommute ∷ ∀ o m. (Functor m,Monoid o) ⇒ WriterT o (NondetT m) ↝ NondetT (WriterT o m)
writerNondetCommute aMM = NondetT $ WriterT $ ff ^$ runNondetT $ runWriterT aMM
  where
    ff ∷ [(o,a)] → (o,[a])
    ff asL = (concat $ fst ^$ asL,snd ^$ asL)

nondetWriterCommute ∷ ∀ o m. (Functor m) ⇒ NondetT (WriterT o m) ↝ WriterT o (NondetT m)
nondetWriterCommute aMM = WriterT $ NondetT $ ff ^$ runWriterT $ runNondetT aMM
  where
    ff ∷ (o,[a]) → [(o,a)]
    ff (o,xs) = (o,) ^$ xs

instance (Functor m,MonadWriter o m,Monoid o) ⇒ MonadWriter o (NondetT m) where
  writerE ∷ WriterT o (NondetT m) ↝ NondetT m
  writerE = fmap writerE ∘ writerNondetCommute
  writerI ∷ NondetT m ↝ WriterT o (NondetT m)
  writerI = nondetWriterCommute ∘ fmap writerI

instance (Functor m,MonadNondet m,Monoid o) ⇒ MonadNondet (WriterT o m) where
  nondetE ∷ NondetT (WriterT o m) ↝ WriterT o m
  nondetE = fmap nondetE ∘ nondetWriterCommute
  nondetI ∷ WriterT o m ↝ NondetT (WriterT o m)
  nondetI = writerNondetCommute ∘ fmap nondetI

-- ## Writer // Cont

-- TODO

-- ## Writer // OpaqeuCont

-- TODO

-- # State // *

-- ## State // Nondet [ADJ]

stateNondetCommute ∷ ∀ s m. (Functor m,Monoid s) ⇒ StateT s (NondetT m) ↝ NondetT (StateT s m)
stateNondetCommute aMM = NondetT $ StateT $ \ s → ff ^$ runNondetT $ runStateTWith s aMM
  where
    ff ∷ [(s,a)] → (s,[a])
    ff asL = (concat $ fst ^$ asL,snd ^$ asL)

nondetStateCommute ∷ ∀ s m. (Functor m) ⇒ NondetT (StateT s m) ↝ StateT s (NondetT m)
nondetStateCommute aMM = StateT $ \ s → NondetT $ ff ^$ runStateTWith s $ runNondetT aMM
  where
    ff ∷ (s,[a]) → [(s,a)]
    ff (s,xs) = (s,) ^$ xs

instance (Functor m,MonadState s m,Monoid s) ⇒ MonadState s (NondetT m) where
  stateE ∷ StateT s (NondetT m) ↝ NondetT m
  stateE = fmap stateE ∘ stateNondetCommute
  stateI ∷ NondetT m ↝ StateT s (NondetT m)
  stateI = nondetStateCommute ∘ fmap stateI

instance (Functor m,MonadNondet m,Monoid s) ⇒ MonadNondet (StateT s m) where
  nondetE ∷ NondetT (StateT s m) ↝ StateT s m
  nondetE = fmap nondetE ∘ nondetStateCommute
  nondetI ∷ StateT s m ↝ NondetT (StateT s m)
  nondetI = stateNondetCommute ∘ fmap nondetI

-- ## State // Cont [???]

stateKonCommute ∷ StateT s (ContT (s,r) m) ↝ ContT r (StateT s m)
stateKonCommute aSK = ContT $ \ (k ∷ a → StateT s m r) → StateT $ \ s →
  runContT (runStateTWith s aSK) $ \ (s',a) → runStateTWith s' $ k a

konStateCommute ∷ ContT r (StateT s m) ↝ StateT s (ContT (s,r) m)
konStateCommute aKS = StateT $ \ s → ContT $ \ (k ∷ (s,a) → m (s,r)) →
  runStateTWith s $ runContT aKS $ \ a → StateT $ \ s' → k (s',a)

instance (Monad m,MonadState s m) ⇒ MonadState s (ContT r m) where
  stateE ∷ StateT s (ContT r m) ↝ ContT r m
  stateE =
    fisomap (stateE,stateI)
    ∘ stateKonCommute
    ∘ stateE
    ∘ fmap (konStateCommute ∘ fisomap (stateI,stateE ∷ StateT s m ↝ m))
  stateI ∷ ContT r m ↝ StateT s (ContT r m)
  stateI =
    fmap (fisomap (stateE,stateI) ∘ stateKonCommute)
    ∘ stateI
    ∘ konStateCommute
    ∘ fisomap (stateI,stateE ∷ StateT s m ↝ m)

-- # State // OpaqueCont [???]

instance (Monad m,MonadState s m,Isomorphism3 (ContFun r) (k r)) ⇒ MonadState s (OpaqueContT k r m) where
  stateE ∷ StateT s (OpaqueContT k r m) ↝ OpaqueContT k r m
  stateE =
    opaque
    ∘ stateE
    ∘ fmap meta
  stateI ∷ OpaqueContT k r m ↝ StateT s (OpaqueContT k r m)
  stateI =
    fmap opaque
    ∘ stateI
    ∘ meta

---------
-- RWS --
---------

newtype RWST r o s m a = RWST { runRWST ∷ ReaderT r (WriterT o (StateT s m)) a }
  deriving
  ( Functor,Monad
  , MonadFailure
  , MonadError e
  , MonadReader r,MonadWriter o,MonadState s
  )
runRWSTWith ∷ ∀ r o s m a. (Functor m) ⇒ r → s → RWST r o s m a → m (s,o,a)
runRWSTWith r s₀ aM = ff ^$ runStateTWith s₀ $ runWriterT $ runReaderTWith r $ runRWST aM 
  where
    ff ∷ (s,(o,a)) → (s,o,a)
    ff (s,(o,a)) = (s,o,a)

-- Base Effect

type RWS r o s = RWST r o s ID

runRWSWith ∷ r → s → RWS r o s a → (s,o,a)
runRWSWith r s aM = runID $ runRWSTWith r s aM

-- Higher Functor

instance FunctorFunctor (RWST r o s) where
  fmap ∷ (m ↝ n) → RWST r o s m a → RWST r o s n a
  fmap f = RWST ∘ fmap (fmap (fmap f)) ∘ runRWST

--------------------
-- Adding Effects --
--------------------

-- # AddWriterT

newtype AddWriterT o₁₂ o₁ m a = AddWriterT { runAddWriterT ∷ WriterT o₁ m a }
  deriving 
    ( Functor,Monad
    , MonadMonoid,MonadBot,MonadJoin
    , MonadFailure
    , MonadError e
    , MonadReader r
    , MonadState s
    -- TODO: implement 
    -- , MonadCont r
    -- TODO: implement and role annotation
    -- , MonadOpaqueCont k r
    )

mergeWriter ∷ (Functor m) ⇒ WriterT o₁ (WriterT o₂ m) a → WriterT (o₁,o₂) m a
mergeWriter = WriterT ∘ ff ^∘ runWriterT ∘ runWriterT
  where
    ff ∷ (o₂,(o₁,a)) → ((o₁,o₂),a)
    ff (o₂,(o₁,a)) = ((o₁,o₂),a)

splitWriter ∷ (Functor m) ⇒ WriterT (o₁,o₂) m a → WriterT o₁ (WriterT o₂ m) a
splitWriter = WriterT ∘ WriterT ∘ ff ^∘ runWriterT
  where
    ff ∷ ((o₁,o₂),a) → (o₂,(o₁,a))
    ff ((o₁,o₂),a) = (o₂,(o₁,a))

instance (Functor m,MonadWriter o₂ m,Monoid o₁,Isomorphism o₁₂ (o₁,o₂)) ⇒ MonadWriter o₁₂ (AddWriterT o₁₂ o₁ m) where
  writerI ∷ AddWriterT o₁₂ o₁ m ↝ WriterT o₁₂ (AddWriterT o₁₂ o₁ m)
  writerI = 
    fmap AddWriterT
    ∘ mapOutput isoFrom
    ∘ mergeWriter
    ∘ fmap (writerCommute ∘ fmap writerI)
    ∘ writerI
    ∘ runAddWriterT
  writerE ∷ WriterT o₁₂ (AddWriterT o₁₂ o₁ m) ↝ AddWriterT o₁₂ o₁ m
  writerE = 
    AddWriterT
    ∘ writerE
    ∘ fmap (fmap writerE ∘ writerCommute) 
    ∘ splitWriter
    ∘ mapOutput isoTo
    ∘ fmap runAddWriterT

-- # AddStateT

newtype AddStateT s₁₂ s₁ m a = AddStateT { runAddStateT ∷ StateT s₁ m a }
  deriving 
    ( Functor,Monad
    , MonadMonoid,MonadBot,MonadJoin
    , MonadFailure
    , MonadError e
    , MonadReader r
    , MonadWriter o
    -- TODO: implement
    -- , MonadCont r
    -- TODO: implement
    -- , MonadOpaqueCont k r
    )

mergeState ∷ (Functor m) ⇒ StateT s₁ (StateT s₂ m) a → StateT (s₁,s₂) m a
mergeState aMM = StateT $ \ (s₁,s₂) → ff ^$ runStateT (runStateT aMM s₁) s₂
  where
    ff ∷ (s₂,(s₁,a)) → ((s₁,s₂),a)
    ff (s₂,(s₁,a)) = ((s₁,s₂),a)

splitState ∷ (Functor m) ⇒ StateT (s₁,s₂) m a → StateT s₁ (StateT s₂ m) a
splitState aM = StateT $ \ s₁ → StateT $ \ s₂ → ff ^$ runStateT aM (s₁,s₂)
  where
    ff ∷ ((s₁,s₂),a) → (s₂,(s₁,a))
    ff ((s₁,s₂),a) = (s₂,(s₁,a))

instance (Functor m,MonadState s₂ m,Isomorphism s₁₂ (s₁,s₂)) ⇒ MonadState s₁₂ (AddStateT s₁₂ s₁ m) where
  stateI ∷ AddStateT s₁₂ s₁ m ↝ StateT s₁₂ (AddStateT s₁₂ s₁ m)
  stateI = 
    fmap AddStateT
    ∘ mapStateT isoFrom isoTo
    ∘ mergeState
    ∘ fmap (stateCommute ∘ fmap stateI)
    ∘ stateI
    ∘ runAddStateT
  stateE ∷ StateT s₁₂ (AddStateT s₁₂ s₁ m) ↝ AddStateT s₁₂ s₁ m
  stateE = 
    AddStateT
    ∘ stateE
    ∘ fmap (fmap stateE ∘ stateCommute) 
    ∘ splitState
    ∘ mapStateT isoTo isoFrom
    ∘ fmap runAddStateT

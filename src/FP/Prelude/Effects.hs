module FP.Prelude.Effects where

import FP.Prelude.Core
import FP.Prelude.Morphism
import FP.Prelude.Lens

-- 4: sums
infixr 4 <⧺>       -- (<⧺>) ∷ (MonadMonoid m) ⇒ m a → m a → m a
infixr 4 <⊔>       -- (<⊔>) ∷ (MonadJoin m) ⇒ m a → m a → m a

-- 5: products
infixr 5 <⊓>       -- (<⊓>) ∷ (MonadMeet m) ⇒ m a → m a → m a

-- # List of effects
--
-- - Failure
-- - Error
-- - Reader
-- - Writer
-- - State
-- - Continuation
-- - OpaqueContinuation

class MonadMonoid (m ∷ ★ → ★) where {mzero ∷ m a;(<⧺>) ∷ m a → m a → m a}
class MonadBot (m ∷ ★ → ★) where mbot ∷ m a
class MonadJoin (m ∷ ★ → ★) where (<⊔>) ∷ m a → m a → m a
class MonadTop (m ∷ ★ → ★) where mtop ∷ m a
class MonadMeet (m ∷ ★ → ★) where (<⊓>) ∷ m a → m a → m a
class (MonadBot m,MonadJoin m) ⇒ MonadJoinLattice m

mconcat ∷ (MonadMonoid m) ⇒ [m a] → m a
mconcat = foldr (<⧺>) mzero

mlist ∷ (Monad m,MonadMonoid m) ⇒ [a] → m a
mlist = foldr ((<⧺>) ∘ return) mzero

msum ∷ (MonadJoinLattice m) ⇒ [m a] → m a
msum = iter (<⊔>) mbot

mset ∷ (Monad m,MonadJoinLattice m) ⇒ [a] → m a
mset = iter ((<⊔>) ∘ return) mbot


type MonadRWS r o s m = (MonadReader r m,MonadWriter o m,MonadState s m)

-- # Failure

newtype FailureT m a = FailureT {runFailureT ∷ m (Maybe a)}
class MonadFailure (m ∷ ★ → ★) where 
  failureE ∷ FailureT m ↝ m
  failureI ∷ m ↝ FailureT m

-- Effectors

effMaybe ∷ (MonadFailure m) ⇒ m (Maybe a) → m a 
effMaybe = failureE ∘ FailureT

abort ∷ (Monad m,MonadFailure m) ⇒ m a 
abort = effMaybe $ return Nothing

abortMaybe ∷ (Monad m,MonadFailure m) ⇒ Maybe a → m a
abortMaybe = elimMaybe abort return

-- Observers

obsMaybe ∷ (MonadFailure m) ⇒ m a → m (Maybe a) 
obsMaybe = runFailureT ∘ failureI

(<|>) ∷ (Monad m,MonadFailure m) ⇒ m a → m a → m a
aM1 <|> aM2 = do
  aM' ← obsMaybe aM1
  case aM' of
    Nothing → aM2
    Just a → return a

-- Combos

tries ∷ (Monad m,MonadFailure m) ⇒ [m a] → m a
tries = foldr (<|>) abort

-- # Error

newtype ErrorT e m a = ErrorT {runErrorT ∷ m (e ⨄ a)}
class MonadError e (m ∷ ★ → ★) | m → e where 
  errorE ∷ ErrorT e m ↝ m
  errorI ∷ m ↝ ErrorT e m

mapError ∷ (Functor m) ⇒ (e1 → e2) → ErrorT e1 m a → ErrorT e2 m a
mapError f = ErrorT ∘ mapLeft f ^∘ runErrorT

-- Effectors

effError ∷ (MonadError e m) ⇒ m (e ⨄ a) → m a  
effError = errorE ∘ ErrorT

throw ∷ (Monad m,MonadError e m) ⇒ e → m a            
throw e = effError $ return $ Left e

throwSum ∷ (Monad m,MonadError e m) ⇒ e ⨄ a → m a      
throwSum = elimSum throw return

throwMaybe ∷ (Monad m,MonadError e m) ⇒ e → Maybe a → m a 
throwMaybe e = elimMaybe (throw e) return

-- Observers

obsError ∷ (MonadError e m) ⇒ m a → m (e ⨄ a)  
obsError = runErrorT ∘ errorI

catch ∷ (Monad m,MonadError e m) ⇒ m a → (e → m a) → m a
catch aM h = do
  aeM ← runErrorT $ errorI aM
  case aeM of
    Left e → h e
    Right a → return a

-- # Reader

newtype ReaderT r m a = ReaderT {runReaderT ∷ r → m a}
class MonadReader r (m ∷ ★ → ★) | m → r where 
  readerE ∷ ReaderT r m ↝ m
  readerI ∷ m ↝ ReaderT r m

runReaderTWith ∷ r → ReaderT r m a → m a
runReaderTWith = flip runReaderT

-- Effectors

effReader ∷ (MonadReader r m) ⇒ (r → m a) → m a 
effReader = readerE ∘ ReaderT

ask ∷ (Monad m,MonadReader r m) ⇒ m r 
ask = effReader return

askL ∷ (Monad m,MonadReader r m) ⇒ Lens r a → m a 
askL l = access l ^$ ask

-- Observers

obsReader ∷ (MonadReader r m) ⇒ m a → (r → m a) 
obsReader = runReaderT ∘ readerI

-- Combos

local ∷ (MonadReader r m) ⇒ (r → r) → m a → m a 
local f aM = effReader $ obsReader aM ∘ f

localOn ∷ (MonadReader r m) ⇒ m a → (r → r) → m a
localOn = flip local

-- # Writer

newtype WriterT o m a = WriterT {runWriterT ∷ m (o,a)}
class MonadWriter o m | m → o where 
  writerE ∷ WriterT o m ↝ m
  writerI ∷ m ↝ WriterT o m

execWriterT ∷ (Functor m) ⇒ WriterT o m a → m o
execWriterT = fst ^∘ runWriterT

mapOutput ∷ (Functor m) ⇒ (o₁ → o₂) → WriterT o₁ m a → WriterT o₂ m a
mapOutput f = WriterT ∘ mapFst f ^∘ runWriterT

-- Effectors

effWriter ∷ (MonadWriter o m) ⇒ m (o,a) → m a 
effWriter = writerE ∘ WriterT

tell ∷ (Monad m,MonadWriter o m) ⇒ o → m () 
tell = effWriter ∘ return ∘ (,())

-- Observers

obsWriter ∷ (MonadWriter o m) ⇒ m a → m (o,a) 
obsWriter = runWriterT ∘ writerI

hijack ∷ (MonadWriter o m) ⇒ m a → m (o,a) 
hijack = obsWriter

-- # State

newtype StateT s m a = StateT {runStateT ∷ s → m (s,a)}
class MonadState s m | m → s where 
  stateE ∷ StateT s m ↝ m 
  stateI ∷ m ↝ StateT s m

runStateTWith ∷ s → StateT s m a → m (s, a)
runStateTWith = flip runStateT

evalStateTWith ∷ (Functor m) ⇒ s → StateT s m a → m a
evalStateTWith s = map snd ∘ runStateTWith s

execStateTWith ∷ (Functor m) ⇒ s → StateT s m a → m s
execStateTWith s = map fst ∘ runStateTWith s

mapStateT ∷ ∀ m s₁ s₂ a. (Functor m) ⇒ (s₁ → s₂) → (s₂ → s₁) → StateT s₁ m a → StateT s₂ m a
mapStateT to from aM = StateT $ \ s₂ → ff ^$ runStateT aM $ from s₂
  where 
    ff ∷ (s₁,a) → (s₂,a)
    ff (s₁, a) = (to s₁, a)

stateLens ∷ (Functor m) ⇒ Lens s₁ s₂ → StateT s₂ m ↝ StateT s₁ m
stateLens l aM = StateT $ \ s₁ →
  let s₂ = access l s₁
      ff (s₂', a) = (update l s₂' s₁, a)
  in ff ^$ runStateT aM s₂

-- Effectors

effState ∷ (MonadState s m) ⇒ (s → m (s,a)) → m a 
effState = stateE ∘ StateT

get ∷ (Monad m,MonadState s m) ⇒ m s 
get = stateE $ StateT $ \ s → return (s,s)

getL ∷ (Monad m,MonadState s m) ⇒ Lens s a → m a 
getL l = map (access l) get

put ∷ (Monad m,MonadState s m) ⇒ s → m () 
put s = stateE $ StateT $ \ _ → return (s,())

putL ∷ (Monad m,MonadState s m) ⇒ Lens s a → a → m () 
putL = modify ∘∘ update

modifyM ∷ (Monad m,MonadState s m) ⇒ (s → m s) → m () 
modifyM f = stateE $ StateT $ \ s → f s <×> return ()

modifyLM ∷ (Monad m,MonadState s m) ⇒ Lens s a → (a → m a) → m () 
modifyLM  = modifyM ∘∘ alterM

modify ∷ (Monad m,MonadState s m) ⇒ (s → s) → m () 
modify = modifyM ∘ kleisli

modifyL ∷ (Monad m,MonadState s m) ⇒ Lens s a → (a → a) → m () 
modifyL = modify ∘∘ alter

getAndPut ∷ (Monad m,MonadState s m) ⇒ s → m s
getAndPut s = do
  s' ← get
  put s
  return s'

getAndPutL ∷ (Monad m,MonadState s m) ⇒ Lens s a → a → m a
getAndPutL 𝓁 x = do
  x' ← getL 𝓁
  putL 𝓁 x
  return x'

next ∷ (Monad m,MonadState s m,Peano s) ⇒ m s
next = do
  i ← get
  put $ suc i
  return i

nextL ∷ (Monad m,MonadState s m,Peano a) ⇒ Lens s a → m a
nextL l = do
  i ← getL l
  putL l $ suc i
  return i

bump ∷ (Monad m,MonadState s m,Peano s) ⇒ m ()
bump = modify suc

bumpL ∷ (Monad m,MonadState s m,Peano a) ⇒ Lens s a → m ()
bumpL l = modifyL l suc

-- Observers

obsState ∷ (MonadState s m) ⇒ m a → (s → m (s,a)) 
obsState = runStateT ∘ stateI

localize ∷ (Monad m,MonadState s m) ⇒ m a → m (s,a)
localize aM = obsState aM *$ get

-- # Nondeterminism

newtype NondetT m a = NondetT { runNondetT ∷ m [a] }
class MonadNondet m where
  nondetE ∷ NondetT m ↝ m
  nondetI ∷ m ↝ NondetT m

-- Effectors

effNondet ∷ (MonadNondet m) ⇒ m [a] → m a
effNondet = nondetE ∘ NondetT

-- Observers

obsNondet ∷ (MonadNondet m) ⇒ m a → m [a]
obsNondet = runNondetT ∘ nondetI

-- # Cont

newtype ContT r m a = ContT {runContT ∷ (a → m r) → m r}
class MonadCont r m | m → r where 
  contE ∷ ContT r m ↝ m
  contI ∷ m ↝ ContT r m

evalContT ∷ (Monad m) ⇒ ContT r m r → m r
evalContT aM = runContT aM return

-- Effectors

effCont ∷ (MonadCont r m) ⇒ ((a → m r) → m r) → m a 
effCont = contE ∘ ContT

callCC ∷ (MonadCont r m) ⇒ ((a → m r) → m r) → m a 
callCC = effCont

-- Observers

obsCont ∷ (MonadCont r m) ⇒ m a → ((a → m r) → m r) 
obsCont = runContT ∘ contI

withC ∷ (MonadCont r m) ⇒ (a → m r) → m a → m r 
withC = flip obsCont

-- Combos

reset ∷ (Monad m,MonadCont r m) ⇒ m r → m r 
reset aM = callCC $ \ k → k *$ withC return aM

modifyC ∷ (Monad m,MonadCont r m) ⇒ (r → m r) → m a → m a 
modifyC f aM = callCC $ \ k → withC (f *∘ k) aM

-- # OpaqueCont

newtype OpaqueContT k r m a = OpaqueContT {runOpaqueContT ∷ k r m a → m r}
class MonadOpaqueCont k r m | m → k,m → r where 
  opaqueContE ∷ OpaqueContT k r m ↝ m
  opaqueContI ∷ m ↝ OpaqueContT k r m

newtype ContFun r m a = ContFun {runContFun ∷ a → m r}

runOpaqueContTWith ∷ k r m a → OpaqueContT k r m a → m r
runOpaqueContTWith = flip runOpaqueContT

evalOpaqueContT ∷ (Monad m,Isomorphism3 (ContFun r) (k r)) ⇒ OpaqueContT k r m r → m r
evalOpaqueContT aM = runMetaContT aM return

runMetaContT ∷ (Isomorphism3 (ContFun r) (k r)) ⇒ OpaqueContT k r m a → (a → m r) → m r
runMetaContT aM k = runOpaqueContT aM $ isoTo3 $ ContFun k

runMetaContTWith ∷ (Isomorphism3 (ContFun r) (k r)) ⇒ (a → m r) → OpaqueContT k r m a → m r
runMetaContTWith = flip runMetaContT

metaContT ∷ (Isomorphism3 (ContFun r) (k r)) ⇒ ((a → m r) → m r) → OpaqueContT k r m a
metaContT nk = OpaqueContT $ \ (k ∷ k r m a) → nk $ runContFun $ isoFrom3 k

meta ∷ (Isomorphism3 (ContFun r) (k r)) ⇒ OpaqueContT k r m ↝ ContT r m
meta aM = ContT $ \ (k ∷ a → m r) → runMetaContT aM k

opaque ∷ (Isomorphism3 (ContFun r) (k r)) ⇒ ContT r m ↝ OpaqueContT k r m
opaque aM = metaContT $ \ (k ∷ a → m r) → runContT aM k

-- Effectors

effOpaqueCont ∷ (MonadOpaqueCont k r m) ⇒ (k r m a → m r) → m a 
effOpaqueCont = opaqueContE ∘ OpaqueContT

callCCOpaque ∷ (MonadOpaqueCont k r m) ⇒ (k r m a → m r) → m a 
callCCOpaque = effOpaqueCont

-- Observers

obsOpaqueCont ∷ (MonadOpaqueCont k r m) ⇒ m a → (k r m a → m r) 
obsOpaqueCont = runOpaqueContT ∘ opaqueContI

withCOpaque ∷ (MonadOpaqueCont k r m) ⇒ k r m a → m a → m r 
withCOpaque  = flip obsOpaqueCont

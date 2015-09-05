module FP.Prelude.Lens where

import FP.Prelude.Core
import FP.Prelude.Morphism

-- Index

newtype Lens a b = Lens {runLens ∷ a → (b,b → a)}
data Prism a b = Prism {inject ∷ b → a,view ∷ a → Maybe b}

-- # Cursors (common for Lens and Prism)

class Index (t ∷ ★ → ★ → ★) where alter ∷ t a b → (b → b) → a → a
class IndexM (t ∷ ★ → ★ → ★) where alterM ∷ (Monad m) ⇒ t a b → (b → m b) → a → m a

update ∷ (Index t) ⇒ t a b → b → a → a
update 𝓁 x = alter 𝓁 $ const x

updateM ∷ (IndexM t,Monad m) ⇒ t a b → m b → a → m a
updateM 𝓁 xM = alterM 𝓁 $ const xM

-- ## Lens

instance Category Lens where
  refl = isoLens id id
  Lens g ⌾ Lens f = Lens $ \ a →
    let (b,ba) = f a
        (c,cb) = g b
    in (c,ba ∘ cb)
instance Index Lens where
  alter l f a = let (b,ba) = runLens l a in ba $ f b
instance IndexM Lens where
  alterM l f a = let (b,ba) = runLens l a in map ba $ f b

lens ∷ (a → b) → (a → b → a) → Lens a b
lens getter setter = Lens $ \ s → (getter s,setter s)

isoLens ∷ (a → b) → (b → a) → Lens a b
isoLens to from = lens to $ const from

access ∷ Lens a b → a → b
access = fst ∘∘ runLens

-- ## Prism

instance Category Prism where
  refl = isoPrism id id
  g ⌾ f = Prism
    { view = view g *∘ view f
    , inject = inject f ∘ inject g
    }
instance Index Prism where
  alter p f a = elimMaybe a (inject p ∘ f) $ view p a

prism ∷ (b → a) → (a → Maybe b) → Prism a b
prism = Prism

isoPrism ∷ (b → a) → (a → b) → Prism a b
isoPrism from to = prism from $ Just ∘ to

unsafeView ∷ Prism a b → a → b
unsafeView = elimMaybe (error "unsafeView") id ∘∘ view

shape ∷ Prism a b → a → 𝔹
shape = elimMaybe False (const True) ∘∘ view

leftL ∷ Prism (a ⨄ b) a
leftL = Prism Left $ elimSum Just $ const Nothing

rightR ∷ Prism (a ⨄ b) b
rightR = Prism Right $ elimSum (const Nothing) Just

fstL ∷ Lens (a,b) a
fstL = lens fst $ \ (_,b) → (,b)

sndL ∷ Lens (a,b) b
sndL = lens snd $ \ (a,_) → (a,)

nothingL ∷ Prism (Maybe a) ()
nothingL = prism (const Nothing) $ elimMaybe (Just ()) $ const Nothing

justL ∷ Prism (Maybe a) a
justL = Prism Just id

singleL ∷ Prism [a] a
singleL = Prism single $ \case
  [x] → Just x
  _ → Nothing


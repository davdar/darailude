module FP.Pretty where

import FP.Core
import FP.Free
import FP.Monads
import FP.Deriving.Lens
import FP.Deriving.Monoid

-- Setup {{{ --

newtype Color256 = Color256 { color256Raw :: Int }
  deriving (ToInteger)
instance FromInteger Color256 where
  fmn i | i >= 0 && i < 256 = Color256 $ fmn i
        | otherwise = error "Color256 values must be [0 <= n < 256]"

data Format = Format
  { foreground :: Maybe Color256
  , background :: Maybe Color256
  , underline :: Bool
  , bold :: Bool
  }
makeMonoid ''Format

setFG :: Color256 -> Format
setFG fg = null { foreground = Just fg }
setBG :: Color256 -> Format
setBG bg = null { background = Just bg }
setUL :: Format
setUL = null { underline = True }
setBD :: Format
setBD = null { bold = True }

data Chunk = Text String | Newline
type POut = FreeMonoidFunctor ((,) Format) Chunk

data Layout = Flat | Break
  deriving (Eq, Ord)
data Failure = CanFail | CantFail
  deriving (Eq, Ord)
data PEnv = PEnv
  { maxColumnWidth :: Int
  , maxRibbonWidth :: Int
  , layout :: Layout
  , failure :: Failure
  , nesting :: Int
  , level :: Int
  , bumped :: Bool
  }
makeLenses ''PEnv

env0 :: PEnv
env0 = PEnv
  { maxColumnWidth = toi 100
  , maxRibbonWidth = toi 60
  , layout = Break
  , failure = CantFail
  , nesting = toi 0
  , level = toi 0
  , bumped = False
  }

data PState = PState
  { column :: Int
  , ribbon :: Int
  }
makeLenses ''PState
state0 :: PState
state0 = PState
  { column = toi 0
  , ribbon = toi 0
  }

type MonadPretty m = (Monad m, MonadReader PEnv m, MonadWriter POut m, MonadState PState m, MonadMaybe m)

-- }}} ---

-- Low Level Interface {{{ --

text :: (MonadPretty m) => String -> m ()
text o = do
  tell $ unit $ Text o
  modifyL columnL $ (+) $ size o
  modifyL ribbonL $ (+) $ countNonSpace o
  f <- askL failureL
  when (f == CanFail) $ do
    cmax <- askL maxColumnWidthL
    rmax <- askL maxRibbonWidthL
    c <- getL columnL
    r <- getL ribbonL
    when (c > cmax) abort
    when (r > rmax) abort
  where
    countNonSpace = iter (cond isSpace id suc) (toi 0)

space :: (MonadPretty m) => Int -> m ()
space = text . flip iterAppend " "

ifFlat :: (MonadPretty m) => m a -> m a -> m a
ifFlat flatAction breakAction = do
  l <- askL layoutL
  case l of
    Flat -> flatAction
    Break -> breakAction

whenFlat :: (MonadPretty m) => m () -> m ()
whenFlat aM = ifFlat aM $ return ()

whenBreak :: (MonadPretty m) => m () -> m ()
whenBreak aM = ifFlat (return ()) aM

mustBreak :: (MonadPretty m) => m () -> m ()
mustBreak = (>>) $ whenFlat abort

hardLine :: (MonadPretty m) => m ()
hardLine = do
  tell $ unit $ Text "\n"
  putL columnL $ toi 0
  putL ribbonL $ toi 0

newline :: (MonadPretty m) => m ()
newline = do
  n <- askL nestingL
  hardLine
  space n

flat :: (MonadPretty m) => m a -> m a
flat = localSetL layoutL Flat

canFail :: (MonadPretty m) => m a -> m a
canFail = localSetL failureL CanFail

nest :: (MonadPretty m) => Int -> m a -> m a
nest = localL nestingL . (+)

group :: (MonadMaybe m, MonadPretty m) => m a -> m a
group aM = ifFlat aM $ (flat . canFail) aM <|> aM

align :: (MonadPretty m) => m a -> m a
align aM = do
  i <- askL nestingL
  c <- getL columnL
  nest (c - i) aM

format :: (MonadPretty m) => Format -> m a -> m a
format f aM = do
  (o, a) <- hijack aM
  tell $ MFApply (f, o)
  return a

-- }}} --

-- High Level Helpers {{{

hsep :: (MonadPretty m) => [m ()] -> m ()
hsep = exec . intersperse (space $ toi 1)

vsep :: (MonadPretty m) => [m ()] -> m ()
vsep = exec . intersperse newline

hvsep :: (MonadPretty m) => [m ()] -> m ()
hvsep = group . exec . intersperse (ifFlat (space $ toi 1) newline)

hsepTight :: (MonadPretty m) => [m ()] -> m ()
hsepTight = exec . intersperse (ifFlat (return ()) (space $ toi 1))

hvsepTight :: (MonadPretty m) => [m ()] -> m ()
hvsepTight = group . exec . intersperse (ifFlat (return ()) newline)

botLevel :: (MonadPretty m) => m () -> m ()
botLevel = local $ set levelL (toi 0) . set bumpedL False

closed :: (MonadPretty m) => m () -> m () -> m () -> m ()
closed alM arM aM = do
  alM
  botLevel $ aM
  arM

parens :: (MonadPretty m) => m () -> m ()
parens = closed (text "(") (text ")") . align

atLevel :: (MonadPretty m) => Int -> m () -> m ()
atLevel i' aM = do
  i <- askL levelL 
  b <- askL bumpedL
  if i < i' || (i == i' && not b)
    then local (set levelL i' . set bumpedL False) aM
    else parens aM

bump :: (MonadPretty m) => m a -> m a
bump = local $ set bumpedL True

inf :: (MonadPretty m) => Int -> m () -> m () -> m () -> m ()
inf i oM x1M x2M = atLevel i $ bump x1M >> space (toi 1) >> oM >> space (toi 1) >> bump x2M

infl :: (MonadPretty m) => Int -> m () -> m () -> m () -> m ()
infl i oM x1M x2M = atLevel i $ x1M >> space (toi 1) >> oM >> space (toi 1) >> bump x2M

infr :: (MonadPretty m) => Int -> m () -> m () -> m () -> m ()
infr i oM x1M x2M = atLevel i $ bump x1M >> space (toi 1) >> oM >> space (toi 1) >> x2M

pre :: (MonadPretty m) => Int -> m () -> m () -> m ()
pre i oM xM = atLevel i $ oM >> space (toi 1) >> xM

post :: (MonadPretty m) => Int -> m () -> m () -> m ()
post i oM xM = atLevel i $ xM >> space (toi 1) >> oM

app :: (MonadPretty m) => m () -> [m ()] -> m ()
app x xs = atLevel (toi 100) $ hvsep $ map (atLevel $ toi 100) $ x : map (align . bump) xs

collection :: (MonadPretty m) => String -> String -> String -> [m ()] -> m ()
collection open close _   []     = pun open >> pun close
collection open close sep (x:xs) = group $ hvsepTight $ concat
  [ single $ hsepTight [pun open, botLevel $ align x]
  , mapOn xs $ \ x' -> hsepTight [pun sep, botLevel $ align x']
  , single $ pun close
  ]

keyPunFmt :: Format
keyPunFmt = setFG (fmn 3) ++ setBD

keyPun :: (MonadPretty m) => String -> m ()
keyPun = format keyPunFmt . text

keyFmt :: Format
keyFmt = keyPunFmt ++ setUL

key :: (MonadPretty m) => String -> m ()
key = format keyFmt . text

conFmt :: Format
conFmt = setFG (fmn 22) ++ setBD

con :: (MonadPretty m) => String -> m ()
con = format conFmt . text

bdrFmt :: Format
bdrFmt = setFG (fmn 6)

bdr :: (MonadPretty m) => String -> m ()
bdr = format bdrFmt . text

litFmt :: Format
litFmt = setFG (fmn 1) -- ++ setBD

lit :: (MonadPretty m) => String -> m ()
lit = format litFmt . text

punFmt :: Format
punFmt = setFG (fmn 8)

pun :: (MonadPretty m) => String -> m ()
pun = format punFmt . text

hlFmt :: Format
hlFmt = setBG (fmn 229)

hl :: (MonadPretty m) => String -> m ()
hl = format hlFmt . text

headingFmt :: Format
headingFmt = setFG (fmn 5) ++ setBD ++ setUL

heading :: (MonadPretty m) => String -> m ()
heading = format headingFmt . text

op :: (MonadPretty m) => String -> m ()
op = format (setFG $ fmn 4) . text

-- }}}

-- DocM {{{

newtype DocM a = DocM { unDocM :: RWST PEnv POut PState Maybe a }
  deriving 
    ( Unit, Functor, Product, Applicative, Bind, Monad
    , MonadReader PEnv, MonadWriter POut, MonadState PState, MonadMaybe
    )
runDocM :: PEnv -> PState -> DocM a -> Maybe (PState, POut, a)
runDocM e s = runRWST e s . unDocM

execDoc :: Doc -> POut
execDoc d =
  let rM = runDocM env0 state0 d
  in case rM of
    Nothing -> MonoidFunctorElem $ Text "<internal pretty printing error>"
    Just (_, o, ()) -> o

type Doc = DocM ()

instance Monoid Doc where
  null = return ()
  (++) = (>>)

class Pretty a where
  pretty :: a -> Doc
instance Pretty Doc where
  pretty = id

class PrettyM m a where
  prettyM :: a -> m Doc

-- }}}

-- No Format {{{

formatChunk :: Chunk -> String
formatChunk (Text s) = s
formatChunk Newline = "\n"

noFormatOut :: POut -> String
noFormatOut (MonoidFunctorElem o) = formatChunk o
noFormatOut MFNull = ""
noFormatOut (o1 :+++: o2) = noFormatOut o1 ++ noFormatOut o2
noFormatOut (MFApply (_, o)) = noFormatOut o

ptoString :: (Pretty a) => a -> String
ptoString = noFormatOut . execDoc . pretty

-- }}}

-- Instances {{{

instance Pretty Bool    where pretty = con . show
instance Pretty Int     where pretty = lit . show
instance Pretty Integer where pretty = lit . show
instance Pretty Char    where pretty = lit . show
instance Pretty String  where pretty = lit . show
instance Pretty Double  where pretty = lit . show
instance Pretty ()      where pretty () = con "()"

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = collection "(" ")" "," [pretty a, pretty b]
instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = collection "(" ")" "," [pretty a, pretty b, pretty c]
instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pretty (a, b, c, d) = collection "(" ")" "," [pretty a, pretty b, pretty c, pretty d]
instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) => Pretty (a, b, c, d, e) where
  pretty (a, b, c, d, e) = collection "(" ")" "," [pretty a, pretty b, pretty c, pretty d, pretty e]
instance Bifunctorial Pretty (,) where bifunctorial = W
instance (Pretty a, Pretty b) => Pretty (a :+: b) where
  pretty (Inl a) = app (con "Inl") [pretty a]
  pretty (Inr b) = app (con "Inr") [pretty b]
instance (Pretty a) => Pretty (Maybe a) where
  pretty (Just a) = pretty a
  pretty Nothing = con "Nothing"

instance (Pretty a) => Pretty [a] where pretty = collection "[" "]" "," . map pretty
instance Functorial Pretty [] where functorial = W

instance (Pretty a) => Pretty (Set a) where pretty = collection "{" "}" "," . map pretty . fromSet
instance Functorial Pretty Set where functorial = W

instance (Pretty a) => Pretty (SetWithTop a) where
  pretty SetTop = con "⊤"
  pretty (SetNotTop xs) = pretty xs

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty = collection "{" "}" "," . map prettyMapping . fromMap
    where
      prettyMapping (k, v) = nest (toi 2) $ hvsep [nest (toi $ -2) $ hsep [pretty k, pun "=>"], pretty v]
instance (Ord a, Pretty a) => Pretty (ListSet a) where pretty = pretty . toSet
instance (Ord a, Pretty a) => Pretty (ListSetWithTop a) where pretty = pretty . setFromListWithTop

instance (Functorial Pretty f) => Pretty (Fix f) where
  pretty (Fix f) =
    with (functorial :: W (Pretty (f (Fix f)))) $
    pretty f

instance (Pretty a, Pretty f) => Pretty (Stamped a f) where
  pretty (Stamped a f) = 
    -- pretty f
    atLevel (toi 0) $ exec [pretty a, pun ":", pretty f]

instance (Pretty a, Functorial Pretty f) => Pretty (StampedFix a f) where
  pretty (StampedFix a f) = 
    with (functorial :: W (Pretty (f (StampedFix a f)))) $ 
    -- pretty f
    atLevel (toi 0) $ exec [bump $ pretty a, pun ":", pretty f]

instance (Pretty a) => Pretty (ID a) where pretty (ID a) = pretty a
instance Functorial Pretty ID where functorial = W

instance (Functorial Pretty m, Pretty e, Pretty a) => Pretty (ErrorT e m a) where
  pretty (ErrorT aM) =
    with (functorial :: W (Pretty (m (e :+: a)))) $
    pretty aM

instance (Functorial Pretty m, Pretty a) => Pretty (ListT m a) where
  pretty (ListT aM) =
    with (functorial :: W (Pretty (m [a]))) $
    pretty aM

instance (Pretty a) => Pretty (SumOfProd a) where
  pretty = 
    collection "{" "}" "⊔"
    . map (collection "{" "}" "⊓")
    . map (map pretty . toList) . toList
    . unSumOfProd
    
-- }}}

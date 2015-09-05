module FP.Pretty.Pretty where

import FP.Prelude
import FP.Pretty.Color

-- # Format

data Format = 
    FormatFG Color
  | FormatBG Color
  | FormatUL
  | FormatBD
  deriving (Eq, Ord)

-- # PrettyEnv

data Layout = Flat | Break
  deriving (Eq,Ord)

data FailureMode = CanFail | CantFail
  deriving (Eq,Ord)

data PrettyParams = PrettyParams
  { punctuationFormat        ∷ [Format]
  , keywordPunctuationFormat ∷ [Format]
  , keywordFormat            ∷ [Format]
  , constructorFormat        ∷ [Format]
  , operatorFormat           ∷ [Format]
  , binderFormat             ∷ [Format]
  , literalFormat            ∷ [Format]
  , highlightFormat          ∷ [Format]
  , headerFormat             ∷ [Format]
  , errorFormat              ∷ [Format]
  , appLevel                 ∷ ℕ
  }
makeLenses ''PrettyParams

prettyParams₀ ∷ PrettyParams
prettyParams₀ = PrettyParams
  { punctuationFormat        = [FormatFG darkGray]
  , keywordPunctuationFormat = [FormatFG darkYellow,FormatBD]
  , keywordFormat            = [FormatFG darkYellow,FormatBD,FormatUL]
  , constructorFormat        = [FormatFG darkGreen,FormatBD]
  , operatorFormat           = [FormatFG darkBlue]
  , binderFormat             = [FormatFG darkTeal]
  , literalFormat            = [FormatFG darkRed]
  , highlightFormat          = [FormatBG highlight]
  , headerFormat             = [FormatFG darkPink,FormatBD,FormatUL]
  , errorFormat              = [FormatFG white,FormatBG darkRed]
  , appLevel                 = 𝕟 100
  }

data PrettyEnv = PrettyEnv
  { prettyParams ∷ PrettyParams
  , maxColumnWidth ∷ ℕ
  , maxRibbonWidth ∷ ℕ
  , layout ∷ Layout
  , failureMode ∷ FailureMode
  , nesting ∷ ℕ
  , level ∷ ℕ
  , bumped ∷ 𝔹
  , undertagMode ∷ Maybe (ℂ,Color)
  , doOutput ∷ 𝔹
  , doFormat ∷ 𝔹
  , doLineNumbers ∷ 𝔹
  , lineNumberDisplayWidth ∷ ℕ
  , formats ∷ [Format]
  , blinders ∷ Maybe (ℕ,ℕ)
  }
makeLenses ''PrettyEnv

prettyEnv₀ ∷ PrettyEnv
prettyEnv₀ = PrettyEnv
  { prettyParams = prettyParams₀
  , maxColumnWidth = 𝕟 100
  , maxRibbonWidth = 𝕟 60
  , layout = Break
  , failureMode = CantFail
  , nesting = 𝕟 0
  , level = 𝕟 0
  , bumped = False
  , undertagMode = Nothing
  , doOutput = True
  , doFormat = True
  , doLineNumbers = False
  , lineNumberDisplayWidth = 𝕟 2
  , formats = []
  , blinders = Nothing
  }

-- # PrettyOut

data Chunk = Text 𝕊 | Newline
  deriving (Eq, Ord)
data PrettyOut = 
    ChunkOut Chunk 
  | FormatOut [Format] PrettyOut
  | NullOut 
  | AppendOut PrettyOut PrettyOut
  deriving (Eq, Ord)
instance Monoid PrettyOut where
  null = NullOut
  (⧺) = AppendOut

-- # PrettyState

data PrettyState = PrettyState
  { column ∷ ℕ
  , ribbon ∷ ℕ
  , beginningOfLine ∷ 𝔹
  , lineNumber ∷ ℕ
  , undertags ∷ [(ℕ,ℕ,ℂ,Color)]
  }
makeLenses ''PrettyState

prettyState₀ ∷ PrettyState
prettyState₀ = PrettyState
  { column = 𝕟 0
  , ribbon = 𝕟 0
  , beginningOfLine = True
  , lineNumber = 𝕟 0
  , undertags = []
  }

-- # PrettyM

newtype PrettyM a = PrettyM { runPrettyM ∷ RWST PrettyEnv PrettyOut PrettyState Maybe a }
  deriving
  ( Functor,Monad
  , MonadReader PrettyEnv,MonadWriter PrettyOut,MonadState PrettyState
  , MonadFailure
  )

runPrettyMWith ∷ PrettyEnv → PrettyState → PrettyM a → Maybe (PrettyState,PrettyOut,a)
runPrettyMWith r s aM = runRWSTWith r s $ runPrettyM aM

execOutPrettyMWith ∷ PrettyEnv → PrettyState → PrettyM a → Maybe PrettyOut
execOutPrettyMWith r s aM = do
  (_,o,_) ← runPrettyMWith r s aM
  return o

-- # Doc

newtype Doc = Doc { runDoc ∷ PrettyM () }
instance Eq Doc where
  (==) = (==) `on` (renderDoc ∘ ppFinal ∘ pretty)
instance Ord Doc where
  compare = compare `on` (renderDoc ∘ ppFinal ∘ pretty)
instance Monoid Doc where
  null = Doc $ return ()
  x ⧺ y = Doc $ runDoc x ≫ runDoc y

renderDoc ∷ Doc → PrettyOut
renderDoc aM =
  let errOut = FormatOut (errorFormat prettyParams₀) $ ChunkOut $ Text "<internal pretty printing error>"
  in ifNothing errOut $ execOutPrettyMWith prettyEnv₀ prettyState₀ $ runDoc aM

-- # Class

class Pretty a where
  pretty ∷ a → Doc

-- # Low-Level Interface

shouldOutputM ∷ PrettyM 𝔹
shouldOutputM = do
  ln ← getL lineNumberL
  bldrs ← askL blindersL
  outP ← askL doOutputL
  return $
    let inBlds = case bldrs of
          Nothing → True
          Just (low,high) → low ≤ ln ∧ ln ≤ high
    in outP ∧ inBlds

shouldOutputNewlineM ∷ PrettyM 𝔹
shouldOutputNewlineM = do
  so ← shouldOutputM
  ln ← getL lineNumberL
  bldrs ← askL blindersL
  uts ← getL undertagsL
  return $ so ∧ (case bldrs of {Nothing → True;Just (_,high) → ln < high} ∨ not (isEmpty uts))

  
-- ⟬s⟭ should contain no newlines
ppllSpit ∷ 𝕊 → PrettyM ()
ppllSpit s 
  | isEmpty s = return ()
  | otherwise = do
      fmtB ← askL doFormatL
      fmts ← askL formatsL
      let fmtF = if fmtB ∧ not (isEmpty fmts) then FormatOut fmts else id
      whenM shouldOutputM $ tell $ fmtF $ ChunkOut $ Text s
      modifyL columnL $ (+) $ length s
      modifyL ribbonL $ (+) $ countNonSpace s
      f ← askL $ failureModeL
      when (f == CanFail) $ do
        cmax ← askL $ maxColumnWidthL
        rmax ← askL $ maxRibbonWidthL
        c ← getL columnL
        r ← getL ribbonL
        when (c > cmax) abort
        when (r > rmax) abort
  where
    countNonSpace ∷ 𝕊 → ℕ
    countNonSpace = iter (\ c → if isSpace c then id else suc) (𝕟 0) ∘ stream

ppllFormat ∷ [Format] → PrettyM () → PrettyM ()
ppllFormat f = local (alter formatsL (f ⧺))

ppllNoFormat ∷ PrettyM () → PrettyM ()
ppllNoFormat = local (update doFormatL False)

ppllClearFormat ∷ PrettyM () → PrettyM ()
ppllClearFormat = local (update formatsL [])

ppllNewline ∷ PrettyM ()
ppllNewline = ppllNoFormat $ do
  whenM shouldOutputNewlineM $ tell $ ChunkOut Newline
  putL beginningOfLineL True
  putL columnL $ 𝕟 0
  putL ribbonL $ 𝕟 0

-- ⟬s⟭ should be non-empty and contain no newlines
ppllString ∷ 𝕊 → PrettyM ()
ppllString s = do
  ppllClearFormat $ whenM (getL beginningOfLineL) $ do
    whenM (askL doLineNumbersL) $ do
      ln ← getL lineNumberL
      w ← askL lineNumberDisplayWidthL
      ppllFormat [FormatFG darkGray] $ ppllSpit $ alignRight w (𝕤 $ show ln) ⧺ ": "
    n ← askL nestingL 
    ppllSpit $ appendN n " "
    putL beginningOfLineL False
  col ← getL columnL
  ppllSpit s
  col' ← getL columnL
  whenMaybeM (askL undertagModeL) $ \ (c,o) → do
    modifyL undertagsL $ (:) (col,col' - (col ⊓ col'),c,o)

ppllUndertags ∷ PrettyM ()
ppllUndertags = ppllClearFormat $ do
  uts ← list ∘ reverse ^$ getL undertagsL
  when (not $ isEmpty uts) $ do
    ppllNewline
    foreachOn uts $ \ (utcol,len,c,o) → do
      col ← getL columnL
      let diff = utcol - (col ⊓ utcol)
      ppllSpit $ 𝕤 $ replicate diff ' '
      ppllFormat [FormatFG o] $ ppllSpit $ 𝕤 $ replicate len c
  putL undertagsL []

ppllLineBreak ∷ PrettyM ()
ppllLineBreak = do
  ppllUndertags
  ppllNewline
  modifyL lineNumberL $ (+ 𝕟 1)

ppllText ∷ 𝕊 → PrettyM ()
ppllText s =
  let (s',snl) = prefixUntil (== '\n') $ list s
  in if not $ isEmpty s'
    then ppllString (𝕤 s') ≫ ppllText (𝕤 snl)
    else case uncons snl of
      Nothing → return ()
      Just ('\n',snl') → ppllLineBreak ≫ ppllText (𝕤 snl')
      Just _ → error $ "<internal error> ppText"

-- # Mid-Level Interface
  
ppFinal ∷ Doc → Doc
ppFinal d = Doc $ do
  runDoc d
  ppllUndertags

ppText ∷ 𝕊 → Doc
ppText = Doc ∘ ppllText

ppFormat ∷ [Format] → Doc → Doc
ppFormat f = Doc ∘ ppllFormat f ∘ runDoc

ppSpace ∷ ℕ → Doc
ppSpace n = ppText $ 𝕤 $ replicate n ' '

ppNewline ∷ Doc
ppNewline = ppText "\n"

ppIfFlat ∷ Doc → Doc → Doc
ppIfFlat flatAction breakAction = Doc $ do
  l ← askL $ layoutL
  runDoc $ case l of
    Flat → flatAction
    Break → breakAction

ppFlat ∷ Doc → Doc
ppFlat = Doc ∘ local (update layoutL Flat) ∘ runDoc

ppCanFail ∷ Doc → Doc
ppCanFail = Doc ∘ local (update failureModeL CanFail) ∘ runDoc

ppGroup ∷ Doc → Doc
ppGroup x  = ppIfFlat x $ Doc $ tries 
  [ runDoc $ ppFlat $ ppCanFail x
  , runDoc x
  ]

ppNest ∷ ℕ → Doc → Doc
ppNest n = Doc ∘ local (alter nestingL (+ n)) ∘ runDoc

ppAlign ∷ Doc → Doc
ppAlign aM = Doc $ do
  i ← askL $ nestingL
  c ← getL columnL
  runDoc $ ppNest (c - (i ⊓ c)) aM

ppLength ∷ Doc → ℕ
ppLength d = case runPrettyMWith prettyEnv₀ prettyState₀ $ runDoc d of
  Nothing → 𝕟 0
  Just (s,_,()) → column s

-- # Formatting Helpers

paramFormat ∷ (Lens PrettyParams [Format]) → 𝕊 → Doc
paramFormat l s = Doc $ do
  fmt ← askL $ l ⌾ prettyParamsL
  runDoc $ ppFormat fmt $ ppText s

ppNoFormat ∷ Doc → Doc
ppNoFormat = Doc ∘ local (update doFormatL False) ∘ runDoc

ppLineNumbers ∷ Doc → Doc
ppLineNumbers = Doc ∘ local (update doLineNumbersL True) ∘ runDoc

ppBlinders ∷ ℕ → ℕ → Doc → Doc
ppBlinders low high = Doc ∘ local (update blindersL $ Just (low,high)) ∘ runDoc

ppSetLineNumber ∷ ℕ → Doc → Doc
ppSetLineNumber n d = Doc $ do
  l ← getL lineNumberL
  putL lineNumberL n 
  runDoc d
  putL lineNumberL l

ppFG ∷ Color → Doc → Doc
ppFG c = ppFormat [FormatFG c]

ppBG ∷ Color → Doc → Doc
ppBG c = ppFormat [FormatBG c]

ppUL ∷ Doc → Doc
ppUL = ppFormat [FormatUL]

ppBD ∷ Doc → Doc
ppBD = ppFormat [FormatBD]

ppPun ∷ 𝕊 → Doc
ppPun = paramFormat punctuationFormatL

ppKeyPun ∷ 𝕊 → Doc
ppKeyPun = paramFormat keywordPunctuationFormatL

ppKey ∷ 𝕊 → Doc
ppKey = paramFormat keywordFormatL

ppCon ∷ 𝕊 → Doc
ppCon = paramFormat constructorFormatL

ppOp ∷ 𝕊 → Doc
ppOp = paramFormat operatorFormatL

ppBdr ∷ 𝕊 → Doc
ppBdr = paramFormat binderFormatL

ppLit ∷ 𝕊 → Doc
ppLit = paramFormat literalFormatL

ppHl ∷ 𝕊 → Doc
ppHl = paramFormat highlightFormatL

ppHeader ∷ 𝕊 → Doc
ppHeader = paramFormat headerFormatL

ppErr ∷ 𝕊 → Doc
ppErr = paramFormat errorFormatL

ppUT ∷ ℂ → Color → Doc → Doc
ppUT c o = Doc ∘ local (update undertagModeL $ Just (c,o)) ∘ runDoc

ppAlignLeft ∷ ℕ → Doc → Doc
ppAlignLeft n d = 
  let len = ppLength d
  in case n ⋚ len of
    LT → d
    EQ → d
    GT → d ⧺ ppSpace (n - (len ⊓ n))

ppAlignRight ∷ ℕ → Doc → Doc
ppAlignRight n d =
  let len = ppLength d
  in case n ⋚ len of
    LT → d
    EQ → d
    GT → ppSpace (n - (len ⊓ n)) ⧺ d

-- # High Level Helpers

ppHorizontal ∷ [Doc] → Doc
ppHorizontal = concat ∘ intersperse (ppSpace $ 𝕟 1) ∘ map ppAlign

ppVertical ∷ [Doc] → Doc
ppVertical = concat ∘ intersperse ppNewline ∘ map ppAlign

ppBreak ∷ Doc
ppBreak = ppIfFlat (ppSpace $ 𝕟 1) ppNewline

ppSeparated ∷ [Doc] → Doc
ppSeparated = ppGroup ∘ concat ∘ intersperse ppBreak ∘ map ppAlign

ppBotLevel ∷ Doc → Doc
ppBotLevel = Doc ∘ local (update levelL (𝕟 0) ∘ update bumpedL False) ∘ runDoc

ppClosed ∷ Doc → Doc → Doc → Doc
ppClosed alM arM aM = concat $ map ppAlign
  [ alM
  , ppBotLevel aM
  , arM
  ]

ppParens ∷ Doc → Doc
ppParens = ppClosed (ppPun "(") (ppPun ")")

ppAtLevel ∷ ℕ → Doc → Doc
ppAtLevel i' aM = Doc $ do
  i ← askL $ levelL
  b ← askL $ bumpedL
  if (i < i') ∨ ((i == i') ∧ not b)
    then local (update levelL i' ∘ update bumpedL False) $ runDoc aM
    else runDoc $ ppParens aM

ppBump ∷ Doc → Doc
ppBump = Doc ∘ local (update bumpedL True) ∘ runDoc

ppInf ∷ ℕ → Doc → Doc → Doc → Doc
ppInf i oM x1M x2M = ppGroup $ ppAtLevel i $ ppSeparated [ppBump x1M,oM,ppBump x2M]

ppInfl ∷ ℕ → Doc → Doc → Doc → Doc
ppInfl i oM x1M x2M = ppGroup $ ppAtLevel i $ ppSeparated [x1M,oM,ppBump x2M]

ppInfr ∷ ℕ → Doc → Doc → Doc → Doc
ppInfr i oM x1M x2M = ppGroup $ ppAtLevel i $ ppSeparated [ppBump x1M,oM,x2M]

ppPre ∷ ℕ → Doc → Doc → Doc
ppPre i oM xM = ppGroup $ ppAtLevel i $ ppSeparated [oM,xM]

ppPost ∷ ℕ → Doc → Doc → Doc
ppPost i oM xM = ppGroup $ ppAtLevel i $ ppSeparated [xM,oM]

ppApp ∷ Doc → [Doc] → Doc
ppApp x xs = ppGroup $ Doc $ do
  l ← askL $ appLevelL ⌾ prettyParamsL
  runDoc $ ppAtLevel l $ ppSeparated $ ppAtLevel l x : map (ppAtLevel l ∘ ppBump) xs

ppCollectionAtLevel ∷ ℕ → 𝕊 → 𝕊 → 𝕊 → [Doc] → Doc
ppCollectionAtLevel i open close sep xs = ppGroup $ ppBotLevel $ ppAtLevel i $ ppIfFlat flatCollection breakCollection
  where
    flatCollection = concat [ppPun open,concat $ intersperse (ppPun sep) xs,ppPun close]
    breakCollection = ppVertical $ list $ concat
      [ mapHead (\ x → ppHorizontal [ppPun open,x]) $ mapTail (\ x → ppHorizontal [ppPun sep,x]) xs
      , return $ ppPun close
      ]

ppCollection ∷ 𝕊 → 𝕊 → 𝕊 → [Doc] → Doc
ppCollection = ppCollectionAtLevel $ 𝕟 0

-- # NoFormat

renderChunk ∷ Chunk → 𝕊
renderChunk (Text s) = s
renderChunk Newline = "\n"

renderNoFormat ∷ PrettyOut → 𝕊
renderNoFormat (ChunkOut c) = renderChunk c
renderNoFormat (FormatOut _ o) = renderNoFormat o
renderNoFormat NullOut = ""
renderNoFormat (AppendOut o₁ o₂) = renderNoFormat o₁ ⧺ renderNoFormat o₂

ppString ∷ (Pretty a) ⇒ a → 𝕊
ppString = renderNoFormat ∘ renderDoc ∘ ppFinal ∘ pretty

-- # Instances

instance Pretty Doc where 
  pretty = id

instance Pretty 𝔹 where pretty = ppCon ∘ 𝕤 ∘ show
instance Pretty 𝕀 where pretty = ppLit ∘ 𝕤 ∘ show
instance Pretty ℤ where pretty = ppLit ∘ 𝕤 ∘ show
instance Pretty ℕ where pretty = ppLit ∘ 𝕤 ∘ show
instance Pretty ℂ where pretty = ppLit ∘ 𝕤 ∘ show
instance Pretty 𝕊 where pretty = ppLit ∘ 𝕤 ∘ show
instance Pretty 𝔻  where pretty = ppLit ∘ 𝕤 ∘ show
instance Pretty () where pretty = ppCon ∘ 𝕤 ∘ show

instance (Pretty a, Pretty b) ⇒ Pretty (a, b) where
  pretty (a, b) = ppCollection "(" ")" "," [pretty a, pretty b]
instance (Pretty a, Pretty b, Pretty c) ⇒ Pretty (a, b, c) where
  pretty (a, b, c) = ppCollection "(" ")" "," [pretty a, pretty b, pretty c]

instance (Pretty a,Pretty b) ⇒ Pretty (a ⨄ b) where
  pretty (Left a) = ppApp (ppCon "Left") [pretty a]
  pretty (Right b) = ppApp (ppCon "Right") [pretty b]

instance (Pretty a) ⇒ Pretty (Stream a) where pretty xs = ppApp (ppText "stream") [pretty $ list xs]
instance (Pretty a) ⇒ Pretty [a] where pretty = ppCollection "[" "]" "," ∘ map pretty
instance (Pretty a) ⇒ Pretty (𝒫 a) where pretty = ppCollection "{" "}"  "," ∘ map pretty ∘ list
instance (Pretty k,Pretty v) ⇒ Pretty (k ⇰ v) where 
  pretty = ppCollection "{" "}" "," ∘ map prettyMapping ∘ list
    where
      prettyMapping (k,v) = ppNest (𝕟 2) $ ppSeparated [pretty k,ppPun "↦",pretty v]

instance (Pretty a) ⇒ Pretty (Maybe a) where
  pretty Nothing = ppCon "Nothing"
  pretty (Just x) = ppApp (ppCon "Just") [pretty x]

-- Tests

testPrettyFormats ∷ Doc
testPrettyFormats = ppVertical
  [ ppPun "punctuation"
  , ppKeyPun "keyword punctuation"
  , ppKey "keyword"
  , ppCon "constructor"
  , ppOp "operator"
  , ppBdr "binder"
  , ppLit "literal"
  , ppHl "highlighted"
  , ppHeader "header"
  , ppErr "error"
  ]

testPrettyNesting ∷ Doc
testPrettyNesting = ppVertical
  [ pretty $
      [ dict [ (111, set [10000,11111,22222,33333,44444,55555,66666,77777])
             , (222, set [10000,11111,22222,33333,44444,55555,66666,77777,88888])
             , (333, set [10000,11111,22222,33333,44444,55555,66666,77777,88888,99999])
             ]
      ]
  ]

testPrettyUndertags ∷ Doc
testPrettyUndertags = ppVertical
  [ ppText "not undertaggedd"
  , ppUT '~' green $ ppText "undertagged green"
  , ppUT '^' blue $ ppVertical
      [ ppText "multiline"
      , ppText "undertagged"
      , ppFG darkPink $ ppText "with color inside"
      ]
  ]

testPrettyLineNumbers ∷ Doc
testPrettyLineNumbers = ppVertical
  [ ppLineNumbers $ ppText "show lines"
  , ppText "don't show lines"
  , ppLineNumbers $ ppVertical
      [ ppText "multiline"
      , ppText "show lines"
      ]
  ]

testPrettyBlinders ∷ Doc
testPrettyBlinders = 
  let lines ∷ [Doc]
      lines = list $ map (\ (i,p) → ppHorizontal [p,ppNoFormat $ pretty i]) $ list $ withIndex $ stream $ replicate (𝕟 30) (ppText "line number")
  in ppLineNumbers $ ppBlinders (𝕟 10) (𝕟 20) $ ppVertical $ lines

-- testNesting ∷ PrettyM ()

-- Console

-- 
-- newtype DocM a = DocM { unDocM ∷ RWST PEnv POut PState Maybe a }
--   deriving 
--     ( Unit, Functor, Product, Applicative, Bind, Monad
--     , MonadReader PEnv, MonadWriter POut, MonadState PState, MonadFailure
--     )
-- runDocM ∷ PEnv → PState → DocM a → Maybe (PState, POut, a)
-- runDocM e s = runRWST e s ∘ unDocM
-- 
-- type Doc = DocM ()
-- 
-- instance Monoid Doc where
--   null = return ()
--   (⧺) = (>>)
-- 
-- instance Pretty Doc where
--   pretty = id
-- 
-- class PrettyM m a where
--   prettyM ∷ a → m Doc
-- 
-- -- }}}
-- 
-- -- No Format {{{
-- 
-- -- }}}
-- 
-- -- Instances {{{
-- 
-- instance (Pretty a, Pretty b, Pretty c, Pretty d) ⇒ Pretty (a, b, c, d) where
--   pretty (a, b, c, d) = collection "(" ")" "," [pretty a, pretty b, pretty c, pretty d]
-- instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) ⇒ Pretty (a, b, c, d, e) where
--   pretty (a, b, c, d, e) = collection "(" ")" "," [pretty a, pretty b, pretty c, pretty d, pretty e]
-- instance Bifunctorial Pretty (,) where bifunctorial = W
-- instance (Pretty a, Pretty b) ⇒ Pretty (a :+: b) where
--   pretty (Left a) = app (con "Left") [pretty a]
--   pretty (Right b) = app (con "Right") [pretty b]
-- instance (Pretty a) ⇒ Pretty (Maybe a) where
--   pretty (Just a) = pretty a
--   pretty Nothing = con "Nothing"
-- 
-- instance (Pretty a) ⇒ Pretty (Set a) where pretty = collection "{" "}" "," ∘ map pretty ∘ fromSet
-- instance Functorial Pretty Set where functorial = W
-- 
-- instance (Pretty a) ⇒ Pretty (SetWithTop a) where
--   pretty SetTop = con "⊤"
--   pretty (SetNotTop xs) = pretty xs
-- 
-- instance (Pretty k, Pretty v) ⇒ Pretty (Map k v) where
--   pretty = collection "{" "}" "," ∘ map prettyMapping ∘ fromMap
--     where
--       prettyMapping (k, v) = ppNest (𝕟 2) $ hvsep [ppNest (𝕟 $ -2) $ hsep [pretty k, pun "⇒"], pretty v]
-- instance (Ord a, Pretty a) ⇒ Pretty (ListSet a) where pretty = pretty ∘ toSet
-- instance (Ord a, Pretty a) ⇒ Pretty (ListSetWithTop a) where pretty = pretty ∘ setFromListWithTop
-- 
-- instance (Functorial Pretty f) ⇒ Pretty (Fix f) where
--   pretty (Fix f) =
--     with (functorial ∷ W (Pretty (f (Fix f)))) $
--     pretty f
-- 
-- instance (Pretty a, Pretty f) ⇒ Pretty (Stamped a f) where
--   pretty (Stamped a f) = 
--     -- pretty f
--     atLevel (𝕟 0) $ exec [pretty a, pun ":", pretty f]
-- 
-- instance (Pretty a, Functorial Pretty f) ⇒ Pretty (StampedFix a f) where
--   pretty (StampedFix a f) = 
--     with (functorial ∷ W (Pretty (f (StampedFix a f)))) $ 
--     -- pretty f
--     atLevel (𝕟 0) $ exec [bump $ pretty a, pun ":", pretty f]
-- 
-- instance (Pretty a) ⇒ Pretty (ID a) where pretty (ID a) = pretty a
-- instance Functorial Pretty ID where functorial = W
-- 
-- instance (Functorial Pretty m, Pretty e, Pretty a) ⇒ Pretty (ErrorT e m a) where
--   pretty (ErrorT aM) =
--     with (functorial ∷ W (Pretty (m (e :+: a)))) $
--     pretty aM
-- 
-- instance (Functorial Pretty m, Pretty a) ⇒ Pretty (ListT m a) where
--   pretty (ListT aM) =
--     with (functorial ∷ W (Pretty (m [a]))) $
--     pretty aM
-- 
-- instance (Pretty a) ⇒ Pretty (SumOfProd a) where
--   pretty = 
--     collection "{" "}" "⊔"
--     ∘ map (collection "{" "}" "⊓")
--     ∘ map (map pretty ∘ toList) ∘ toList
--     ∘ unSumOfProd
--     
-- -- }}}
--
--
-- makeMonoid ''Format
-- 
-- setFG ∷ Color → Format
-- setFG fg = null { foreground = First fg }
-- setBG ∷ Color → Format
-- setBG bg = null { background = First bg }
-- setUL ∷ Format
-- setUL = null { underline = True }
-- setBD ∷ Format
-- setBD = null { bold = True }

-- whenFlat ∷ (MonadPretty m) ⇒ m () → m ()
-- whenFlat aM = ppIfFlat aM $ return ()
-- 
-- whenBreak ∷ (MonadPretty m) ⇒ m () → m ()
-- whenBreak aM = ppIfFlat (return ()) aM
-- 
-- mustBreak ∷ (MonadPretty m) ⇒ m () → m ()
-- mustBreak aM = whenFlat abort >> aM

-- hsepTight ∷ (MonadPretty m) ⇒ [m ()] → m ()
-- hsepTight = exec ∘ intersperse (ppIfFlat (return ()) (ppSpace $ 𝕟 1))
-- 
-- hvsepTight ∷ (MonadPretty m) ⇒ [m ()] → m ()
-- hvsepTight = ppGroup ∘ exec ∘ intersperse (ppIfFlat (return ()) ppNewline)
-- 

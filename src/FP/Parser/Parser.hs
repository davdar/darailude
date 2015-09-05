module FP.Parser.Parser where

import FP.Prelude
import FP.Pretty

-- # Render

-- class Render t where
--   render ∷ t → Doc
--   renderError ∷ t → Doc
--   renderError = render
-- 
-- instance Render ℂ where
--   render c = ppText $ 𝕤 $ single c
--   renderError '\n' = ppErr "\\n\n"
--   renderError c = render c

renderChar ∷ ℂ → Doc
renderChar = ppText ∘ 𝕤

renderErrorChar ∷ ℂ → Doc
renderErrorChar '\n' = ppErr "\\n\n"
renderErrorChar c = renderChar c

-- # Loc

data Loc = Loc
  { locPos ∷ ℕ
  , locRow ∷ ℕ
  , locCol ∷ ℕ
  }
makeLenses ''Loc
makePrettySum ''Loc

instance Eq Loc where
  (==) = (==) `on` locPos
instance Ord Loc where
  compare = (⋚) `on` locPos

instance Bot Loc where
  bot = Loc bot bot bot
instance Join Loc where
  l₁ ⊔ l₂ = case locPos l₁ ⋚ locPos l₂ of
    LT → l₂
    EQ → l₁
    GT → l₁
instance Meet Loc where
  l₁ ⊓ l₂ = case locPos l₁ ⋚ locPos l₂ of
    LT → l₁
    EQ → l₁
    GT → l₂

bumpRow ∷ Loc → Loc
bumpRow (Loc pos row _) = Loc (pos + 𝕟 1) (row + 𝕟 1) (𝕟 0)

bumpCol ∷ Loc → Loc
bumpCol (Loc pos row col) = Loc (pos + 𝕟 1) row (col + 𝕟 1)

loc₀ ∷ Loc
loc₀ = Loc (𝕟 0) (𝕟 0) (𝕟 0)

data LocRange = LocRange
  { locRangeBegin ∷ Loc
  , locRangeEnd ∷ Loc
  } deriving (Eq, Ord)
makeLenses ''LocRange
makePrettySum ''LocRange

instance Join LocRange where
  LocRange b₁ e₁ ⊔ LocRange b₂ e₂ = LocRange (b₁ ⊓ b₂) (e₁ ⊔ e₂)

-- # Token

data Token t = Token
  { tokenValue ∷ t
  , tokenLocRange ∷ LocRange
  , tokenRender ∷ Doc
  , tokenRenderError ∷ Doc
  }
makeLenses ''Token
makePrettySum ''Token

-- # ParserInput

data ParserInput t = ParserInput
  { parserInputStream ∷ Stream (Token t)
  , parserInputNextLoc ∷ Loc
  }
makeLenses ''ParserInput
makePrettySum ''ParserInput

parserInput₀ ∷ Stream (Token t) → ParserInput t
parserInput₀ ss = ParserInput ss loc₀

-- # ParserEnv

data ParserEnv t = ParserEnv
  { parserEnvRenderFormat ∷ [Format] 
  , parserEnvErrorStack ∷ ([𝕊],𝕊)
  , parserEnvLevel ∷ ℕ
  , parserEnvBumped ∷ 𝔹
  }
makeLenses ''ParserEnv
makePrettySum ''ParserEnv

parserEnv₀ ∷ ParserEnv t
parserEnv₀ = ParserEnv [] ([],"<top level>") (𝕟 0) False

-- # ParserOut

data ErrorTrace = ErrorTrace 
  { errorTraceFinal ∷ 𝒫 𝕊
  , errorTraceChain ∷ 𝕊 ⇰ ErrorTrace
  } deriving (Eq, Ord)
makePrettySum ''ErrorTrace
instance Bot ErrorTrace where
  bot = ErrorTrace bot bot
instance Join ErrorTrace where
  ErrorTrace fin₁ ch₁ ⊔ ErrorTrace fin₂ ch₂ = ErrorTrace (fin₁ ⊔ fin₂) (ch₁ ⊔ ch₂)
instance JoinLattice ErrorTrace

errorTraceFromList ∷ [𝕊] → 𝕊 → ErrorTrace
errorTraceFromList [] fin = ErrorTrace (set $ single fin) bot
errorTraceFromList (msg:msgs) fin = ErrorTrace bot $ msg ↦ errorTraceFromList msgs fin

data ParserErrorInfo = ParserErrorInfo
  { parserErrorInfoPrefix ∷ Doc
  , parserErrorInfoTrace ∷ ErrorTrace
  }
makeLenses ''ParserErrorInfo
makePrettySum ''ParserErrorInfo

data ParserError t = ParserError
  { parserErrorInput ∷ ParserInput t
  , parserErrorContexts ∷ (AddBot LocRange,Doc) ⇰ ParserErrorInfo
  }
makeLenses ''ParserError
makePrettySum ''ParserError

data ParserErrorM t = NullParserErrorM | ParserErrorM (ParserError t)
makePrisms ''ParserErrorM
makePrettySum ''ParserErrorM

instance Monoid (ParserErrorM t) where
  null = NullParserErrorM
  NullParserErrorM ⧺ pem = pem
  pem ⧺ NullParserErrorM = pem
  ParserErrorM (ParserError pin₁ ectxs₁) ⧺ ParserErrorM (ParserError pin₂ ectxs₂) = ParserErrorM $ case parserInputNextLoc pin₁ ⋚ parserInputNextLoc pin₂ of
    LT → ParserError pin₂ ectxs₂
    EQ → 
      ParserError pin₁ $ unionWithDictOn ectxs₁ ectxs₂ $ \ pei₁ pei₂ →
        let ParserErrorInfo pre₁ trace₁ = pei₁
            ParserErrorInfo _    trace₂ = pei₂
        in ParserErrorInfo pre₁ (trace₁ ⊔ trace₂)
    GT → ParserError pin₁ ectxs₁

data ParserOut t = ParserOut
  { parserOutSuccessDepth ∷ ℕ
  , parserOutError ∷ ParserErrorM t
  }
makeLenses ''ParserOut
makePrettySum ''ParserOut

instance Monoid (ParserOut t) where
  null = ParserOut (𝕟 0) null
  ParserOut sd₁ er₁ ⧺ ParserOut sd₂ er₂ = ParserOut  (sd₁ ⊔ sd₂) (er₁ ⧺ er₂)

-- # ParserState

data ParserContext t = ParserContext
  { parserContextPrefix ∷ Doc
  , parsercontextPrefixError ∷ Doc
  , parserContextDisplay ∷ Doc
  , parserContextDisplayError ∷ Doc
  , parserContextRange ∷ AddBot LocRange
  }
makeLenses ''ParserContext
makePrettySum ''ParserContext

data ParserState t = ParserState
  { parserStateInput ∷ ParserInput t
  , parserStateErrorContext ∷ ParserContext t
  , parserStateCaptureContext ∷ ParserContext t
  }
makeLenses ''ParserState
makePrettySum ''ParserState

parserState₀ ∷ Stream (Token t) → ParserState t
parserState₀ ss =
  ParserState (parserInput₀ ss)
  (ParserContext null null null null bot)
  (ParserContext null null null null bot)

-- # Parser Monad

newtype Parser t a = Parser 
  { runParser ∷ 
      ReaderT (ParserEnv t) 
      (StateT (ParserState t) 
       (NondetT 
        (Writer (ParserOut t)))) a 
  } deriving 
  ( Functor,Monad
  , MonadReader (ParserEnv t)
  , MonadWriter (ParserOut t)
  , MonadState (ParserState t)
  , MonadMonoid
  )

runParserWith ∷ ParserEnv t → ParserState t → Parser t a → (ParserOut t,[(ParserState t,a)])
runParserWith r s = runWriter ∘ runNondetT ∘ runStateTWith s ∘ runReaderTWith r ∘ runParser

makeParser ∷ (ParserEnv t → ParserState t → (ParserOut t,[(ParserState t,a)])) → Parser t a
makeParser f = Parser $ ReaderT $ \ r → StateT $ \ s → NondetT $ WriterT $ ID $ f r s

pFail ∷ Parser t a
pFail = do
  pi ← getL parserStateInputL
  ParserContext prefix _prefixError display _displayError range ← getL parserStateErrorContextL
  (stack,message) ← askL parserEnvErrorStackL
  tell $ ParserOut bot $ ParserErrorM $ 
    ParserError pi
    ((range,display) ↦ (ParserErrorInfo prefix $ errorTraceFromList (reverse stack) message))
  mzero

pAppendError ∷ 𝕊 → Parser t a → Parser t a
pAppendError msg xM = do
  (stack,msg') ← askL parserEnvErrorStackL
  local (update parserEnvErrorStackL (msg':stack,msg)) xM

pNewContext ∷ Lens (ParserState t) (ParserContext t) → Parser t a → Parser t (a,ParserContext t)
pNewContext 𝓁 xM = do
  ParserContext prefix prefixError display displayError range ← getL 𝓁
  putL 𝓁 $ ParserContext (prefix ⧺ display) (prefixError ⧺ displayError) null null bot
  x ← xM
  ParserContext prefix' prefixError' display' displayError' range' ← getL 𝓁
  putL 𝓁 $ ParserContext prefix prefixError (display ⧺ display') (displayError ⧺ displayError') (range ⊔ range')
  return (x,ParserContext prefix' prefixError' display' displayError' range')

pError ∷ 𝕊 → Parser t a → Parser t a
pError msg = compose
  [ fst ^∘ pNewContext parserStateErrorContextL 
  , local (update parserEnvErrorStackL ([],msg))
  ]

pCapture ∷ Parser t a → Parser t (a,ParserContext t)
pCapture = pNewContext parserStateCaptureContextL

pRender ∷ Color → Parser t s → Parser t s
pRender c = local (alter parserEnvRenderFormatL (⧺ [FormatFG c]))

pPluck ∷ Parser t t
pPluck = do
  ParserInput ts nextLoc ← getL parserStateInputL
  case suncons ts of
    Nothing → pAppendError "more input" pFail
    Just (x,ts') → do
      let nextNextLoc = case suncons ts' of
            Nothing → bumpCol nextLoc
            Just (x',_) → locRangeBegin $ tokenLocRange x'
      putL parserStateInputL $ ParserInput ts' nextNextLoc
      fmt ← askL parserEnvRenderFormatL
      modifyL parserStateErrorContextL $ updateContext fmt x
      modifyL parserStateCaptureContextL $ updateContext fmt x
      return $ tokenValue x
  where
    updateContext fmt x (ParserContext prefix prefixError display displayError range) =
      ParserContext prefix  prefixError
      (display ⧺ (ppFormat fmt $ tokenRender x))
      (displayError ⧺ (ppFormat fmt $ tokenRenderError x))
      (range ⊔ AddBot (tokenLocRange x))

pEnd ∷ Parser t ()
pEnd = do
  ts ← getL (parserInputStreamL ⌾ parserStateInputL)
  when (shape justL $ suncons ts) $ pAppendError "end of stream" pFail

pFinal ∷ Parser t a → Parser t a
pFinal aM = do
  a ← aM
  pEnd
  return a

pShaped ∷ 𝕊 → (t → Maybe a) → Parser t a
pShaped msg sh = do
  s ← get
  t ← pPluck
  case sh t of
    Nothing → do
      put s
      pAppendError msg pFail
    Just x → return x

pSatisfies ∷ 𝕊 → (t → 𝔹) → Parser t t
pSatisfies msg p = pShaped msg $ \ x → if p x then Just x else Nothing

pLit ∷ (Eq t,Pretty t) ⇒ t → Parser t t
pLit l = pSatisfies (ppString l) ((==) l)

pWord ∷ ∀ s t. (Eq s,Pretty s,Eq t,Pretty t,Isomorphism s [t]) ⇒ s → Parser t s
pWord s = pAppendError (ppString s) $ isoFrom ^$ mapM pLit (isoTo s ∷ [t])

pOptional ∷ Parser t a → Parser t (Maybe a)
pOptional p = mconcat [map Just p,return Nothing]

pCatch ∷ Parser t a → Parser t a → Parser t a
pCatch cM xM = do
  (o,xM') ← hijack $ mconcat
    [ do
        x ← xM
        pos ← getL $ locPosL ⌾ parserInputNextLocL ⌾ parserStateInputL
        tell $ ParserOut pos null
        return $ Just x
    , return Nothing
    ]
  tell $ ParserOut bot $ parserOutError o
  if parserOutSuccessDepth o > 𝕟 0
    then returnMaybe mzero xM'
    else cM

pOptionalGreedy ∷ Parser t a → Parser t (Maybe a)
pOptionalGreedy = pCatch (return Nothing) ∘ map Just

pManyGreedy ∷ Parser t a → Parser t [a]
pManyGreedy xM = pCatch (return []) $ do
  x ← xM
  xs ← pManyGreedy xM
  return $ x:xs

pOneOrMoreGreedy ∷ Parser t a → Parser t [a]
pOneOrMoreGreedy xM = do
  x ← xM
  xs ← pManyGreedy xM
  return $ x:xs

pManySepByGreedy ∷ Parser t () → Parser t a → Parser t [a]
pManySepByGreedy sepM xM = pCatch (return []) $ do
  x ← xM
  xs ← map snd ^$ pManyGreedy $ sepM <×> xM
  return $ x:xs

tokens ∷ 𝕊 → Stream (Token ℂ)
tokens (stream → Stream s₀ f) = streamState loc₀ $ MStream s₀ $ \ s → do
  (c,s') ← abortMaybe $ f s
  loc ← get
  put $ if c == '\n'
    then bumpRow loc
    else bumpCol loc
  return (Token c (LocRange loc loc) (renderChar c) (renderErrorChar c),s')

displayErrorTrace ∷ ErrorTrace → Doc
displayErrorTrace (ErrorTrace final chain) = ppVertical $ concat
  [ if isEmpty final then null else return $ ppHorizontal $ concat
      [ return $ ppFG red $ ppText "Expected"
      , intersperse (ppFG red $ ppText "OR") $ map ppText $ list final
      ]
  , mapOn (list chain) $ \ (msg,tr) → ppVertical
      [ ppHorizontal
          [ ppFG darkGreen $ ppText "Parsing"
          , ppText msg
          ]
      , concat [ppSpace (𝕟 2),ppAlign $ displayErrorTrace tr]
      ]
  ]

displayParserError ∷ ParserErrorM t → Doc
displayParserError NullParserErrorM = ppHeader "Nothing to Parse"
displayParserError (ParserErrorM (ParserError (ParserInput ts (Loc _ row col)) ectxs)) = 
  ppVertical $ concat
  [ return $ ppHeader "Parse Failure"
  , return $ ppHorizontal 
      [ ppErr ">"
      , concat [ppText "row:",pretty row]
      , concat [ppText "col:",pretty col]
      ]
  , return $ ppHeader "One Of:"
  , intersperse (ppHeader "OR") $ mapOn (list ectxs) $ 
    \ ((locRange,ctx),ParserErrorInfo pre etrace) → 
        let blind = case locRange of
              Bot → id
              AddBot (LocRange low high) → ppBlinders (locRow low) (locRow high)
            (nextTok,followStream) = case suncons ts of
              Nothing → (ppErr "EOF",null)
              Just (x,ts') → (tokenRenderError x,ts')
        in
        ppVertical
          [ ppLineNumbers $ ppSetLineNumber (𝕟 0) $ blind $ concat
              [ pre
              , ppUT '^' green ctx
              , ppUT '^' red nextTok
              , concat $ map tokenRender followStream
              ]
          , displayErrorTrace etrace
          ]
  ]
             
runParser₀ ∷ (ToStream (Token t) ts) ⇒ Parser t a → ts → (ParserOut t,[(ParserState t,a)])
runParser₀ p ts = runParserWith parserEnv₀ (parserState₀ $ stream ts) p

runParserFinal ∷ (ToStream (Token t) ts) ⇒ Parser t a → ts → (ParserOut t,[(ParserState t,a)])
runParserFinal p ss = runParser₀ (pFinal p) ss

parse ∷ (ToStream (Token t) ts,Pretty a) ⇒ Parser t a → ts → Doc ⨄ a
parse p ss = case runParserWith parserEnv₀ (parserState₀ $ stream ss) (pFinal p) of
  (ParserOut _ pe,[]) → Left $ displayParserError pe
  (_,[(_,x)]) → Right x
  (_,(x:xs)) → Left $ ppVertical $ concat
    [ return $ ppHeader "Ambiguous Parse"
    , intersperse (ppHeader "OR") $ map (pretty ∘ snd) (x:xs)
    ]

parseIO ∷ (ToStream (Token t) ts,Pretty a) ⇒ Parser t a → ts → IO a
parseIO p ss = case parse p ss of
  Left d → pprint d ≫ abortIO
  Right a → return a

parseIOMain ∷ (ToStream (Token t) ts,Pretty a) ⇒ Parser t a → ts → IO ()
parseIOMain p ss = do
  x ← parseIO p ss
  pprint $ ppVertical
    [ ppHeader "Success"
    , pretty x
    ]

tokenize ∷ ∀ ts t a. (ToStream (Token t) ts) ⇒ Parser t a → ts → Doc ⨄ [Token a]
tokenize p ss = loop (parserState₀ $ stream ss) null
  where
    loop ∷ ParserState t → ParserOut t → Doc ⨄ [Token a]
    loop s pe
      | isEmpty $ parserInputStream $ parserStateInput s = return null
      | otherwise =
          let (ParserOut sd pe',sxs₀) = runParserWith parserEnv₀ s (tell pe ≫ pCapture p)
              ord = flip compare `on` (locPos ∘ parserInputNextLoc ∘ parserStateInput ∘ fst)
              sxs = head $ sortBy ord sxs₀
          in case sxs of
            Nothing → Left $ displayParserError pe'
            Just (s',(x,cc)) → do
              xs ← loop s' $ ParserOut sd pe'
              let locRange = case parserContextRange cc of
                    Bot → 
                      let loc = parserInputNextLoc $ parserStateInput s
                      in LocRange loc loc
                    AddBot r → r
              return $ Token x locRange (parserContextDisplay cc) (parserContextDisplayError cc):xs

tokenizeIO ∷ (ToStream (Token t) ts,Pretty a) ⇒ Parser t a → ts → IO [Token a]
tokenizeIO p ss = case tokenize p ss of
  Left d → pprint d ≫ abortIO
  Right a → return a

tokenizeIOMain ∷ (ToStream (Token t) ts,Pretty a) ⇒ Parser t a → ts → IO ()
tokenizeIOMain p ss = do
  x ← tokenizeIO p ss
  pprint $ ppVertical
    [ ppHeader "Success"
    , pretty x
    ]

testParsingMultipleFailure ∷ IO ()
testParsingMultipleFailure = parseIOMain parser input
  where
    parser ∷ Parser ℂ 𝕊
    parser = mconcat
      [ pError "XXX*" $ mconcat
          [ pRender pink $ pWord "xxxy"
          , pRender pink $ pWord "xxxz"
          ]
      , pError "XXXZ" $ do
          x ← pError "XX" $ pRender blue $ pWord "xx"
          y ← pError "XZ" $ pRender green $ pWord "xz"
          return $ x ⧺ y
      , pError "XXZZ" $ pWord "xxzz"
      , pError "XXXAorB" $ pRender teal $ do
          x ← pWord "xxx"
          y ← 𝕤 ∘ single ^$ mconcat
            [ pLit 'a'
            , pLit 'b'
            ]
          return $ x ⧺ y
      ]
    input ∷ Stream (Token ℂ)
    input = tokens "xxxx"
    
testParsingBlinders ∷ IO ()
testParsingBlinders = parseIOMain parser input
  where
    parser ∷ Parser ℂ [𝕊]
    parser = oneOrMore $ pError "Item" $ mconcat
      [ pWord "xxxx"
      , 𝕤 ∘ single ^$ pLit '\n'
      , pWord "xxxxxxxx\nxxxxxxxx"
      ]
    input ∷ Stream (Token ℂ)
    input = tokens "xxxx\nxxxxxxxx\nxxxxxxxy\nxxxxxxxx\nxxxxxxxx"

testParsingAmbiguity ∷ IO ()
testParsingAmbiguity = parseIOMain parser input
  where
    parser = concat ^$ oneOrMore $ mconcat 
      [ ppFG green ∘ ppText ∘ 𝕤 ∘ single ^$ pLit 'x'
      , ppFG blue ∘ ppText ^$ pWord "xx" 
      ]
    input = tokens "xxx"

testParsingSuccess ∷ IO ()
testParsingSuccess = parseIOMain parser input
  where
    parser = concat ^$ oneOrMore $ mconcat [pRender green $ pWord "xx",pRender blue $ pWord "yy"]
    input = tokens "xxxxyyxxyy"

testParsingErrorNewline ∷ IO ()
testParsingErrorNewline = parseIOMain (𝕤 ^$ many $ pLit 'x') $ tokens "xxx\nx"

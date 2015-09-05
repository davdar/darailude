module FP.Parser.SExp where

import FP.Prelude
import FP.Parser.Parser
import FP.Pretty

import qualified Prelude

data SNumber = 
    SNInteger ℤ
  | SNDouble 𝔻
  deriving (Eq,Ord)
makePrettySum ''SNumber

data SLit =
    SLNumber SNumber
  | SLString 𝕊
  deriving (Eq,Ord)
makePrettySum ''SLit

data SToken =
    STLParen
  | STRParen
  | STLit SLit
  | STSymbol 𝕊
  | STWhitespace 𝕊
  deriving (Eq,Ord)
makePrettySum ''SToken
makePrisms ''SToken

lparenTok ∷ Parser ℂ ()
lparenTok = pRender darkGray $ void $ pLit '(' 

rparenTok ∷ Parser ℂ ()
rparenTok = pRender darkGray $ void $ pLit ')' 

litTok ∷ Parser ℂ SLit
litTok = pRender darkRed $ mconcat
  [ SLNumber ^$ pError "number" numberTok
  , SLString ^$ pError "string" stringTok
  ]
  where
    numberTok ∷ Parser ℂ SNumber
    numberTok = mconcat
      [ SNInteger ^$ pError "integer" integerTok
      , SNDouble ^$ pError "double" doubleTok
      ]
      where
        integerTok ∷ Parser ℂ ℤ
        integerTok = do
          sign ← signTok
          digits ← 𝕤 ^$ pOneOrMoreGreedy $ pSatisfies "digit" isDigit
          return $ Prelude.read $ chars $ sign ⧺ digits
        doubleTok ∷ Parser ℂ 𝔻
        doubleTok = do
          sign ← signTok
          digitsBefore ← 𝕤 ^$ pOneOrMoreGreedy $ pSatisfies "digit" isDigit
          dec ← 𝕤 ^$ mapM pLit $ chars "."
          digitsAfter ← 𝕤 ^$ pOneOrMoreGreedy $ pSatisfies "digit" isDigit
          return $ Prelude.read $ chars $ sign ⧺ digitsBefore ⧺ dec ⧺ digitsAfter
        signTok ∷ Parser ℂ 𝕊
        signTok = mconcat
          [ 𝕤 ^$ mapM pLit $ chars "-"
          , return ""
          ] 
    stringTok ∷ Parser ℂ 𝕊
    stringTok = do
      void $ pLit '"'
      s ← concat ^$ pManyGreedy $ mconcat
        [ 𝕤 ∘ single ^$ pSatisfies "anything but '\"' or '\\'" $ \ c → not $ c ≟ '"' ∨ c ≟ '\\'
        , pAppendError "escape sequence" $ do
            bslash ← 𝕤 ∘ single ^$ pLit '\\'
            c ← 𝕤 ∘ single ^$ pLit '\\' <⧺> pLit 'n'
            return $ bslash ⧺ c
        ]
      void $ pLit '"'
      return s

symbolTok ∷ Parser ℂ 𝕊
symbolTok = 𝕤 ^$ pOneOrMoreGreedy $ pSatisfies "letter" isLetter

whitespaceTok ∷ Parser ℂ 𝕊
whitespaceTok = 𝕤 ^$ pOneOrMoreGreedy $ pSatisfies "space" isSpace

tok ∷ Parser ℂ SToken
tok = mconcat
  [ const STLParen ^$ pError "lparen"     lparenTok 
  , const STRParen ^$ pError "rparen"     rparenTok 
  , STLit          ^$ pError "lit"        litTok
  , STSymbol       ^$ pError "symbol"     symbolTok
  , STWhitespace   ^$ pError "whitespace" whitespaceTok
  ]

testSExpTokenizerSuccess ∷ IO ()
testSExpTokenizerSuccess = tokenizeIOMain tok $ tokens "((-1-2-1.42(\"astringwith\\\\stuff\\n\" ( "

testSExpTokenizerFailure1 ∷ IO ()
testSExpTokenizerFailure1 = tokenizeIOMain tok $ tokens "((foo-1and0.01+bar"

testSExpTokenizerFailure2 ∷ IO ()
testSExpTokenizerFailure2 = tokenizeIOMain tok $ tokens "()foo-1\"astring\\badescape\""

data FullContext t = FullContext
  { fullContextCaptured ∷ ParserContext t
  , fullContextFutureInput ∷ ParserInput t
  }
instance Pretty (FullContext t) where
  pretty (FullContext (ParserContext pre _ display _ _) (ParserInput ss _)) = concat
    [ ppPun "⟬"
    , ppAlign $ pre ⧺ (ppUT '^' green display) ⧺ concat (map tokenRender ss)
    , ppPun "⟭"
    ]

data SAtom =
    SALit SLit
  | SASymbol 𝕊
makePrettySum ''SAtom
data TaggedFix t (f ∷ ★ → ★) = TaggedFix
  { taggedFixContext ∷ FullContext t
  , taggedFixValue ∷ f (TaggedFix t f)
  }
makePrettySum ''TaggedFix
data PreSExp e =
    SEAtom SAtom
  | SEExp [e]
makePrettySum ''PreSExp
type SExp = TaggedFix SToken PreSExp

atomPar ∷ Parser SToken SAtom
atomPar = pError "atom" $ mconcat
  [ SALit ^$ litPar
  , SASymbol ^$ symbolPar
  ]

litPar ∷ Parser SToken SLit
litPar = pShaped "lit" $ view sTLitL

symbolPar ∷ Parser SToken 𝕊
symbolPar = pShaped "symbol" $ view sTSymbolL

preSExpPar ∷ Parser SToken (PreSExp SExp)
preSExpPar = mconcat
  [ SEAtom ^$ atomPar
  , SEExp ^$ inParensPar
  ]

inParensPar ∷ Parser SToken [SExp]
inParensPar = do
  void $ pLit STLParen
  es ← sexpsPar
  void $ pLit STRParen
  return es

sexpsPar ∷ Parser SToken [SExp]
sexpsPar = do
  void $ pOptionalGreedy $ pSatisfies "whitespace" $ shape sTWhitespaceL
  xs ← pManySepByGreedy (void $ pOptionalGreedy $ pSatisfies "whitespace" $ shape sTWhitespaceL) sexpPar
  void $ pOptionalGreedy $ pSatisfies "whitespace" $ shape sTWhitespaceL
  return xs

sexpPar ∷ Parser SToken SExp
sexpPar = do
  (s,cc) ← pCapture $ preSExpPar
  pin ← getL parserStateInputL
  return $ TaggedFix (FullContext cc pin) s

testSExpParserSuccess ∷ IO ()
testSExpParserSuccess = do
  toks ← tokenizeIO tok input
  parseIOMain sexpsPar toks
  where
    input ∷ Stream (Token ℂ)
    input = tokens " x y  ( -1-2)  0.0"

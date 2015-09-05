module FP.Parser.Mixfix where

import FP.Prelude
import FP.Pretty
import FP.Parser.Parser

data Mixes t a = Mixes
  { mixesPre  ∷ Parser t (a → a)
  , mixesPost ∷ Parser t (a → a)
  , mixesInf  ∷ Parser t (a → a → a)
  , mixesInfl ∷ Parser t (a → a → a)
  , mixesInfr ∷ Parser t (a → a → a)
  }

instance Monoid (Mixes t a) where
  null = Mixes mzero mzero mzero mzero mzero
  Mixes pre₁ post₁ inf₁ infl₁ infr₁ ⧺ Mixes pre₂ post₂ inf₂ infl₂ infr₂ = 
    Mixes (pre₁ <⧺> pre₂) (post₁ <⧺> post₂) (inf₁ <⧺> inf₂) (infl₁ <⧺> infl₂) (infr₁ <⧺> infr₂)

data Mix t a =
    Pre  ℕ (Parser t (a → a))
  | Post ℕ (Parser t (a → a))
  | Inf  ℕ (Parser t (a → a → a))
  | Infl ℕ (Parser t (a → a → a))
  | Infr ℕ (Parser t (a → a → a))
  | Terminal (Parser t a)

data Mixfix t a = Mixfix 
  { mixfixTerminals ∷ Parser t a
  , mixfixLevels ∷ ℕ ⇰ Mixes t a
  }
instance Monoid (Mixfix t a) where
  null = Mixfix mzero bot
  Mixfix ts₁ ls₁ ⧺ Mixfix ts₂ ls₂ = Mixfix (ts₁ <⧺> ts₂) (ls₁ ⧺ ls₂)

mix ∷ Mix t a → Mixfix t a
mix (Pre l pre) = null {mixfixLevels = l ↦ null {mixesPre = pre}}
mix (Post l post) = null {mixfixLevels = l ↦ null {mixesPost = post}}
mix (Inf l inf) = null {mixfixLevels = l ↦ null {mixesInf = inf}}
mix (Infl l infl) = null {mixfixLevels = l ↦ null {mixesInfl = infl}}
mix (Infr l infr) = null {mixfixLevels = l ↦ null {mixesInfr = infr}}
mix (Terminal term) = null {mixfixTerminals = term}

-- PRE (PRE (x INFR (PRE (PRE y))))
-- PRE PRE x INFR PRE PRE y
-- 
-- ((((x POST) POST) INFL y) POST) POST
-- x POST POST INFL y POST POST

mixfixParser ∷ ∀ t a. Mixfix t a → Parser t a
mixfixParser (Mixfix terms levels₀) = loop levels₀
  where
    loop ∷ ℕ ⇰ Mixes t a → Parser t a
    loop levels = case removeDict levels of
      Nothing → terms
      Just ((i,mixes),levels') → 
        let nextLevel = loop levels'
        in buildLevel i nextLevel mixes
    buildLevel ∷ ℕ → Parser t a → Mixes t a → Parser t a
    buildLevel i nextLevel mixes = mconcat
      [ do
          x ← nextLevel
          mconcat
            [ pAppendError ("infixl " ⧺ ppString i) $ levelInflAfterOne x nextLevel mixes
            , pAppendError ("infix "  ⧺ ppString i) $ levelInfAfterOne  x nextLevel mixes
            , pAppendError ("infixr " ⧺ ppString i) $ levelInfrAfterOne x nextLevel mixes
            , return x
            ]
      , pAppendError ("infixr " ⧺ ppString i) $ levelInfrNotAfterOne nextLevel mixes
      ]
    levelInflAfterOne ∷ a → Parser t a → Mixes t a → Parser t a
    levelInflAfterOne x nextLevel mixes = do
      fxs ← pOneOrMoreGreedy $ mconcat
        [ mixesPost mixes
        , do
            f ← mixesInfl mixes
            x₂ ← nextLevel
            return $ \ x₁ → f x₁ x₂
        ]
      return $ compose (reverse fxs) x
    _levelInfr ∷ Parser t a → Mixes t a → Parser t a
    _levelInfr nextLevel mixes = do
      fxs ← pOneOrMoreGreedy $ mconcat
        [ mixesPre mixes
        , do
            x₁ ← nextLevel
            f ← mixesInfr mixes
            return $ \ x₂ → f x₁ x₂
        ]
      x ← nextLevel
      return $ compose fxs x
    levelInfrAfterOne ∷ a → Parser t a → Mixes t a → Parser t a
    levelInfrAfterOne x₁ nextLevel mixes = do
      f ← mixesInfr mixes
      levelInfrAfterOneCombo (\ x₂ → f x₁ x₂) nextLevel mixes
    levelInfrNotAfterOne ∷ Parser t a → Mixes t a → Parser t a
    levelInfrNotAfterOne nextLevel mixes = do
      f ← mixesPre mixes
      levelInfrAfterOneCombo f nextLevel mixes
    levelInfrAfterOneCombo ∷ (a → a) → Parser t a → Mixes t a → Parser t a
    levelInfrAfterOneCombo f nextLevel mixes = do
      fxs ← pManyGreedy $ mconcat
        [ mixesPre mixes
        , do
            x₁ ← nextLevel
            f' ← mixesInfr mixes
            return $ \ x₂ → f' x₁ x₂
        ]
      x₂ ← nextLevel
      return $ f $ compose fxs x₂
    levelInfAfterOne ∷ a → Parser t a → Mixes t a → Parser t a
    levelInfAfterOne x₁ nextLevel mixes = do
      f ← mixesInf mixes
      x₂ ← nextLevel
      return $ f x₁ x₂

-- Sample Expression Language

data ExpToken =
    ETWhitespace 𝕊
  | ETLParen
  | ETRParen
  | ETSymbol 𝕊
  | ETNatural ℕ
  | ETPlus     -- infixr   5
  | ETTimes    -- infixr   6
  | ETPower    -- infixl   7
  | ETFact     -- postfix  7
  | ETNegative -- prefix   8
  | ETEqual    -- infix    5
makePrisms ''ExpToken
makePrettySum ''ExpToken

tokExp ∷ Parser ℂ ExpToken
tokExp = mconcat
  [ inject eTWhitespaceL ∘ 𝕤 ^$ pOneOrMoreGreedy $ pSatisfies "space" isSpace 
  , inject eTLParenL ^$ void $ pLit '('
  , inject eTRParenL ^$ void $ pLit ')'
  , inject eTSymbolL ∘ 𝕤 ^$ pOneOrMoreGreedy $ pSatisfies "letter" isLetter
  , inject eTNaturalL ∘ 𝕤read ∘ 𝕤 ^$ pOneOrMoreGreedy $ pSatisfies "digit" isDigit
  , inject eTPlusL ^$ void $ pLit '+'
  , inject eTTimesL ^$ void $ pLit '*'
  , inject eTPowerL ^$ void $ pLit '^'
  , inject eTFactL ^$ void $ pLit '!'
  , inject eTNegativeL ^$ void $ pLit '-'
  , inject eTEqualL ^$ void $ pLit '='
  ]

testTokExpSuccess ∷ IO ()
testTokExpSuccess = tokenizeIOMain tokExp $ tokens "1 + 2 - 3 * 4 ^ 5 ! = 1"

data Atom =
    ASymbol 𝕊
  | ANatural ℕ
makePrettySum ''Atom

data Exp =
    EAtom Atom
  | ESum Exp Exp
  | EProduct Exp Exp
  | EExpo Exp Exp
  | EFact Exp
  | ENegate Exp
  | EEquality Exp Exp
makePrisms ''Exp
makePrettySum ''Exp

parseExp ∷ Parser ExpToken Exp 
parseExp = pError "exp" $ mixfixParser $ concat
  [ mix $ Terminal $ do
      void $ pSatisfies "lparen" $ shape eTLParenL
      x ← parseExp
      void $ pSatisfies "rparen" $ shape eTRParenL
      return x
  , mix $ Terminal $ EAtom ∘ ASymbol ^$ pShaped "symbol" $ view eTSymbolL
  , mix $ Terminal $ EAtom ∘ ANatural ^$ pShaped "natural" $ view eTNaturalL
  , mix $ Infr (𝕟 5) $ const ESum ^$ surroundWhitespace $ pShaped "plus" $ view eTPlusL
  , mix $ Infr (𝕟 6) $ const EProduct ^$ surroundWhitespace $ pShaped "times" $ view eTTimesL
  , mix $ Infl (𝕟 7) $ const EExpo ^$ surroundWhitespace $ pShaped "power" $ view eTPowerL
  , mix $ Post (𝕟 7) $ const EFact ^$ preWhitespace $ pShaped "fact" $ view eTFactL
  , mix $ Pre  (𝕟 8) $ const ENegate ^$ postWhitespace $ pShaped "neg" $ view eTNegativeL
  , mix $ Inf  (𝕟 5) $ const EEquality ^$ surroundWhitespace $ pShaped "equal" $ view eTEqualL
  ]
  where
    surroundWhitespace ∷ Parser ExpToken a → Parser ExpToken a
    surroundWhitespace xM = do
      void $ pOptionalGreedy $ pSatisfies "whitespace" $ shape eTWhitespaceL
      x ← xM
      void $ pOptionalGreedy $ pSatisfies "whitespace" $ shape eTWhitespaceL
      return x
    preWhitespace ∷ Parser ExpToken a → Parser ExpToken a
    preWhitespace xM = do
      void $ pOptionalGreedy $ pSatisfies "whitespace" $ shape eTWhitespaceL
      xM
    postWhitespace ∷ Parser ExpToken a → Parser ExpToken a
    postWhitespace xM = do
      x ← xM
      void $ pOptionalGreedy $ pSatisfies "whitespace" $ shape eTWhitespaceL
      return x

testParseExpSuccess ∷ IO ()
testParseExpSuccess = parseIOMain parseExp *$ tokenizeIO tokExp $ tokens "(((((- 1))) + 2 + 3 * 4 ^ 5 ^ 6 !))"

testParseExpFailure1 ∷ IO ()
testParseExpFailure1 = parseIOMain parseExp *$ tokenizeIO tokExp $ tokens "((9 = (((- 1))) + 2 + 3 * 4 ^ 5 ^ 6 !))"

testParseExpFailure2 ∷ IO ()
testParseExpFailure2 = parseIOMain parseExp *$ tokenizeIO tokExp $ tokens "(((((- 1))) + 2 + 3 * 4 ^ 5 ^ 6 ! = 0))"

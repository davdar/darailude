module FP.Parser where

import FP.Core
import FP.Monads
import FP.Deriving.Prism
import FP.Deriving.Pretty
import FP.Deriving.Lens
import FP.Pretty (Pretty(..))
-- import FP.Console

data ParserState t = ParserState 
  { parserStateStream :: [t]
  , parserStateConsumed :: Int
  }
makeLenses ''ParserState
makePrettySum ''ParserState
instance Monoid (ParserState t) where
  null = ParserState [] (fmn 0)
  ParserState xs m ++ ParserState ys n = ParserState (xs ++ ys) (m + n)

class 
  ( Monad m
  , MonadBot m
  , MonadAppend m
  , MonadState (ParserState t) m
  ) => MonadParser t m | m -> t where

end :: (MonadParser t m) => m ()
end = do
  ts <- getL parserStateStreamL
  case ts of
    [] -> return ()
    _:_ -> mbot

final :: (MonadParser t m) => m a -> m a
final aM = do
  a <- aM
  end
  return a

satisfies :: (MonadParser t m) => (t -> Bool) -> m t
satisfies p = do
  ts <- getL parserStateStreamL
  case ts of
    t:ts' | p t -> do
      putL parserStateStreamL ts'
      bumpL parserStateConsumedL
      return t
    _ -> mbot

pluck :: (MonadParser t m) => m t
pluck = satisfies $ const True

lit :: (MonadParser t m, Eq t) => t -> m t
lit = satisfies . (==)

word :: (MonadParser t m, Eq t) => [t] -> m [t]
word ts = mapM lit ts

string :: (MonadParser Char m) => String -> m String
string = fromChars ^. word . toChars

newtype Parser t a = Parser { unParser :: StateT (ParserState t) (ListT ID) a }
  deriving 
    ( Unit, Functor, Product, Applicative, Bind, Monad
    , MonadBot, MonadAppend
    , MonadState (ParserState t)
    )
instance MonadParser t (Parser t) where

runParser :: [t] -> Parser t a -> [(ParserState t, a)]
runParser ts = unID . unListT . runStateT (ParserState ts $ fmn 0) . unParser

greedy :: Parser t a -> Parser t a
greedy p = do
  ParserState ts consumed <- get
  case runParser ts p of
    [] -> mbot
    x:xs -> do
      let (ParserState ts' consumed', a) = findMax (parserStateConsumed . fst) x xs
      put $ ParserState ts' (consumed + consumed')
      return a

most :: Parser t a -> Parser t [a]
most aM = greedy $ mconcat
  [ oneOrMostList aM
  , return []
  ]

oneOrMost :: Parser t a -> Parser t (a, [a])
oneOrMost aM = do
  x <- aM
  xs <- most aM
  return (x, xs)

oneOrMostList :: Parser t a -> Parser t [a]
oneOrMostList = uncurry (:) ^. oneOrMost

data ParseError t a =
    ParsingError [t]
  | AmbiguousParse [a]
makePrettySum ''ParseError

parseFinal :: Parser t a -> [t] -> ParseError t a :+: a
parseFinal p ts = case map snd $ runParser ts $ final p of
  [] -> Inl $ ParsingError ts
  [x] -> Inr x
  xs -> Inl $ AmbiguousParse xs

parseFinalOn :: [t] -> Parser t a -> ParseError t a :+: a
parseFinalOn = flip parseFinal

tokenize :: Parser c a -> [c] -> [c] :+: [a]
tokenize aM = loop 
  where
    loop [] = return []
    loop ts = do
      case runParser ts aM of
        [] -> throw ts
        x:xs -> do
          let (s, a) = findMax (parserStateConsumed . fst) x xs
          (a :) ^$ loop $ parserStateStream s 

data LexParseError c t a = 
    LexingError [c] 
  | LexParsingError [t]
  | LexAmbiguousParse ([t], [a])
makePrettySum ''LexParseError

lexParseFinal :: forall c t a. (Pretty c, Pretty t) => Parser c t -> (t -> Bool) -> Parser t a -> [c] -> LexParseError c t a :+: a
lexParseFinal tp wp ep cs = do
  ts <- mapInl LexingError $ tokenize tp cs
  let ts' = filter (not . wp) ts
  (x,xs) <- 
    maybeElimOn (view consL $ runParser ts' ep) (throw (LexParsingError ts' :: LexParseError c t a)) return
  if is nilL xs
    then return $ snd x
    else throw (LexAmbiguousParse (ts', map snd $ x:xs) :: LexParseError c t a)

-- Mixfix

data Mix m a = 
    Pre  (m (a -> a))
  | Post (m (a -> a))
  | Inf  (m (a -> a -> a))
  | InfL (m (a -> a -> a))
  | InfR (m (a -> a -> a))
makePrisms ''Mix

data Level m a = Level
  { levelPre :: m (a -> a)
  , levelPost :: m (a -> a)
  , levelInf :: m (a -> a -> a)
  , levelInfL :: m (a -> a -> a)
  , levelInfR :: m (a -> a -> a)
  }

splitMixes :: (MonadParser t m) => [Mix m a] -> Level m a
splitMixes ms = Level
  { levelPre = mconcat $ maybeZero . view preL *$ ms
  , levelPost = mconcat $ maybeZero . view postL *$ ms
  , levelInf = mconcat $ maybeZero . view infL *$ ms
  , levelInfL = mconcat $ maybeZero . view infLL *$ ms
  , levelInfR = mconcat $ maybeZero . view infRL *$ ms
  }

pre :: (Monad m) => (b -> a -> a) -> m b -> Mix m a
pre f bM = Pre $ do
  b <- bM
  return $ \ aR -> f b aR

post :: (Monad m) => (a -> b -> a) -> m b -> Mix m a
post f bM = Post $ do
  b <- bM
  return $ \ aL -> f aL b

inf' :: (Monad m) => (a -> b -> a -> a) -> m b -> m (a -> a -> a)
inf' f bM = do
  b <- bM
  return $ \ aL aR -> f aL b aR

inf :: (Monad m) => (a -> b -> a -> a) -> m b -> Mix m a
inf f bM = Inf $ inf' f bM

infl :: (Monad m) => (a -> b -> a -> a) -> m b -> Mix m a
infl f bM = InfL $ inf' f bM

infr :: (Monad m) => (a -> b -> a -> a) -> m b -> Mix m a
infr f bM = InfR $ inf' f bM

between :: (MonadParser t m) => m () -> m () -> m a -> m a
between alM arM aM = do
  alM
  a <- aM
  arM
  return a

buildMix :: (MonadParser t m) => [m a] -> Map Int [Mix m a] -> m a
buildMix lits lps = case mapRemove lps of
  Nothing -> mconcat lits
  Just ((_i, ms), lps') -> buildLevel (buildMix lits lps') $ splitMixes ms

buildLevel :: (MonadParser t m) => m a -> Level m a -> m a
buildLevel aM l = mconcat
  [ buildMixPre aM $ levelPre l
  , do
      a <- aM
      f <- mconcat
        [ buildMixPost    $ levelPost l
        , buildMixInf  aM $ levelInf  l
        , buildMixInfL aM $ levelInfL l
        , buildMixInfR aM $ levelInfR l
        , return id
        ]
      return $ f a
  ]

buildMixPre :: (MonadParser t m) => m a -> m (a -> a) -> m a
buildMixPre aM preM = do
  ps <- oneOrMoreList preM
  a <- aM
  return $ foldr (.) id ps a

buildMixPost :: (MonadParser t m) => m (a -> a) -> m (a -> a)
buildMixPost postM = do
  ps <- oneOrMoreList postM
  return $ foldl (.) id ps

buildMixInf :: (MonadParser t m) => m a -> m (a -> a -> a) -> m (a -> a)
buildMixInf aM infM = do
  p <- infM
  a2 <- aM
  return $ \ a1 -> p a1 a2

buildMixInfL :: (MonadParser t m) => m a -> m (a -> a -> a) -> m (a -> a)
buildMixInfL aM infLM = do
  ies <- map (\ (f, eR) eL -> eL `f` eR) ^$ oneOrMoreList $ infLM <*> aM
  return $ foldl (flip (.)) id ies

buildMixInfR :: (MonadParser t m) => m a -> m (a -> a -> a) -> m (a -> a)
buildMixInfR aM infRM = do
  ies <- oneOrMoreList $ infRM <*> aM
  return $ \ a1 ->
    let (ies', an) = swizzle (a1, ies)
        ies'' = map (\ (eL, f) eR -> eL `f` eR) ies'
    in foldr (.) id ies'' an
  where
    swizzle :: (a, [(b, a)]) -> ([(a, b)], a)
    swizzle (a, []) = ([], a)
    swizzle (aL, (b, a):bas) =
      let (abs, aR) = swizzle (a, bas) 
      in ((aL, b):abs, aR)


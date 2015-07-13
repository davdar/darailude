module Darkdown.Parser where

import FP.Core
import FP.Console
import FP.String
import FP.IO
import FP.Parser
import FP.DerivingPretty

import qualified Data.Text as Text

blockify :: String -> [String]
blockify = Text.splitOn "\n\n" . subst "\n\n+" "\n\n" . Text.stripEnd

data Inline =
    TextInline String
  | QuoteInline String
  | Newline
makePrettySum ''Inline

parseFirstText :: Parser Char String
parseFirstText = fromChars ^$ oneOrMostList $ satisfies $ not . elemOf "«»<>\n-#" 

parseText :: Parser Char String
parseText = fromChars ^$ oneOrMostList $ satisfies $ not . elemOf "«»<>\n"

parseInnerQuote :: Parser Char String
parseInnerQuote = fromChars ^$ oneOrMostList $ satisfies $ not . elemOf "«»<>"

parseQuote :: Parser Char String
parseQuote = mconcat
  [ do
      void $ lit '«'
      s <- parseInnerQuote
      void $ lit '»'
      return s
  ]

parseFirstInline :: Parser Char Inline
parseFirstInline = mconcat
  [ TextInline ^$ parseFirstText
  , lit '\n' >> return Newline
  ]

parseInline :: Parser Char Inline
parseInline = mconcat
  [ TextInline ^$ parseText
  , QuoteInline ^$ parseQuote
  , lit '\n' >> return Newline
  ]

parsePar :: Parser Char [Inline]
parsePar = do
  s <- parseFirstInline
  ss <- many parseInline
  return $ s:ss

parseHeader :: Parser Char (Int, [Inline])
parseHeader = do
  hashes <- oneOrMostList $ lit '#'
  void $ oneOrMostList $ satisfies $ elemOf " "
  ss <- many parseInline
  return (length hashes, ss)

-- data List = List [([Inline], List
-- parseList :: Parser Char List

-- - x
-- - y
--   y
--   z
--   - z
--   - q
-- - r

test :: IO ()
test = do
  ss <- blockify ^$ readFile "test.darkdown"
  pprint $ mapOnM ss $ \ s -> 
    parseFinal parsePar (toChars s)

-- data Header = Header
--   { headerLevel :: Integer
--   , headerText :: String
--   }
-- makePrettySum ''Header
-- 
-- data Inline =
--     RawInline String
--   | EmphInline Inline
--   | BoldInline Inline
--   | EmphBoldInline Inline
--   | LinkInline Inline
--   | FootnoteInline Inline
--   | CitationInline Inline
--   | ReferenceInline Inline
--   | VerbatimInline String
-- makePrettySum ''Inline
-- 
-- data ListType =
--     Itemize String
--   | Enum
-- makePrettySum ''ListType
-- 
-- data ListEntry = ListEntry
--   { listEntryContents :: [Inline]
--   , listEntryChildren :: [ListEntry]
--   }
-- makePrettySum ''ListEntry
-- 
-- data List = List
--   { listType :: ListType
--   , listItems :: [ListEntry]
--   }
-- makePrettySum ''List
-- 
-- data Block =
--     HeaderBlock Header
--   | TextBlock [Inline]
--   | ListBlock List
--   | VerbatimBlock String
-- makePrettySum ''Block
-- 
-- data Dark = Dark
--   { darkBlocks :: [Block]
--   }
-- 
-- data Chunk = Chunk
--   { chunkIndent :: Integer
--   , chunkContents :: String
--   }
-- makePrettySum ''Chunk
-- 
-- data Section = Section
--   { sectionChunks :: [Chunk]
--   }
-- makePrettySum ''Section
-- 
-- chunkBlock :: Parser Char [Section]
-- chunkBlock = do
--   s <- many $ lit ' '
--   mconcat
--     [ do
--         void $ lit '\n'
--         chunkBlock
--     , do
--         t0 <- satisfies $ not . (?) "\n-"
--         t <- undefined -- fromChars ^$ many $ satisfies $ (/=) '\n'
--         void $ lit '\n'
--         (cs, ss) <- chunkEndline
--         undefined -- return $ Section (Chunk (length s) (cons t0 t) : cs) : ss
--     , end >> return []
--     ]
-- 
-- chunkEndline :: Parser Char ([Chunk], [Section])
-- chunkEndline = do
--   s <- many $ lit ' '
--   mconcat
--     [ do
--         void $ lit '\n'
--         ss <- chunkBlock
--         return ([], ss)
--     , do 
--         t <- fromChars ^$ oneOrMoreList $ satisfies $ (/=) '\n'
--         void $ lit '\n'
--         (cs, ss) <- chunkEndline
--         return (Chunk (length s) t : cs,  ss)
--     , end >> return ([], [])
--     ]
-- 
-- parseHeaderBlock :: Parser Chunk Header
-- parseHeaderBlock = do
--   Chunk i s <- pluck
--   guard $ i == 0
--   sumElim (const mbot) return $ parseFinalOn (toChars s) $ do
--     void $ lit '#'
--     subs <- many $ string "-|" 
--     rest <- many pluck
--     return $ Header (length subs) $ fromChars rest
-- 
-- parseTextBlockAccum :: Parser Chunk String
-- parseTextBlockAccum = concat . intersperse " " ^$ many $ do
--   Chunk i s <- pluck
--   guard $ i == 0
--   return s
-- 
-- data InlineToken =
--     TextInlineToken String
--   | EscapedInlineToken Char
--   | EmphasisInlineToken
--   | BoldInlineToken
--   | BoldEmphInlineToken
--   | LinkInlineToken
--   | FootnoteInlineToken Int
--   | CitationInlineToken
--   | DirectReferenceInlineTokenL
--   | DirectReferenceInlineTokenR
--   | IndirectReferenceInlineTokenL
--   | IndirectReferenceInlineTokenR
--   | VerbatimInlineToken
-- 
-- inlineToken :: Parser Char InlineToken
-- inlineToken = mconcat
--   [ TextInlineToken . fromChars ^$ oneOrMoreList $ satisfies $ not . (?) "+|[]()" ]
-- 
-- parseInline :: Parser Char Inline
-- parseInline = undefined
-- 
-- parseTextBlock :: Parser Chunk [Inline]
-- parseTextBlock = do
--   s <- parseTextBlockAccum
--   sumElim (const mbot) return $ parseFinal (many parseInline) $ toChars s
-- 
-- parseListBlock :: Parser Chunk List
-- parseListBlock = undefined
-- 
-- parseVerbatimBlock :: Parser Chunk String
-- parseVerbatimBlock = undefined
-- 
-- parseChunks :: Parser Chunk Block
-- parseChunks = mconcat
--   [ HeaderBlock ^$ parseHeaderBlock
--   , TextBlock ^$ parseTextBlock
--   , ListBlock ^$ parseListBlock
--   , VerbatimBlock ^$ parseVerbatimBlock
--   ]
-- 
-- parse :: String -> Doc :+: Dark
-- parse s = do
--   ss <- mapInl pretty $ parseFinal chunkBlock $ toChars s
--   Dark ^$ mapOnM ss $ \ (Section cs) ->
--     mapInl pretty $ parseFinal parseChunks cs

{-

# Phase 1

Support:

- paragraphs: text blocked by newlines
- lists: text blocked by newlines where each line begins with «-»
- blocks: text blocked by newlines and indented by 4
- custom blocks: a block preceded by a metadata section

-}

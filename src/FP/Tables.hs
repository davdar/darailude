module FP.Tables where

import FP.Core
import FP.Deriving.Pretty
import FP.IO

import qualified Data.Text as Text

data Table = Table
  { tableColumnNames :: [String]
  , tableRows :: [[(String, String)]]
  , tableColumns :: [(String, [String])]
  } deriving (Eq, Ord)

makePrettyUnion ''Table

quad :: String
quad = "    "

spiltOnQuad :: String -> [String]
spiltOnQuad = filter (not . Text.null) . map Text.strip . Text.splitOn quad

mkTable :: (MonadError String m, Monad m) => [String] -> [[String]] -> m Table
mkTable header rows = maybeError "internalError:mkTable: header different length than cols" $ do
  hrows <- mapM (zip header) rows
  hcols <- zip header $ transpose rows
  return $ Table header hrows hcols

parseTable :: (MonadError String m, Monad m) => String -> m Table
parseTable s = do
  let noHeaderMsg = "There must be at least two lines: a header and a separator" 
  (rawHeader, separator, rawRows) <- maybeError noHeaderMsg $ view cons2L $ Text.lines s
  when (not $ Text.all (== '=') $ Text.strip separator) $
    throw "separator is malformed, should be whitespace followed by repeated '=' followed by whitespace"
  let header = spiltOnQuad rawHeader
      rows = map spiltOnQuad rawRows
      hlen :: Int
      hlen = length header
  traverseOn (listWithIndex rows) $ uncurry $ \ (l :: Int) rowData ->
    when (length rowData /= hlen) $
      throw $ "row " ++ show l ++ " doesn't have same number of cols as the header"
  mkTable header rows

parseTableFromFile :: String -> IO Table
parseTableFromFile = (sumElim failIO return . parseTable) *. readFile

checkTable :: String -> Maybe String
checkTable = sumElim Just (const Nothing) . parseTable

checkTableIO :: String -> IO ()
checkTableIO fp = do
  printLn $ "checking " ++ fp
  print " ... "
  s <- readFile fp
  case checkTable s of
    Just msg -> printLn $ "BAD " ++ msg
    Nothing -> printLn "OK"

printTable :: (MonadError String m, Monad m) => Table -> m String
printTable (Table header rows cols) = do
  let columnWidths :: [Int]
      columnWidths = map (\(hdr, vals) -> findMax' $ map Text.length $ hdr:vals) cols
      separatorLen :: Int
      separatorLen = sum columnWidths + toi 4 * (length columnWidths - toi 1)
  headerWithWidths <- maybeError "table malformed" $ zip columnWidths header
  rowsWithWidths <- maybeError "table malformed" $ mapM (zip columnWidths . map snd) rows
  return $ concat $ intersperse "\n" $ concat
    [ single $ Text.intercalate quad $ map (uncurry pad) headerWithWidths
    , single $ Text.replicate separatorLen "="
    , map (concat . intersperse quad) $ map (map $ uncurry pad) rowsWithWidths
    ]
  where
    pad :: Int -> String -> String
    pad i s = s `Text.append` Text.replicate (i - Text.length s) " "

printTableToFile :: String -> Table -> IO ()
printTableToFile fp t = sumElim failIO (writeFile fp) $ printTable t

tidyTableIO :: String -> IO ()
tidyTableIO fp = do
  printLn $ "checking " ++ fp
  print " ... "
  s <- readFile fp
  case parseTable s of
    Inl msg -> printLn $ "BAD " ++ msg
    Inr t -> do
      tp <- sumElim failIO return $ printTable t
      if tp /= s
      then do
        printLn "TIDYING"
        writeFile fp tp
      else do
        printLn "OK"

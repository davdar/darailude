import Control.Monad
import Data.List
import Data.Maybe
import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Language.Haskell.Extension
import System.Directory
import System.IO

main :: IO ()
main = do
  fs <- liftM (filter (".cabal" `isSuffixOf`)) $ getDirectoryContents "."
  case fs of
    [] -> putStrLn "EnvSetup: no cabal file"
    [fn] -> do
      putStrLn $ "EnvSetup: loading " ++ fn
      envSetup fn
    _ -> putStrLn "EnvSetup: more than one cabal file??"

envSetup :: String -> IO ()
envSetup fn = do
  sandboxExists <- doesDirectoryExist ".cabal-sandbox"
  packageFileM <- 
    if sandboxExists 
    then do
      dc <- liftM (filter ("packages.conf.d" `isSuffixOf`)) $ getDirectoryContents ".cabal-sandbox"
      case dc of
        [] -> do
          putStrLn $ "EnvSetup: no cabal sandbox package db" 
          return Nothing
        [f] -> do
          putStrLn $ "EnvSetup: found cabal sandbox package db: " ++ f
          return $ Just f
        _ -> do
          putStrLn $ "EnvSetup: more than one cabal sandbox package db??"
          return Nothing
    else return Nothing
  p <- readPackageDescription silent fn
  let bi = libBuildInfo $ condTreeData $ fromJust $ condLibrary p
  -- EXTENSIONS --
  let extensions = catMaybes $ map enabledOnly $ defaultExtensions bi
  -- write out a file listing all extensions
  withFile ".extensions" WriteMode $ \ h -> do
    forM_ extensions $ \ e -> do
      hPutStrLn h $ show e
  -- write out a file for ghci to load extensions
  withFile ".extensions.init.ghci" WriteMode $ \ h -> do
    forM_ extensions $ \ e -> do
      hPutStrLn h $ ":set -X" ++ show e
  -- write out a file for hdevtools to load extensions
  withFile ".extensions.hdev" WriteMode $ \ h -> do
    forM_ extensions $ \ e -> do
      hPutStrLn h $ "-g-X" ++ show e
  -- write out a file for ghc to load extensions
  withFile ".extensions.ghc" WriteMode $ \ h -> do
    forM_ extensions $ \ e -> do
      hPutStr h $ " -X" ++ show e
  -- OPTIONS --
  let ops = fromJust $ lookup GHC $ options bi
  withFile ".ghc_options.ghc" WriteMode $ \ h -> do
    forM_ ops $ \ o -> do
      hPutStr h $ o ++ " "
    case packageFileM of
      Nothing -> return ()
      Just packageFile -> hPutStr h $ "-package-db .cabal-sandbox/" ++ packageFile
  -- write out a file for ghci to load options
  withFile ".ghc_options.init.ghci" WriteMode $ \ h -> do
    forM_ ops $ \ o -> do
      hPutStrLn h $ ":set " ++ o
    case packageFileM of
      Nothing -> return ()
      Just packageFile -> hPutStrLn h $ ":set -package-db " ++ packageFile
  -- write out a file for hdevtools to load options
  withFile ".ghc_options.hdev" WriteMode $ \ h -> do
    forM_ ops $ \ o -> do
      hPutStrLn h $ "-g" ++ o
  return ()
  where
    enabledOnly (EnableExtension s) = Just s
    enabledOnly _ = Nothing

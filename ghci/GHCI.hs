{-# LANGUAGE ImplicitPrelude, NoRebindableSyntax, NoOverloadedStrings #-}

module GHCI where

import Control.Monad
import Data.List
import System.Directory
import System.Environment

filesForInit :: IO [String]
filesForInit = liftM (sort . filter (".init.ghci" `isSuffixOf`)) $ getDirectoryContents "."

filesForReload :: IO [String]
filesForReload = liftM (sort . filter (".reload.ghci" `isSuffixOf`)) $ getDirectoryContents "."

commands :: [IO String] -> IO String
commands = liftM (intercalate "\n") . sequence

loadGHCI :: Maybe String -> IO String
loadGHCI mM = do
  e <- System.Environment.getEnvironment
  case lookup "GHCI_LOAD" e of 
    Nothing -> case mM of
      Nothing -> return ""
      Just m -> do
        return $ ":load " ++ m
    Just f -> do
      putStrLn $ "LOADING " ++ f
      case mM of
        Nothing -> return $ ":load " ++ f
        Just m -> do
          return $ ":load " ++ f ++ " " ++ m ++ "\n:module + " ++ m

sourceGHCI :: String -> IO String
sourceGHCI f = do
  b <- doesFileExist f
  when b $ putStrLn $ "SOURCING " ++ f
  if b
    then readFile f
    else return ""

initGHCI :: String -> IO String
initGHCI = const $ commands . map sourceGHCI =<< filesForInit

llGHCI :: Maybe String -> String -> IO String
llGHCI mM = const $ commands . (loadGHCI mM:) . map sourceGHCI =<< filesForReload

rlGHCI :: String -> IO String
rlGHCI = const $ commands . (return ":re":) . map sourceGHCI =<< filesForReload

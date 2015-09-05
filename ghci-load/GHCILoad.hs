{-# LANGUAGE UnicodeSyntax #-}

module GHCILoad where

import Control.Monad
import Data.List
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Compiler
import Distribution.Verbosity
import Language.Haskell.Extension
import System.Directory
import System.Environment
import System.Process
import System.Exit
import System.IO

helpInfo ∷ String
helpInfo = intercalate "\n"
  [ "usage:"
  , "<cmd> ops"
  , "  -- output command-line options for sandbox package-d and cabal file extensions and options"
  , "<cmd> launch [<path>]"
  , "  -- execute ghci with sandbox package-d, cabal file extensions and options,"
  , "  -- and load <path> (if provided)."
  , "<cmd> help"
  , "  -- show this message"
  ]
  

main ∷ IO ExitCode
main = do
  args ← getArgs
  case args of
    ["help"] → do
      putStrLn helpInfo
      exitWith ExitSuccess
    ["ops"] → do
      bi ← getBuildInfo
      ppM ← getPackagePath
      putStr $ intercalate " " $ concat
        [ [extensionFlags bi]
        , [optionFlags bi]
        , packageFlag ppM
        ]
      exitWith ExitSuccess 
    _ → do
      putStrLn "BAD COMMAND FORMAT"
      putStrLn helpInfo
      exitWith ExitSuccess

getLocalCabalFile ∷ IO String
getLocalCabalFile = do
  fs ← liftM (filter (".cabal" `isSuffixOf`)) $ getDirectoryContents "."
  case fs of
    [] → fail "no local cabal file"
    [fn] → return fn
    fs → fail $ "multiple cabal files: " ++ show fs

getBuildInfo ∷ IO BuildInfo
getBuildInfo = do
  fn ← getLocalCabalFile
  gp ← readPackageDescription silent fn
  case (libBuildInfo . condTreeData) `fmap` condLibrary gp of
    Nothing → fail "no library in cabal file"
    Just bi → return bi

getPackagePath ∷ IO (Maybe String)
getPackagePath = do
  (code,out,err) ← readCreateProcessWithExitCode (shell "cat cabal.sandbox.config | grep package-db") ""
  case code of
    ExitSuccess → case words out of
      [_,path] → return $ Just path
    ExitFailure _ → return Nothing

extensionCommand ∷ Extension → String
extensionCommand (EnableExtension ex) = show ex
extensionCommand (DisableExtension ex) = "No" ++ show ex

extensionFlags ∷ BuildInfo → String
extensionFlags = intercalate " " . map (\ s → "-X" ++ extensionCommand s) . defaultExtensions

optionFlags ∷ BuildInfo → String
optionFlags bi =
  let ops = case lookup GHC $ options bi of
        Nothing → []
        Just os → os
  in intercalate " " ops

packageFlag ∷ Maybe String → [String]
packageFlag ppM = case ppM of
  Nothing → []
  Just pp → ["-package-db=" ++ pp]

ghciExtensionCommands ∷ BuildInfo → String
ghciExtensionCommands = intercalate "\n" . map (\ s → ":set -X" ++ extensionCommand s) . defaultExtensions

ghciOptionCommands ∷ BuildInfo → String
ghciOptionCommands bi = 
  let ops = case lookup GHC $ options bi of
        Nothing → []
        Just os → os
  in intercalate "\n" $ map (\ s → ":set " ++ show s) ops

ghciPrettyPrintCommand ∷ String → String
ghciPrettyPrintCommand pp = ":set -interactive-print=" ++ pp

ghciLoadCommand ∷ String → String
ghciLoadCommand m = ":load " ++ m

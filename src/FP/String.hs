module FP.String where

import FP.Core

import Text.Regex.TDFA ((=~))
import qualified Data.Text.Encoding as Text

match :: String -> String -> (String, String, String)
match pat t = 
  let (before, m, after) = Text.encodeUtf8 t =~ toChars pat 
  in (Text.decodeUtf8 before, Text.decodeUtf8 m, Text.decodeUtf8 after)

matches :: String -> String -> Bool
matches pat t = Text.encodeUtf8 t =~ toChars pat

subst :: String -> String -> String -> String
subst pat replacement t =
  let (before, m, after) = match pat t
  in if m == "" then t else before ++ replacement ++ subst pat replacement after

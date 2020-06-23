{-# LANGUAGE RecordWildCards #-}

-- | Generate a markdown that summarizes the builtin hints.
module Test.Summary where

import qualified Data.Map as Map
import Config.Type
import Test.Util

genBuiltinSummaryMd :: BuiltinSummary -> String
genBuiltinSummaryMd builtins = unlines $
  [ "# Built-in Hints"
  , ""
  , "This page is auto-generated from `cabal run hlint test -- --generate-summary`"
  , "or `stack run hlint test -- --generate-summary`."
  , ""
  ]
  ++ table builtins

table :: BuiltinSummary -> [String]
table builtins =
  ["<table>"]
  ++ row ["<th>Hint</th>", "<th>Severity</th>", "<th>Support Refactoring?</th>"]
  ++ Map.foldMapWithKey showHint builtins
  ++ ["</table>"]

row :: [String] -> [String]
row xs = ["<tr>"] ++ xs ++ ["</tr>"]

-- | Render using <code> if it is single-line, otherwise using <pre>.
haskell :: String -> [String]
haskell s
  | '\n' `elem` s = ["<pre>", s, "</pre>"]
  | otherwise = ["<code>", s, "</code>", "<br>"]

showHint :: (String, Severity, Refactor) -> BuiltinEx -> [String]
showHint (hint, sev, refact) BuiltinEx{..} = row1 ++ row2
  where
    row1 = row
      [ "<td rowspan=2>" ++ hint ++ "</td>"
      , "<td>" ++ show sev ++ "</td>"
      , "<td>" ++ show refact ++ "</td>"
      ]
    row2 = row example
    example =
      [ "<td colspan=2>"
      , "Example:"
      ]
      ++ haskell builtinInp
      ++ ["Found:"]
      ++ haskell builtinFrom
      ++ ["Suggestion:"]
      ++ haskell to
      ++ ["</td>"]
    to = case builtinTo of
      Nothing -> ""
      Just "" -> "Perhaps you should remove it."
      Just s -> s

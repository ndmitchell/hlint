{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module Summary (generateMdSummary, generateJsonSummary, generateExhaustiveConfig) where

import Data.Map qualified as Map
import Control.Monad.Extra
import System.FilePath
import Data.List.Extra
import System.Directory

import Idea
import Apply
import Hint.Type
import Hint.All
import Config.Type
import Test.Annotations
import Deriving.Aeson
import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)

data Summary = Summary
  { sBuiltinRules :: ![BuiltinHint]
  , sLhsRhsRules :: ![HintRule]
  } deriving (Show, Generic)
  deriving (ToJSON) via CustomJSON '[FieldLabelModifier (StripPrefix "s", CamelToSnake)] Summary

data BuiltinHint = BuiltinHint
  { hName :: !String
  , hSeverity :: !Severity
  , hRefactoring :: !Bool
  , hCategory :: !String
  , hExamples :: ![BuiltinExample]
  } deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON) via CustomJSON '[FieldLabelModifier (StripPrefix "h", CamelToSnake)] BuiltinHint

data BuiltinKey = BuiltinKey
  { kName :: !String
  , kSeverity :: !Severity
  , kRefactoring :: !Bool
  , kCategory :: !String
  } deriving (Show, Eq, Ord)

data BuiltinExample = BuiltinExample
    { eContext :: !String
    , eFrom :: !String
    , eTo :: !(Maybe String)
    } deriving (Show, Eq, Ord, Generic)
    deriving (ToJSON) via CustomJSON '[FieldLabelModifier (StripPrefix "e", CamelToSnake)] BuiltinExample

dedupBuiltin :: [(BuiltinKey, BuiltinExample)] -> [BuiltinHint]
dedupBuiltin = fmap makeHint . Map.toAscList . Map.fromListWith (<>) . fmap exampleToList where
  exampleToList (k, e) = (k, [e])
  makeHint (BuiltinKey{..}, examples) = BuiltinHint
    kName
    kSeverity
    kRefactoring
    kCategory
    examples

-- | The summary of built-in hints is generated by running the test cases in
-- @src/Hint/*.hs@.
mkBuiltinSummary :: IO [BuiltinHint]
mkBuiltinSummary = concatForM builtinHints $ \(category, hint) -> do
    let file = "src/Hint" </> category <.> "hs"
    b <- doesFileExist file
    if not b then do
        putStrLn $ "Couldn't find source hint file " ++ file ++ ", some hints will be missing"
        pure []
     else do
        tests <- parseTestFile file
        fmap dedupBuiltin <$> concatForM tests $ \(TestCase _ _ inp _ _) -> do
            m <- parseModuleEx defaultParseFlags file (Just inp)
            pure $ case m of
                Right m -> map (ideaToValue category inp) $ applyHints [] hint [m]
                Left _ -> []
    where
        ideaToValue :: String -> String -> Idea -> (BuiltinKey, BuiltinExample)
        ideaToValue category inp Idea{..} = (k, v)
            where
                -- make sure Windows/Linux don't differ on path separators
                to = fmap (\x -> if "Combine with " `isPrefixOf` x then replace "\\" "/" x else x) ideaTo
                k = BuiltinKey ideaHint ideaSeverity (notNull ideaRefactoring) category
                v = BuiltinExample inp ideaFrom to

getSummary :: [Setting] -> IO Summary
getSummary settings = do
  builtinHints <- mkBuiltinSummary
  let lhsRhsHints = [hint | SettingMatchExp hint <- settings]
  pure $ Summary builtinHints lhsRhsHints

jsonToString :: ToJSON a => a -> String
jsonToString = unpack . toStrict . encode

-- | Generate a summary of hints, including built-in hints and YAML-configured hints
generateMdSummary :: [Setting] -> IO String
generateMdSummary = fmap genSummaryMd . getSummary

generateJsonSummary :: [Setting] -> IO String
generateJsonSummary = fmap jsonToString . getSummary

generateExhaustiveConfig :: Severity -> [Setting] -> IO String
generateExhaustiveConfig severity = fmap (genExhaustiveConfig severity) . getSummary

genExhaustiveConfig :: Severity -> Summary -> String
genExhaustiveConfig severity Summary{..} = unlines $
  [ "# HLint configuration file"
  , "# https://github.com/ndmitchell/hlint"
  , "##########################"
  , ""
  , "# This file contains a template configuration file, which is typically"
  , "# placed as .hlint.yaml in the root of your project"
  , ""
  , "# All built-in hints"
  ]
    ++ (mkLine <$> sortDedup (hName <$> sBuiltinRules))
    ++ ["", "# All LHS/RHS hints"]
    ++ (mkLine <$> sortDedup (hintRuleName <$> sLhsRhsRules))
  where
    sortDedup = fmap head . group . sort
    mkLine name = "- " <> show severity <> ": {name: " <> jsonToString name <> "}"

genSummaryMd :: Summary -> String
genSummaryMd Summary{..} = unlines $
  [ "# Summary of Hints"
  , ""
  , "This page is auto-generated from `hlint --generate-summary`."
  ] ++
  concat ["" : ("## Builtin " ++ group ) : "" : builtinTable hints | (group, hints) <- groupHintsByCategory sBuiltinRules] ++
  [ ""
  , "## Configured hints"
  , ""
  ]
  ++ lhsRhsTable sLhsRhsRules
  where
    groupHintsByCategory = Map.toAscList . Map.fromListWith (<>) . fmap keyCategory
    keyCategory hint = (hCategory hint, [hint])

row :: [String] -> [String]
row xs = ["<tr>"] ++ xs ++ ["</tr>"]

-- | Render using <code> if it is single-line, otherwise using <pre>.
haskell :: String -> [String]
haskell s
  | '\n' `elem` s = ["<pre>", s, "</pre>"]
  | otherwise = ["<code>", s, "</code>", "<br>"]

builtinTable :: [BuiltinHint] -> [String]
builtinTable builtins =
  ["<table>"]
  ++ row ["<th>Hint Name</th>", "<th>Hint</th>", "<th>Severity</th>"]
  ++ concatMap showBuiltin builtins
  ++ ["</table>"]

showBuiltin :: BuiltinHint -> [String]
showBuiltin BuiltinHint{..} = row1
  where
    row1 = row $
      [ "<td>" ++ hName ++ "</td>", "<td>"]
      ++ showExample (head hExamples)
      ++ ["Does not support refactoring." | not hRefactoring]
      ++ ["</td>"] ++
      [ "<td>" ++ show hSeverity ++ "</td>"
      ]
    showExample BuiltinExample{..} =
      ["Example: "]
        ++ haskell eContext
        ++ ["Found:"]
        ++ haskell eFrom
        ++ ["Suggestion:"]
        ++ haskell eTo'
      where
      eTo' = case eTo of
        Nothing -> ""
        Just "" -> "Perhaps you should remove it."
        Just s -> s

lhsRhsTable :: [HintRule] -> [String]
lhsRhsTable hints =
  ["<table>"]
  ++ row ["<th>Hint Name</th>", "<th>Hint</th>", "<th>Severity</th>"]
  ++ concatMap showLhsRhs hints
  ++ ["</table>"]

showLhsRhs :: HintRule -> [String]
showLhsRhs HintRule{..} = row $
  [ "<td>" ++ hintRuleName ++ "</td>"
  , "<td>"
  , "LHS:"
  ]
  ++ haskell (show hintRuleLHS)
  ++ ["RHS:"]
  ++ haskell (show hintRuleRHS)
  ++
  [ "</td>"
  , "<td>" ++ show hintRuleSeverity ++ "</td>"
  ]

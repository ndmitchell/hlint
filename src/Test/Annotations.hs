{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}

-- | Check the <TEST> annotations within source and hint files.
module Test.Annotations(testAnnotations) where

import Control.Exception.Extra
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Either.Extra
import Data.Function
import Data.Functor
import Data.List.Extra
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra
import Data.Yaml
import System.Exit
import System.FilePath
import System.IO.Extra
import HSE.All
import qualified Data.ByteString.Char8 as BS

import Config.Type
import Idea
import Apply
import Extension
import Refact
import Test.Util
import Prelude
import Config.Yaml
import FastString

import GHC.Util
import SrcLoc
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

-- Input, Output
-- Output = Nothing, should not match
-- Output = Just xs, should match xs
data TestCase = TestCase SrcLoc Refactor String (Maybe String) [Setting] deriving (Show)

data Refactor = TestRefactor | SkipRefactor deriving (Eq, Show)

testAnnotations :: [Setting] -> FilePath -> Maybe FilePath -> Test ()
testAnnotations setting file rpath = do
    tests <- liftIO $ parseTestFile file
    mapM_ f tests
    where
        f (TestCase loc refact inp out additionalSettings) = do
            ideas <- liftIO $ try_ $ do
                res <- applyHintFile defaultParseFlags (setting ++ additionalSettings) file $ Just inp
                evaluate $ length $ show res
                pure res

            -- the hints from data/Test.hs are really fake hints we don't actually deploy
            -- so don't record them
            when (takeFileName file /= "Test.hs") $
                either (const $ pure ()) addIdeas ideas

            let good = case (out, ideas) of
                    (Nothing, Right []) -> True
                    (Just x, Right [idea]) | match x idea -> True
                    _ -> False
            let bad =
                    [failed $
                        ["TEST FAILURE (" ++ show (either (const 1) length ideas) ++ " hints generated)"
                        ,"SRC: " ++ unsafePrettyPrint loc
                        ,"INPUT: " ++ inp] ++
                        map ("OUTPUT: " ++) (either (pure . show) (map show) ideas) ++
                        ["WANTED: " ++ fromMaybe "<failure>" out]
                        | not good] ++
                    [failed
                        ["TEST FAILURE (BAD LOCATION)"
                        ,"SRC: " ++ unsafePrettyPrint loc
                        ,"INPUT: " ++ inp
                        ,"OUTPUT: " ++ show i]
                        | i@Idea{..} <- fromRight [] ideas, let SrcLoc{..} = srcSpanStart ideaSpan, srcFilename == "" || srcLine == 0 || srcColumn == 0]
                        -- TODO: shouldn't these checks be == -1 instead?

            -- Skip refactoring test if the hlint test failed, or if the
            -- test is annotated with @NoRefactor.
            let skipRefactor = notNull bad || refact == SkipRefactor
            badRefactor <- if skipRefactor then pure [] else liftIO $ do
                refactorErr <- case ideas of
                    Right [] -> testRefactor rpath Nothing inp
                    Right [idea] -> testRefactor rpath (Just idea) inp
                    -- Skip refactoring test if there are multiple hints
                    _ -> pure []
                pure $ [failed $
                           ["TEST FAILURE (BAD REFACTORING)"
                           ,"SRC: " ++ unsafePrettyPrint loc
                           ,"INPUT: " ++ inp] ++ refactorErr
                           | notNull refactorErr]

            if null bad && null badRefactor then passed else sequence_ (bad ++ badRefactor)

        match "???" _ = True
        match (word1 -> ("@Message",msg)) i = ideaHint i == msg
        match (word1 -> ("@Note",note)) i = map show (ideaNote i) == [note]
        match "@NoNote" i = null (ideaNote i)
        match (word1 -> ('@':sev, msg)) i = sev == show (ideaSeverity i) && match msg i
        match msg i = on (==) norm (fromMaybe "" $ ideaTo i) msg

        -- FIXME: Should use a better check for expected results
        norm = filter $ \x -> not (isSpace x) && x /= ';'


parseTestFile :: FilePath -> IO [TestCase]
parseTestFile file =
    -- we remove all leading # symbols since Yaml only lets us do comments that way
    f Nothing TestRefactor . zipFrom 1 . map (dropPrefix "# ") . lines <$> readFile file
    where
        open :: String -> Maybe [Setting]
        open line
          |  "<TEST>" `isPrefixOf` line =
             let suffix = dropPrefix "<TEST>" line
                 config = decodeEither'  $ BS.pack suffix
             in case config of
                  Left err -> Just []
                  Right config -> Just $ settingsFromConfigYaml [config]
          | otherwise = Nothing

        shut :: String -> Bool
        shut = isPrefixOf "</TEST>"

        f :: Maybe [Setting] -> Refactor -> [(Int, String)] -> [TestCase]
        f Nothing _ ((i,x):xs) = f (open x) TestRefactor xs
        f (Just s) refact ((i,x):xs)
            | shut x = f Nothing TestRefactor xs
            | Just (x',_) <- stripInfix "@NoRefactor" x =
                f (Just s) SkipRefactor ((i, trimEnd x' ++ ['\\' | "\\" `isSuffixOf` x]) : xs)
            | null x || "-- " `isPrefixOf` x = f (Just s) refact xs
            | Just x <- stripSuffix "\\" x, (_,y):ys <- xs = f (Just s) refact $ (i,x++"\n"++y):ys
            | otherwise = parseTest refact file i x s : f (Just s) TestRefactor xs
        f _ _ [] = []


parseTest :: Refactor -> String -> Int -> String -> [Setting] -> TestCase
parseTest refact file i x = uncurry (TestCase (mkSrcLoc (mkFastString file) i 0) refact) $ f x
    where
        f x | Just x <- stripPrefix "<COMMENT>" x = first ("--"++) $ f x
        f (' ':'-':'-':xs) | null xs || " " `isPrefixOf` xs = ("", Just $ trimStart xs)
        f (x:xs) = first (x:) $ f xs
        f [] = ([], Nothing)


-- Returns an empty list if the refactoring test passes, otherwise
-- returns error messages.
testRefactor :: Maybe FilePath -> Maybe Idea -> String -> IO [String]
-- Skip refactoring test if the refactor binary is not found.
testRefactor Nothing _ _ = pure []
testRefactor fp (Just idea) inp
  -- Skip refactoring test if the hint has no suggestion (e.g., a parse error).
  | isNothing (ideaTo idea) = pure []
  | null (ideaRefactoring idea) =
      if ideaHint idea `Set.member` noRefactoringHints
        -- If a hint does not support refactoring, test refactoring as if there's no hint
        then testRefactor fp Nothing inp
        else pure ["Hint \"" ++ ideaHint idea ++ "\" does not support refactoring. Add it to Test.Annotations.noRefactoringHints."]
testRefactor (Just rpath) midea inp = withTempFile $ \tempInp -> withTempFile $ \tempHints -> do
    -- Note that we test the refactoring even if there are no suggestions,
    -- as an extra test of apply-refact, on which we rely.
    -- See https://github.com/ndmitchell/hlint/issues/958 for a discussion.
    let refacts = map (show &&& ideaRefactoring) (maybeToList midea)
        -- Ignores spaces and semicolons because apply-refact may disagree
        -- with unsafePrettyPrint on spaces and semicolons.
        process = filter (\c -> not (isSpace c) && c /= ';')
        matched expected g actual = process expected `g` process actual
        x `isProperSubsequenceOf` y = x /= y && x `isSubsequenceOf` y
    writeFile tempInp inp
    writeFile tempHints (show refacts)
    exitCode <- runRefactoring rpath tempInp tempHints defaultExtensions [] "--inplace"
    refactored <- readFile tempInp
    pure $ case exitCode of
        ExitFailure ec -> ["Refactoring failed: exit code " ++ show ec]
        ExitSuccess -> case fmap ideaTo midea of
            -- No hints. Refactoring should be a no-op.
            Nothing | not (matched inp (==) refactored) ->
                ["Expected refactor output: " ++ inp, "Actual: " ++ refactored]
            -- The hint's suggested replacement is @Just ""@, which means the hint
            -- suggests removing something from the input. The refactoring output
            -- should be a proper subsequence of the input.
            Just (Just "") | not (matched refactored isProperSubsequenceOf inp) ->
                ["Refactor output is expected to be a proper subsequence of: " ++ inp, "Actual: " ++ refactored]
            -- The hint has a suggested replacement. The suggested replacement
            -- should be a substring of the refactoring output.
            Just (Just to) | not (matched to isInfixOf refactored) ->
                ["Refactor output is expected to contain: " ++ to, "Actual: " ++ refactored]
            _ -> []

-- | Hints in this list don't support refactoring either at all, or sometimes.
-- For example, the "Redundant return" hint sometimes supports refactoring, sometimes doesn't.
noRefactoringHints :: Set String
noRefactoringHints = Set.fromList
  [ "Avoid lambda"
  , "Eta reduce"
  , "Reduce duplication"
  , "Redundant as-pattern"
  , "Redundant return"
  , "Redundant section"
  , "Redundant variable capture"
  , "Redundant void"
  , "Redundant where"
  , "Use camelCase"
  , "Use explicit module export list"
  , "Use lambda"
  , "Use lambda-case"
  , "Use module export list"
  , "Use newtype instead of data"
  , "Use section"
  , "Use tuple-section"
  , "Used otherwise as a pattern"
  ]

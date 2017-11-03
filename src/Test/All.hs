{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Test.All(test) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import Data.Functor
import Prelude

import Config.Type
import Config.Read
import CmdLine
import HSE.All
import Hint.All
import Test.Util
import Test.InputOutput
import Test.Annotations
import Test.Translate
import System.IO.Extra


test :: Cmd -> ([String] -> IO ()) -> FilePath -> [FilePath] -> IO Int
test CmdTest{..} main dataDir files = withBuffering stdout NoBuffering $ withTests $ do
    hasSrc <- doesFileExist "hlint.cabal"
    useSrc <- return $ hasSrc && null files
    testFiles <- if files /= [] then return files else do
        xs <- getDirectoryContents dataDir
        return [dataDir </> x | x <- xs, takeExtension x `elem` [".hs",".yml",".yaml"]
                              , not $ "HLint_" `isPrefixOf` takeBaseName x]
    testFiles <- forM testFiles $ \file -> do
        hints <- readFilesConfig [(file, Nothing)]
        return (file, hints ++ (if takeBaseName file /= "Test" then [] else map (Builtin . fst) builtinHints))
    let wrap msg act = putStr (msg ++ " ") >> act >> putStrLn ""

    putStrLn "Testing"
    checkCommentedYaml $ dataDir </> "default.yaml"
    config <- readFilesConfig [(".hlint.yaml",Nothing)]
    when useSrc $ wrap "Source annotations" $
        forM_ builtinHints $ \(name,_) -> do
            progress
            testAnnotations (Builtin name : if name == "Restrict" then config else []) $ "src/Hint" </> name <.> "hs"
    when useSrc $ wrap "Input/outputs" $ testInputOutput main

    wrap "Hint names" $ mapM_ (\x -> do progress; testNames $ snd x) testFiles
    wrap "Hint annotations" $ forM_ testFiles $ \(file,h) -> do progress; testAnnotations h file
    when cmdTypeCheck $ wrap "Hint typechecking" $
        progress >> testTypeCheck cmdDataDir cmdTempDir [h | (file, h) <- testFiles, takeFileName file /= "Test.hs"]
    when cmdQuickCheck $ wrap "Hint QuickChecking" $
        progress >> testQuickCheck cmdDataDir cmdTempDir [h | (file, h) <- testFiles, takeFileName file /= "Test.hs"]

    when (null files && not hasSrc) $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"


---------------------------------------------------------------------
-- VARIOUS SMALL TESTS

testNames :: [Setting] -> IO ()
testNames  hints = sequence_
    [ failed ["No name for the hint " ++ prettyPrint hintRuleLHS ++ " ==> " ++ prettyPrint hintRuleRHS]
    | SettingMatchExp x@HintRule{..} <- hints, hintRuleName == defaultHintName]

checkCommentedYaml :: FilePath -> IO ()
checkCommentedYaml file = do
    src <- lines <$> readFile' file
    let src2 = [x | x <- src, Just x <- [stripPrefix "# " x], not $ all (\x -> isAlpha x || x == '$') $ take 1 x]
    e <- readFilesConfig [(file, Just $ unlines src2)]
    void $ evaluate $ length e

{-# LANGUAGE RecordWildCards #-}

module Test.All(test) where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import Prelude

import Config.Type
import Config.All
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
        hints <- readFileConfig file Nothing
        return (file, hints ++ (if takeBaseName file /= "Test" then [] else map (Builtin . fst) builtinHints))
    let wrap msg act = putStr (msg ++ " ") >> act >> putStrLn ""

    putStrLn "Testing"
    when useSrc $ wrap "Source annotations" $
        forM_ builtinHints $ \(name,_) -> do progress; testAnnotations [Builtin name] $ "src/Hint" </> name <.> "hs"
    when useSrc $ wrap "Input/outputs" $ testInputOutput main

    wrap "Hint names" $ mapM_ (\x -> do progress; testNames $ snd x) testFiles
    wrap "Hint annotations" $ forM_ testFiles $ \(file,h) -> do progress; testAnnotations h file
    when cmdTypeCheck $ wrap "Hint typechecking" $
        progress >> testTypeCheck cmdTempDir [h | (file, h) <- testFiles, takeFileName file /= "Test.hs"]
    when cmdQuickCheck $ wrap "Hint QuickChecking" $
        progress >> testQuickCheck cmdTempDir [h | (file, h) <- testFiles, takeFileName file /= "Test.hs"]

    when (null files && not hasSrc) $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"


---------------------------------------------------------------------
-- VARIOUS SMALL TESTS

testNames :: [Setting] -> IO ()
testNames  hints = sequence_
    [ failed ["No name for the hint " ++ prettyPrint hintRuleLHS ++ " ==> " ++ prettyPrint hintRuleRHS]
    | SettingMatchExp x@HintRule{..} <- hints, hintRuleName == defaultHintName]

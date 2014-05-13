{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

module Test.All(test) where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath

import Settings
import CmdLine
import HSE.All
import Hint.All
import Test.Util
import Test.InputOutput
import Test.Annotations
import Test.Translate


test :: Cmd -> ([String] -> IO ()) -> FilePath -> [FilePath] -> IO Int
test CmdTest{..} main dataDir files = withTests $ do
    hasSrc <- doesFileExist "hlint.cabal"
    useSrc <- return $ hasSrc && null files
    testFiles <- if files /= [] then return files else do
        xs <- getDirectoryContents dataDir
        return [dataDir </> x | x <- xs, takeExtension x == ".hs", not $ "HLint" `isPrefixOf` takeBaseName x]
    testFiles <- forM testFiles $ \file -> fmap ((,) file) $ readSettings2 dataDir [file] []
    let wrap msg act = putStr msg >> act >> putStrLn ""

    when useSrc $ wrap "Testing source annotations: " $
        forM_ builtinHints $ \(name,_) -> testAnnotations [Builtin name] $ "src/Hint" </> name <.> "hs"
    when useSrc $ wrap "Testing input/output pairs: " $ testInputOutput main

    wrap "Testing names in hints: " $ mapM_ (testNames . snd) testFiles
    wrap "Testing annotations in hints: " $ forM_ testFiles $ \(file,h) -> testAnnotations h file
    when cmdTypeCheck $ wrap "Typechecking hints: " $
        forM_ testFiles $ \(file,h) -> when (takeFileName file /= "Test.hs") $ testTypeCheck h

    when (null files && not hasSrc) $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"


---------------------------------------------------------------------
-- VARIOUS SMALL TESTS

testNames :: [Setting] -> IO ()
testNames  hints = sequence_
    [ failed ["No name for the hint " ++ prettyPrint (hintRuleLHS x)]
    | SettingMatchExp x@HintRule{} <- hints, hintRuleName x == defaultHintName]

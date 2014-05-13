{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

module Test.All(test) where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath

import Settings
import HSE.All
import Hint.All
import Test.Util
import Test.InputOutput
import Test.Annotations
import Test.Translate


test :: ([String] -> IO ()) -> FilePath -> [FilePath] -> IO Int
test main dataDir files = withTests $
    if null files then do
        src <- doesFileExist "hlint.cabal"
        sequence_ $ (if src then id else take 1)
            [testHintFiles dataDir, testSourceFiles, testInputOutput main]
        putStrLn ""
        unless src $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"
    else do
        mapM_ (testHintFile dataDir) files


testHintFiles :: FilePath -> IO ()
testHintFiles dataDir = do
    xs <- getDirectoryContents dataDir
    mapM_ (testHintFile dataDir)
        [dataDir </> x | x <- xs, takeExtension x == ".hs", not $ "HLint" `isPrefixOf` takeBaseName x]


testHintFile :: FilePath -> FilePath -> IO ()
testHintFile dataDir file = do
    hints <- readSettings2 dataDir [file] []
    sequence_ $ nameCheckHints hints : checkAnnotations hints file :
                [typeCheckHints hints | takeFileName file /= "Test.hs"]
    progress


testSourceFiles :: IO ()
testSourceFiles = sequence_
    [checkAnnotations [Builtin name] ("src/Hint" </> name <.> "hs") | (name,h) <- builtinHints]

---------------------------------------------------------------------
-- VARIOUS SMALL TESTS

nameCheckHints :: [Setting] -> IO ()
nameCheckHints hints = do
    sequence_ [failed ["No name for the hint " ++ prettyPrint (hintRuleLHS x)] | SettingMatchExp x@HintRule{} <- hints, hintRuleName x == defaultHintName]

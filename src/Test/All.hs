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
test CmdTest{..} main dataDir files = withTests $
    if null files then do
        src <- doesFileExist "hlint.cabal"
        sequence_ $ (if src then id else take 1)
            [testHintFiles cmdTypeCheck dataDir, testSourceFiles, testInputOutput main]
        putStrLn ""
        unless src $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"
    else do
        mapM_ (testHintFile cmdTypeCheck dataDir) files


testHintFiles :: Bool -> FilePath -> IO ()
testHintFiles typecheck dataDir = do
    xs <- getDirectoryContents dataDir
    mapM_ (testHintFile typecheck dataDir)
        [dataDir </> x | x <- xs, takeExtension x == ".hs", not $ "HLint" `isPrefixOf` takeBaseName x]


testHintFile :: Bool -> FilePath -> FilePath -> IO ()
testHintFile typecheck dataDir file = do
    hints <- readSettings2 dataDir [file] []
    sequence_ $ nameCheckHints hints : testAnnotations hints file :
                [testTranslate hints | typecheck, takeFileName file /= "Test.hs"]
    progress


testSourceFiles :: IO ()
testSourceFiles = sequence_
    [testAnnotations [Builtin name] ("src/Hint" </> name <.> "hs") | (name,h) <- builtinHints]

---------------------------------------------------------------------
-- VARIOUS SMALL TESTS

nameCheckHints :: [Setting] -> IO ()
nameCheckHints hints = do
    sequence_ [failed ["No name for the hint " ++ prettyPrint (hintRuleLHS x)] | SettingMatchExp x@HintRule{} <- hints, hintRuleName x == defaultHintName]

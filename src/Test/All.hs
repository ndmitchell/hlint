{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Test.All(test) where

import Control.Exception
import System.Console.CmdArgs
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Either.Extra
import Data.List
import System.Directory
import System.FilePath
import Data.Functor
import Prelude

import Config.Type
import Config.Read
import CmdLine
import Refact
import HSE.All
import Hint.All
import Test.Util
import Test.InputOutput
import Test.Annotations
import Test.Translate
import System.IO.Extra


test :: Cmd -> ([String] -> IO ()) -> FilePath -> [FilePath] -> IO Int
test CmdTest{..} main dataDir files = do
    rpath <- refactorPath (if cmdWithRefactor == "" then Nothing else Just cmdWithRefactor)

    (failures, ideas) <- withBuffering stdout NoBuffering $ withTests $ do
        hasSrc <- liftIO $ doesFileExist "hlint.cabal"
        useSrc <- return $ hasSrc && null files
        testFiles <- if files /= [] then return files else do
            xs <- liftIO $ getDirectoryContents dataDir
            return [dataDir </> x | x <- xs, takeExtension x `elem` [".hs",".yml",".yaml"]
                                , not $ "HLint_" `isPrefixOf` takeBaseName x]
        testFiles <- liftIO $ forM testFiles $ \file -> do
            hints <- readFilesConfig [(file, Nothing)]
            return (file, hints ++ (if takeBaseName file /= "Test" then [] else map (Builtin . fst) builtinHints))
        let wrap msg act = do liftIO $ putStr (msg ++ " "); act; liftIO $ putStrLn ""

        liftIO $ putStrLn "Testing"
        liftIO $ checkCommentedYaml $ dataDir </> "default.yaml"
        when useSrc $ wrap "Source annotations" $ do
            config <- liftIO $ readFilesConfig [(".hlint.yaml",Nothing)]
            forM_ builtinHints $ \(name,_) -> do
                progress
                testAnnotations (Builtin name : if name == "Restrict" then config else [])
                                ("src/Hint" </> name <.> "hs")
                                (eitherToMaybe rpath)
        when useSrc $ wrap "Input/outputs" $ testInputOutput main

        wrap "Hint names" $ mapM_ (\x -> do progress; testNames $ snd x) testFiles
        wrap "Hint annotations" $ forM_ testFiles $ \(file,h) -> do progress; testAnnotations h file (eitherToMaybe rpath)
        let hs = [h | (file, h) <- testFiles, takeFileName file /= "Test.hs"]
        when cmdTypeCheck $ wrap "Hint typechecking" $
            progress >> testTypeCheck cmdDataDir cmdTempDir hs
        when cmdQuickCheck $ wrap "Hint QuickChecking" $
            progress >> testQuickCheck cmdDataDir cmdTempDir hs

        when (null files && not hasSrc) $ liftIO $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"
        getIdeas
    whenLoud $ mapM_ print ideas
    case rpath of
        Left refactorNotFound -> putStrLn $ unlines [refactorNotFound, "Refactoring tests skipped"]
        _ -> return ()
    return failures


---------------------------------------------------------------------
-- VARIOUS SMALL TESTS

-- Check all hints in the standard config files get sensible names
testNames :: [Setting] -> Test ()
testNames hints = sequence_
    [ failed ["No name for the hint " ++ prettyPrint hintRuleLHS ++ " ==> " ++ prettyPrint hintRuleRHS]
    | SettingMatchExp x@HintRule{..} <- hints, hintRuleName == defaultHintName]


-- Check that the default.yaml template I supply is valid when I strip off all the comments, since that's
-- what a user gets with --default
checkCommentedYaml :: FilePath -> IO ()
checkCommentedYaml file = do
    src <- lines <$> readFile' file
    let src2 = [x | x <- src, Just x <- [stripPrefix "# " x], not $ all (\x -> isAlpha x || x == '$') $ take 1 x]
    e <- readFilesConfig [(file, Just $ unlines src2)]
    void $ evaluate $ length e

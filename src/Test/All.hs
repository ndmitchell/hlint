{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Test.All(test) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Either.Extra
import Data.Foldable
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Data.Functor
import Prelude

import Config.Type
import Config.Read
import CmdLine
import Refact
import Hint.All
import Test.Annotations
import Test.InputOutput
import Test.Util
import System.IO.Extra
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable


test :: Cmd -> ([String] -> IO ()) -> FilePath -> [FilePath] -> IO Int
test CmdMain{..} main dataDir files = do
    rpath <- refactorPath (if cmdWithRefactor == "" then Nothing else Just cmdWithRefactor)

    (failures, ideas) <- withBuffering stdout NoBuffering $ withTests $ do
        hasSrc <- liftIO $ doesFileExist "hlint.cabal"
        let useSrc = hasSrc && null files
        testFiles <- if files /= [] then pure files else do
            xs <- liftIO $ getDirectoryContents dataDir
            pure [dataDir </> x | x <- xs, takeExtension x `elem` [".yml",".yaml"]]
        testFiles <- liftIO $ forM testFiles $ \file -> do
            hints <- readFilesConfig [(file, Nothing),("CommandLine.yaml", Just "- group: {name: testing, enabled: true}")]
            pure (file, hints ++ (if takeBaseName file /= "Test" then [] else map (Builtin . fst) builtinHints))
        let wrap msg act = do liftIO $ putStr (msg ++ " "); act; liftIO $ putStrLn ""

        liftIO $ putStrLn $ "Testing (" ++ (if isRight rpath then "with" else "WITHOUT") ++ " refactoring)"
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

        when (null files && not hasSrc) $ liftIO $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"

    case rpath of
        Left refactorNotFound -> putStrLn $ unlines [refactorNotFound, "Refactoring tests skipped"]
        _ -> pure ()
    pure failures


---------------------------------------------------------------------
-- VARIOUS SMALL TESTS

-- Check all hints in the standard config files get sensible names
testNames :: [Setting] -> Test ()
testNames hints = sequence_
    [ failed ["No name for the hint " ++ unsafePrettyPrint hintRuleLHS ++ " ==> " ++ unsafePrettyPrint hintRuleRHS]
    | SettingMatchExp x@HintRule{..} <- hints, hintRuleName == defaultHintName]


-- Check that the default.yaml template I supply is valid when I strip off all the comments, since that's
-- what a user gets with --default
checkCommentedYaml :: FilePath -> IO ()
checkCommentedYaml file = do
    src <- lines <$> readFile' file
    let src2 = [x | x <- src, Just x <- [stripPrefix "# " x], not $ all (\x -> isAlpha x || x == '$') $ take 1 x]
    e <- readFilesConfig [(file, Just $ unlines src2)]
    void $ evaluate $ length e

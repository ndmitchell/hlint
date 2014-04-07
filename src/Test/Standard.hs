{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

module Test.Standard(test) where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO
import System.Cmd
import System.Exit

import Settings
import HSE.All
import Hint.All
import Test.Util
import Test.InputOutput
import Test.Annotations


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


-- | Given a set of hints, do all the HintRule hints type check
typeCheckHints :: [Setting] -> IO ()
typeCheckHints hints = bracket
    (openTempFile "." "hlinttmp.hs")
    (\(file,h) -> removeFile file)
    $ \(file,h) -> do
        hPutStrLn h $ unlines contents
        hClose h
        res <- system $ "runhaskell " ++ file
        progress
        tested $ res == ExitSuccess
    where
        matches = [x | SettingMatchExp x <- hints]

        -- Hack around haskell98 not being compatible with base anymore
        hackImport i@ImportDecl{importAs=Just a,importModule=b}
            | prettyPrint b `elem` words "Maybe List Monad IO Char" = i{importAs=Just b,importModule=a}
        hackImport i = i

        contents =
            ["{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules #-}"] ++
            concat [map (prettyPrint . hackImport) $ scopeImports $ hintRuleScope x | x <- take 1 matches] ++
            ["main = return ()"
            ,"(==>) :: a -> a -> a; (==>) = undefined"
            ,"_noParen_ = id"
            ,"_eval_ = id"] ++
            ["{-# LINE " ++ show (startLine $ ann rhs) ++ " " ++ show (fileName $ ann rhs) ++ " #-}\n" ++
             prettyPrint (PatBind an (toNamed $ "test" ++ show i) Nothing bod Nothing)
            | (i, HintRule _ _ _ lhs rhs side _) <- zip [1..] matches, "notTypeSafe" `notElem` vars (maybeToList side)
            , let vs = map toNamed $ nub $ filter isUnifyVar $ vars lhs ++ vars rhs
            , let inner = InfixApp an (Paren an lhs) (toNamed "==>") (Paren an rhs)
            , let bod = UnGuardedRhs an $ if null vs then inner else Lambda an vs inner]

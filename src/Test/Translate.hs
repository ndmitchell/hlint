{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

-- | Translate the hints to Haskell and run with GHC.
module Test.Translate(testTypeCheck, testQuickCheck) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Cmd
import System.Exit
import System.FilePath

import Paths_hlint
import Settings
import Util
import HSE.All
import Test.Util


runMains :: [String] -> IO ()
runMains xs = withTemporaryFiles "HLint_tmp.hs" (length xs + 1) $ \(root:bodies) -> do
    forM_ (zip bodies xs) $ \(file,x) -> do
        writeFile file $ replace "module Main" ("module " ++ takeBaseName file) x
    let ms = map takeBaseName bodies
    writeFile root $ unlines $
        ["import qualified " ++ m | m <- ms] ++
        ["main = do"] ++
        ["    " ++ m ++ ".main" | m <- ms]
    dat <- getDataDir
    res <- system $ "runhaskell -i" ++ takeDirectory root ++ " -i" ++ dat ++ " " ++ root
    replicateM_ (length xs) $ tested $ res == ExitSuccess


-- | Given a set of hints, do all the HintRule hints type check
testTypeCheck :: [[Setting]] -> IO ()
testTypeCheck = wrap toTypeCheck

-- | Given a set of hints, do all the HintRule hints satisfy QuickCheck
testQuickCheck :: [[Setting]] -> IO ()
testQuickCheck = wrap toQuickCheck

wrap :: ([HintRule] -> [String]) -> [[Setting]] -> IO ()
wrap f hints = runMains [unlines $ body [x | SettingMatchExp x <- xs] | xs <- hints]
    where
        body xs =
            ["{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules, ScopedTypeVariables, DeriveDataTypeable #-}"
            ,"{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}"
            ,"module Main(main) where"] ++
            concat [map (prettyPrint . hackImport) $ scopeImports $ hintRuleScope x | x <- take 1 xs] ++
            f xs

        -- Hack around haskell98 not being compatible with base anymore
        hackImport i@ImportDecl{importAs=Just a,importModule=b}
            | prettyPrint b `elem` words "Maybe List Monad IO Char" = i{importAs=Just b,importModule=a}
        hackImport i = i


---------------------------------------------------------------------
-- TYPE CHECKING

toTypeCheck :: [HintRule] -> [String]
toTypeCheck hints =
    ["import HLint_TypeCheck hiding(main)"
    ,"main = return ()"] ++
    ["{-# LINE " ++ show (startLine $ ann rhs) ++ " " ++ show (fileName $ ann rhs) ++ " #-}\n" ++
     prettyPrint (PatBind an (toNamed $ "test" ++ show i) Nothing bod Nothing)
    | (i, HintRule _ _ _ lhs rhs side _) <- zip [1..] hints, "notTypeSafe" `notElem` vars (maybeToList side)
    , let vs = map toNamed $ nub $ filter isUnifyVar $ vars lhs ++ vars rhs
    , let inner = InfixApp an (Paren an lhs) (toNamed "==>") (Paren an rhs)
    , let bod = UnGuardedRhs an $ if null vs then inner else Lambda an vs inner]


---------------------------------------------------------------------
-- QUICKCHECK

toQuickCheck :: [HintRule] -> [String]
toQuickCheck hints =
    ["import HLint_QuickCheck"
    ,"default(Maybe Bool,Int,Double)"
    ,"main = do " ++ intercalate "; " ["hlintTest " ++ show i ++ " " ++ show n ++ " test" ++ show i | (i,n,_) <- tests]] ++
    map thd3 tests
    where
        tests =
            [(,,) i (prettyPrint lhs ++ " ==> " ++ prettyPrint rhs) $
              prettyPrint (PatBind an (toNamed $ "test" ++ show i) Nothing bod Nothing)
            | (i, HintRule _ _ _ lhs rhs side _) <- zip [1..] hints, "notTypeSafe" `notElem` vars (maybeToList side)
            , i `notElem` ([2,118,139,323,324] ++ [199..251] ++ [41,42,43,44,106])
            , let vs = map toNamed $ nub $ filter isUnifyVar $ vars lhs ++ vars rhs
            , let inner = InfixApp an (Paren an lhs) (toNamed "==>") (Paren an rhs)
            , let bod = UnGuardedRhs an $ if null vs then inner else Lambda an vs inner]

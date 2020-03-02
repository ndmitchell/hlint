
-- | Translate the hints to Haskell and run with GHC.
module Test.Translate(testTypeCheck, testQuickCheck) where

import Config.Type
import Test.Util

testTypeCheck :: FilePath -> FilePath -> [[Setting]] -> Test ()
testTypeCheck _ _ _ = fail "Test.Translate is disabled."

-- | Given a set of hints, do all the HintRule hints satisfy QuickCheck
testQuickCheck :: FilePath -> FilePath -> [[Setting]] -> Test ()
testQuickCheck _ _ _ = fail "Test.Translate is disabled."

{-
import Control.Monad
import Control.Monad.IO.Class
import Data.List.Extra
import System.IO.Extra
import Data.Maybe
import System.Process
import System.Exit
import System.FilePath
import Language.Haskell.Exts.Util(FreeVars, freeVars)
import qualified Data.Set as Set

import Config.Type
import HSE.All
import Test.Util


runMains :: FilePath -> FilePath -> [String] -> Test ()
runMains datadir tmpdir xs = do
    res <- liftIO $ (if tmpdir == "" then withTempDir else ($ tmpdir)) $ \dir -> do
        ms <- forM (zipFrom 1 xs) $ \(i,x) -> do
            let m = "I" ++ show i
            writeFile (dir </> m <.> "hs") $ replace "module Main" ("module " ++ m) x
            pure m
        writeFile (dir </> "Main.hs") $ unlines $
            ["import qualified " ++ m | m <- ms] ++
            ["main = do"] ++
            ["    " ++ m ++ ".main" | m <- ms]
        system $ "runhaskell -i" ++ dir ++ " -i" ++ datadir ++ " Main"
    replicateM_ (length xs) $ tested $ res == ExitSuccess


-- | Given a set of hints, do all the HintRule hints type check
testTypeCheck :: FilePath -> FilePath -> [[Setting]] -> Test ()
testTypeCheck = wrap toTypeCheck

-- | Given a set of hints, do all the HintRule hints satisfy QuickCheck
testQuickCheck :: FilePath -> FilePath -> [[Setting]] -> Test ()
testQuickCheck = wrap toQuickCheck

wrap :: ([HintRule] -> [String]) -> FilePath -> FilePath -> [[Setting]] -> Test ()
wrap f datadir tmpdir hints = runMains datadir tmpdir [unlines $ body [x | SettingMatchExp x <- xs] | xs <- hints]
    where
        body xs =
            ["{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules, ScopedTypeVariables, DeriveDataTypeable #-}"
            ,"{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}"
            ,"module Main(main) where"] ++
            -- concat [map (prettyPrint . hackImport) $ scopeImports $ hintRuleScope x | x <- take 1 xs] ++
            f xs

        {-
        -- Hack around haskell98 not being compatible with base anymore
        hackImport i@ImportDecl{importAs=Just a,importModule=b}
            | prettyPrint b `elem` words "Maybe List Monad IO Char" = i{importAs=Just b,importModule=a}
        hackImport i = i
        -}


---------------------------------------------------------------------
-- TYPE CHECKING

toTypeCheck :: [HintRule] -> [String]
toTypeCheck hints =
    ["import HLint_TypeCheck hiding(main)"
    ,"main = pure ()"] ++
    ["{-# LINE " ++ show (startLine $ ann rhs) ++ " " ++ show (fileName $ ann rhs) ++ " #-}\n" ++
     prettyPrint (PatBind an (toNamed $ "test" ++ show i) bod Nothing)
    | (i, HintRule _ _ lhs rhs side _notes  _ghcScope  _ghcLhs _ghcRhs _ghcSide) <- zipFrom 1 hints, "noTypeCheck" `notElem` vars (maybeToList side)
    , let vs = map toNamed $ nubOrd $ filter isUnifyVar $ vars lhs ++ vars rhs
    , let inner = InfixApp an (Paren an lhs) (toNamed "==>") (Paren an rhs)
    , let bod = UnGuardedRhs an $ if null vs then inner else Lambda an vs inner]


---------------------------------------------------------------------
-- QUICKCHECK

toQuickCheck :: [HintRule] -> [String]
toQuickCheck hints =
    ["import HLint_QuickCheck hiding(main)"
    ,"default(Maybe Bool,Int,Dbl)"
    ,prettyPrint $ PatBind an (toNamed "main") (UnGuardedRhs an $ toNamed "withMain" $$ Do an tests) Nothing]
    where
        str x = Lit an $ String an x (show x)
        int x = Lit an $ Int an (toInteger x) (show x)
        app = App an
        a $$ b = InfixApp an a (toNamed "$") b
        tests =
            [ Qualifier an $
                Let an (BDecls an [PatBind an (toNamed "t") (UnGuardedRhs an bod) Nothing]) $
                (toNamed "test" `app` str (fileName $ ann rhs) `app` int (startLine $ ann rhs) `app`
                 str (prettyPrint lhs ++ " ==> " ++ prettyPrint rhs)) `app` toNamed "t"
            | (i, HintRule _ _ lhs rhs side note _ghcScope _ghcLhs _ghcRhs _ghcSide) <- zipFrom 1 hints, "noQuickCheck" `notElem` vars (maybeToList side)
            , let vs = map (restrict side) $ nubOrd $ filter isUnifyVar $ vars lhs ++ vars rhs
            , let op = if any isRemovesError note then "?==>" else "==>"
            , let inner = InfixApp an (Paren an lhs) (toNamed op) (Paren an rhs)
            , let bod = if null vs then Paren an inner else Lambda an vs inner]

        restrict (Just side) v
            | any (=~= App an (toNamed "isNegZero") (toNamed v)) (universe side) = PApp an (toNamed "NegZero") [toNamed v]
            | any (=~= App an (toNamed "isNat") (toNamed v)) (universe side) = PApp an (toNamed "Nat") [toNamed v]
            | any (=~= App an (toNamed "isCompare") (toNamed v)) (universe side) = PApp an (toNamed "Compare") [toNamed v]
        restrict _ v = toNamed v


isRemovesError :: Note -> Bool
isRemovesError RemovesError{} = True
isRemovesError _ = False

vars :: FreeVars a => a -> [String]
vars  = Set.toList . Set.map prettyPrint . freeVars
-}

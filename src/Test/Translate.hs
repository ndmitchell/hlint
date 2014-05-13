{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

-- | Translate the hints to Haskell and run with GHC.
module Test.Translate(testTypeCheck, testQuickCheck) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Cmd
import System.Exit
import System.FilePath

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
    res <- system $ "runhaskell -i" ++ takeDirectory root ++ " " ++ root
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
    ["main = return ()"
    ,"(==>) :: a -> a -> a; (==>) = undefined"
    ,"_noParen_ = id"
    ,"_eval_ = id"] ++
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
    ["import System.IO.Unsafe"
    ,"import Data.Typeable"
    ,"import Control.Exception"
    ,"import System.IO"
    ,"import Control.Concurrent"
    ,"import System.Mem.Weak"
    ,"import Test.QuickCheck hiding ((==>))"
    ,"default(Maybe Bool,Int,Double)"
    ,"data Test a = a :==> a deriving (Show, Typeable)"
    ,"a ==> b = a :==> b"
    ,"instance Testable2 a => Testable (Test a) where property (x :==> y) = property2 x y"
    ,"class Testable2 a where property2 :: a -> a -> Property"
    ,"instance Eq a => Testable2 a where property2 x y = property $ catcher x == catcher y"
    ,"instance (Arbitrary a, Show a, Testable2 b) => Testable2 (a -> b) where property2 x y = property $ \\a -> property2 (x a) (y a)"
    ,"main = do " ++ intercalate "; " ["hlintTest " ++ show i ++ " " ++ show n ++ " test" ++ show i | (i,n,_) <- tests]
    ,"hlintTest :: (Show p, Testable p, Typeable p) => Int -> String -> p -> IO ()"
    ,"hlintTest i s x = do putStrLn $ \"test\" ++ show i ++ \" :: \" ++ show (typeOf x); putStrLn s; quickCheck x"
    ,"catcher :: a -> Maybe a"
    ,"catcher x = unsafePerformIO $ do"
    ,"    res <- try $ evaluate x"
    ,"    return $ case res of"
    ,"        Left (_ :: SomeException) -> Nothing"
    ,"        Right v -> Just v"
    ,"instance (Show a, Show b) => Show (a -> b) where show x = \"<func>\""
    ,"instance (Show a) => Show (IO a) where show x = \"<IO>\""
    ,"instance Show (Weak a) where show x = \"<Weak>\""
    ,"instance Eq (IO a) where _ == _ = True"
    ,"instance Arbitrary Handle where arbitrary = elements [stdin, stdout, stderr]"
    ,"instance CoArbitrary Handle where coarbitrary _ = variant 0"
    ,"instance Arbitrary IOMode where arbitrary = elements [ReadMode,WriteMode,AppendMode,ReadWriteMode]"
    ,"instance Typeable IOMode where typeOf _ = typeOf ()"
    ,"instance Arbitrary a => Arbitrary (IO a) where arbitrary = fmap return arbitrary"
    ,"instance Eq SomeException where a == b = show a == show b"
    ,"instance Exception (Maybe Bool)"
    ,"instance Arbitrary (Chan a)"
    ,"instance Show (Chan a)"
    ,"_noParen_ = id"
    ,"_eval_ = id"] ++
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

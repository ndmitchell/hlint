{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

-- | Translate the hints to Haskell and run with GHC.
module Test.Translate(testTypeCheck) where

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
testTypeCheck hints = runMains $ map (unlines . toTypeCheck) hints

toTypeCheck :: [Setting] -> [String]
toTypeCheck hints =
        ["{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules #-}"
        ,"module Main(main) where"] ++
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
    where
        matches = [x | SettingMatchExp x <- hints]

        -- Hack around haskell98 not being compatible with base anymore
        hackImport i@ImportDecl{importAs=Just a,importModule=b}
            | prettyPrint b `elem` words "Maybe List Monad IO Char" = i{importAs=Just b,importModule=a}
        hackImport i = i

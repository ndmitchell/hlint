{-# LANGUAGE PatternGuards #-}

module Test(test) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Function
import System.Directory
import System.FilePath
import System.IO
import System.Cmd
import System.Exit

import Settings
import Util
import Idea
import Apply
import HSE.All
import Hint.All

data Result = Result {_failures :: Int, _total :: Int}
pass = Result 0 1
failure = Result 1 1
result x = if x then pass else failure
results = fmap mconcat

instance Monoid Result where
    mempty = Result 0 0
    mappend (Result f1 t1) (Result f2 t2) = Result (f1+f2) (t1+t2)


test :: FilePath -> IO Int
test dataDir = do
    src <- doesFileExist "hlint.cabal"
    Result failures total <- results $ sequence $ (if src then id else take 1)
        [testHintFiles dataDir, testSourceFiles]
    unless src $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"
    if failures == 0
        then putStrLn $ "Tests passed (" ++ show total ++ ")"
        else putStrLn $ "Tests failed (" ++ show failures ++ " of " ++ show total ++ ")"
    return failures


testHintFiles :: FilePath -> IO Result
testHintFiles dataDir = do
    xs <- getDirectoryContents dataDir
    let files = [dataDir </> x | x <- xs, takeExtension x == ".hs", not $ "HLint" `isPrefixOf` takeBaseName x]
    results $ forM files $ \file -> do
        hints <- readSettings dataDir [file]
        results $ sequence [nameCheckHints hints, typeCheckHints hints, checkAnnotations hints file]


testSourceFiles :: IO Result
testSourceFiles = fmap mconcat $ sequence
    [checkAnnotations [Builtin name] ("src/Hint" </> name <.> "hs") | (name,h) <- staticHints]


---------------------------------------------------------------------
-- VARIOUS SMALL TESTS

nameCheckHints :: [Setting] -> IO Result
nameCheckHints hints = do
    let bad = [putStrLn $ "No name for the hint " ++ prettyPrint (lhs x) | x@MatchExp{} <- hints, hintS x == defaultHintName]
    sequence_ bad
    return $ Result (length bad) 0


-- | Given a set of hints, do all the MatchExp hints type check
typeCheckHints :: [Setting] -> IO Result
typeCheckHints hints = bracket
    (openTempFile "." "hlinttmp.hs")
    (\(file,h) -> removeFile file)
    $ \(file,h) -> do
        hPutStrLn h $ unlines contents
        hClose h
        res <- system $ "runhaskell " ++ file
        return $ result $ res == ExitSuccess
    where
        contents =
            ["{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules #-}"
            ,"import System.IO"
            ,"import Control.Arrow"
            ,"import Data.Maybe"
            ,"import Control.Monad"
            ,"import Data.List"
            ,"import Data.Int"
            ,"import Data.Ord"
            ,"import Data.Monoid"
            ,"import Data.Function"
            ,"main = return ()"
            ,"(==>) :: a -> a -> a; (==>) = undefined"
            ,"_noParen_ = id"
            ,"_eval_ = id"
            ,"bad = undefined"] ++
            [prettyPrint $ PatBind an (toNamed $ "test" ++ show i) Nothing bod Nothing
            | (i, MatchExp _ _ lhs rhs side) <- zip [1..] hints, "notTypeSafe" `notElem` vars side
            , let vs = map toNamed $ nub $ filter isUnifyVar $ vars lhs ++ vars rhs
            , let inner = InfixApp an (Paren an lhs) (toNamed "==>") (Paren an rhs)
            , let bod = UnGuardedRhs an $ if null vs then inner else Lambda an vs inner]


---------------------------------------------------------------------
-- CHECK ANNOTATIONS

-- Input, Output
-- Output = Nothing, should not match
-- Output = Just xs, should match xs
data Test = Test SrcLoc String (Maybe String)

checkAnnotations :: [Setting] -> FilePath -> IO Result
checkAnnotations setting file = do
    tests <- parseTestFile file
    failures <- concatMapM f tests
    putStr $ unlines failures
    return $ Result (length failures) (length tests)
    where
        f (Test loc inp out) = do
            ideas <- applyHintStr parseFlags setting file inp
            let good = case out of
                    Nothing -> null ideas
                    Just x -> length ideas == 1 &&
                              length (show ideas) >= 0 && -- force, mainly for hpc
                              not (isParseError (head ideas)) &&
                              match x (head ideas)
            return
                ["TEST FAILURE (" ++ show (length ideas) ++ " hints generated)\n" ++
                 "SRC: " ++ showSrcLoc loc ++ "\n" ++
                 "INPUT: " ++ inp ++ "\n" ++
                 concatMap ((++) "OUTPUT: " . show) ideas ++
                 "WANTED: " ++ fromMaybe "<failure>" out ++ "\n\n"
                | not good]

        match "???" _ = True
        match x y | "@" `isPrefixOf` x = a == show (rank y) && match (ltrim b) y
            where (a,b) = break isSpace $ tail x
        match x y = on (==) norm (to y) x

        -- FIXME: Should use a better check for expected results
        norm = filter $ \x -> not (isSpace x) && x /= ';'


parseTestFile :: FilePath -> IO [Test]
parseTestFile file = do
    src <- readFile file
    return $ f False $ zip [1..] $ lines src
    where
        open = isPrefixOf "<TEST>"
        shut = isPrefixOf "</TEST>"

        f False ((i,x):xs) = f (open x) xs
        f True  ((i,x):xs)
            | shut x = f False xs
            | null x || "--" `isPrefixOf` x = f True xs
            | "\\" `isSuffixOf` x, (_,y):ys <- xs = f True $ (i,init x++"\n"++y):ys
            | otherwise = parseTest file i x : f True xs
        f _ [] = []


parseTest file i x = Test (SrcLoc file i 0) x $
    case dropWhile (/= "--") $ words x of
        [] -> Nothing
        _:xs -> Just $ unwords xs

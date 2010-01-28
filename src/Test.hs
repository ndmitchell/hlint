{-# LANGUAGE PatternGuards #-}

module Test where

import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import System.Directory
import System.FilePath
import System.IO
import System.Cmd
import System.Exit

import Settings
import Type
import Hint
import HSE.All
import Hint.All


-- Input, Output
-- Output = Nothing, should not match
-- Output = Just xs, should match xs
data Test = Test SrcLoc String (Maybe String)


test :: FilePath -> IO Int
test dataDir = do
    dataLs <- getDirectoryContents dataDir

    src <- doesDirectoryExist "src/Hint"
    (fail,total) <- fmap ((sum *** sum) . unzip) $ sequence $
        [runTestDyn dataDir (dataDir </> h) | h <- dataLs, takeExtension h == ".hs", not $ "HLint" `isPrefixOf` takeBaseName h] ++
        [runTest id [h] ("src/Hint" </> name <.> "hs") | (name,h) <- staticHints, src]
    unless src $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"
    if fail == 0
        then putStrLn $ "Tests passed (" ++ show total ++ ")"
        else putStrLn $ "Tests failed (" ++ show fail ++ " of " ++ show total ++ ")"
    return fail


runTestDyn :: FilePath -> FilePath -> IO (Int,Int)
runTestDyn dataDir file = do
    settings <- readSettings dataDir [file]
    let bad = [putStrLn $ "No name for the hint " ++ prettyPrint (lhs x) | x@MatchExp{} <- settings, hintS x == defaultHintName]
    sequence_ bad

    (f1,t1) <- runTestTypes settings
    (f2,t2) <- runTest (classify settings) (allHints settings) file
    return (length bad + f1 + f2, t1 + t2)


runTestTypes :: [Setting] -> IO (Int,Int)
runTestTypes settings = bracket
    (openTempFile "." "hlinttmp.hs")
    (\(file,h) -> removeFile file)
    $ \(file,h) -> do
        hPutStrLn h $ unlines contents
        hClose h
        res <- system $ "runhaskell " ++ file
        return (if res == ExitSuccess then 0 else 1, 1)
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
            | (i, MatchExp _ _ lhs rhs side) <- zip [1..] settings, "notTypeSafe" `notElem` vars side
            , let vs = map toNamed $ nub $ filter isUnifyVar $ vars lhs ++ vars rhs
            , let inner = InfixApp an (Paren an lhs) (toNamed "==>") (Paren an rhs)
            , let bod = UnGuardedRhs an $ if null vs then inner else Lambda an vs inner]


-- return the number of fails/total
runTest :: (Idea -> Idea) -> [Hint] -> FilePath -> IO (Int,Int)
runTest classify hint file = do
    tests <- parseTestFile file
    let failures = concatMap f tests
    putStr $ unlines failures
    return (length failures, length tests)
    where
        f (Test loc inp out) =
                ["TEST FAILURE (" ++ show (length ideas) ++ " hints generated)\n" ++
                 "SRC: " ++ showSrcLoc loc ++ "\n" ++
                 "INPUT: " ++ inp ++ "\n" ++
                 concatMap ((++) "OUTPUT: " . show) ideas ++
                 "WANTED: " ++ fromMaybe "<failure>" out ++ "\n\n"
                | not good]
            where
                ideas = map classify $ applyHintStr parseFlags hint file inp
                good = case out of
                    Nothing -> null ideas
                    Just x -> length ideas == 1 &&
                              length (show ideas) >= 0 && -- force, mainly for hpc
                              not (isParseError (head ideas)) &&
                              match x (head ideas)

        match "???" _ = True
        match x y | "@" `isPrefixOf` x = a == show (rank y) && match (dropWhile isSpace b) y
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

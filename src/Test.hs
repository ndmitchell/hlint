{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

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

progress = putChar '.'
failed xs = putStrLn $ unlines $ "" : xs


test :: ([String] -> IO ()) -> FilePath -> [FilePath] -> IO Int
test main dataDir files = do
    Result failures total <-
        if null files then do
            src <- doesFileExist "hlint.cabal"
            res <- results $ sequence $ (if src then id else take 1)
                [testHintFiles dataDir, testSourceFiles, testInputOutput main]
            putStrLn ""
            unless src $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"
            return res
        else do
            res <- results $ mapM (testHintFile dataDir) files
            putStrLn ""
            return res
    if failures == 0
        then putStrLn $ "Tests passed (" ++ show total ++ ")"
        else putStrLn $ "Tests failed (" ++ show failures ++ " of " ++ show total ++ ")"
    return failures


testHintFiles :: FilePath -> IO Result
testHintFiles dataDir = do
    xs <- getDirectoryContents dataDir
    results $ mapM (testHintFile dataDir)
        [dataDir </> x | x <- xs, takeExtension x == ".hs", not $ "HLint" `isPrefixOf` takeBaseName x]


testHintFile :: FilePath -> FilePath -> IO Result
testHintFile dataDir file = do
    hints <- readSettings dataDir [file]
    res <- results $ sequence $ nameCheckHints hints : checkAnnotations hints file :
                                [typeCheckHints hints | takeFileName file /= "Test.hs"]
    progress
    return res


testSourceFiles :: IO Result
testSourceFiles = fmap mconcat $ sequence
    [checkAnnotations [Builtin name] ("src/Hint" </> name <.> "hs") | (name,h) <- staticHints]


testInputOutput :: ([String] -> IO ()) -> IO Result
testInputOutput main = do
    xs <- getDirectoryContents "tests"
    results $ mapM (checkInputOutput main) $ groupBy ((==) `on` takeBaseName) $ sort $ filter (not . isPrefixOf ".") xs


---------------------------------------------------------------------
-- VARIOUS SMALL TESTS

nameCheckHints :: [Setting] -> IO Result
nameCheckHints hints = do
    let bad = [failed ["No name for the hint " ++ prettyPrint (lhs x)] | x@MatchExp{} <- hints, hintS x == defaultHintName]
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
        progress
        return $ result $ res == ExitSuccess
    where
        matches = filter isMatchExp hints

        contents =
            ["{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules #-}"] ++
            concat (take 1 [map prettyPrint $ scopeImports $ scope x | x <- matches]) ++
            ["main = return ()"
            ,"(==>) :: a -> a -> a; (==>) = undefined"
            ,"_noParen_ = id"
            ,"_eval_ = id"] ++
            ["{-# LINE " ++ show (startLine $ ann rhs) ++ " " ++ show (fileName $ ann rhs) ++ " #-}\n" ++
             prettyPrint (PatBind an (toNamed $ "test" ++ show i) Nothing bod Nothing)
            | (i, MatchExp _ _ _ lhs rhs side) <- zip [1..] matches, "notTypeSafe" `notElem` vars side
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
    sequence_ failures
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
            return [failed $
                ["TEST FAILURE (" ++ show (length ideas) ++ " hints generated)"
                ,"SRC: " ++ showSrcLoc loc
                ,"INPUT: " ++ inp] ++
                map ((++) "OUTPUT: " . show) ideas ++
                ["WANTED: " ++ fromMaybe "<failure>" out]
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


---------------------------------------------------------------------
-- CHECK INPUT/OUTPUT PAIRS

checkInputOutput :: ([String] -> IO ()) -> [FilePath] -> IO Result
checkInputOutput main xs = do
    let pre = takeBaseName $ head xs
        has x = (pre <.> x) `elem` xs
        reader x = readFile' $ "tests" </> pre <.> x

    flags <-
        if has "flags" then fmap (takeWhile (/= '\n')) $ reader "flags"
        else if has "hs" then return $ "tests/" ++ pre <.> "hs"
        else if has "lhs" then return $ "tests/" ++ pre <.> "lhs"
        else error "checkInputOutput, couldn't find or figure out flags"

    -- Note: if main crashes (argument error etc) then it won't trap the ExitCode
    got <- captureOutput $ handle (\(e::ExitCode) -> return ()) $ main $ words flags
    want <- reader "output"

    if got == want then return pass else do
        (got,want) <- return (lines got, lines want)
        let trail = replicate (max (length got) (length want)) "<EOF>"
        let (i,g,w):_ = [(i,g,w) | (i,g,w) <- zip3 [1..] (got++trail) (want++trail), g /= w]
        putStrLn $ unlines
            ["TEST FAILURE IN tests/" ++ pre
            ,"DIFFER ON LINE: " ++ show i
            ,"GOT : " ++ g
            ,"WANT: " ++ w]
        return failure

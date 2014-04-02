{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

module Test.Standard(test) where

import Control.Applicative
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
import Test.Util
import Test.InputOutput


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
    putStrLn $ if failures == 0
        then "Tests passed (" ++ show total ++ ")"
        else "Tests failed (" ++ show failures ++ " of " ++ show total ++ ")"
    return failures


testHintFiles :: FilePath -> IO Result
testHintFiles dataDir = do
    xs <- getDirectoryContents dataDir
    results $ mapM (testHintFile dataDir)
        [dataDir </> x | x <- xs, takeExtension x == ".hs", not $ "HLint" `isPrefixOf` takeBaseName x]


testHintFile :: FilePath -> FilePath -> IO Result
testHintFile dataDir file = do
    hints <- readSettings2 dataDir [file] []
    res <- results $ sequence $ nameCheckHints hints : checkAnnotations hints file :
                                [typeCheckHints hints | takeFileName file /= "Test.hs"]
    progress
    return res


testSourceFiles :: IO Result
testSourceFiles = mconcat <$> sequence
    [checkAnnotations [Builtin name] ("src/Hint" </> name <.> "hs") | (name,h) <- builtinHints]

---------------------------------------------------------------------
-- VARIOUS SMALL TESTS

nameCheckHints :: [Setting] -> IO Result
nameCheckHints hints = do
    let bad = [failed ["No name for the hint " ++ prettyPrint (hintRuleLHS x)] | SettingMatchExp x@HintRule{} <- hints, hintRuleName x == defaultHintName]
    sequence_ bad
    return $ Result (length bad) 0


-- | Given a set of hints, do all the HintRule hints type check
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
            ideas <- applyHintFile defaultParseFlags setting file $ Just inp
            let good = case out of
                    Nothing -> null ideas
                    Just x -> length ideas == 1 &&
                              seq (length (show ideas)) True && -- force, mainly for hpc
                              isJust (ideaTo $ head ideas) && -- detects parse failure
                              match x (head ideas)
            return $
                [failed $
                    ["TEST FAILURE (" ++ show (length ideas) ++ " hints generated)"
                    ,"SRC: " ++ showSrcLoc loc
                    ,"INPUT: " ++ inp] ++
                    map ((++) "OUTPUT: " . show) ideas ++
                    ["WANTED: " ++ fromMaybe "<failure>" out]
                    | not good] ++
                [failed
                    ["TEST FAILURE (BAD LOCATION)"
                    ,"SRC: " ++ showSrcLoc loc
                    ,"INPUT: " ++ inp
                    ,"OUTPUT: " ++ show i]
                    | i@Idea{..} <- ideas, let SrcLoc{..} = getPointLoc ideaSpan, srcFilename == "" || srcLine == 0 || srcColumn == 0]

        match "???" _ = True
        match x y | "@" `isPrefixOf` x = a == show (ideaSeverity y) && match (ltrim b) y
            where (a,b) = break isSpace $ tail x
        match x y = on (==) norm (fromMaybe "" $ ideaTo y) x

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


{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

module Test.Standard(test) where

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
import Util
import Idea
import Apply
import HSE.All
import Hint.All
import Test.Util
import Test.InputOutput


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
    progress $ "Testing hint file " ++ file
    hints <- readSettings2 dataDir [file] []
    sequence_ $ nameCheckHints hints : checkAnnotations hints file :
                [typeCheckHints hints | takeFileName file /= "Test.hs"]


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
        progress $ "Typechecking " ++ file
        res <- system $ "runhaskell " ++ file
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


---------------------------------------------------------------------
-- CHECK ANNOTATIONS

-- Input, Output
-- Output = Nothing, should not match
-- Output = Just xs, should match xs
data Test = Test SrcLoc String (Maybe String)

checkAnnotations :: [Setting] -> FilePath -> IO ()
checkAnnotations setting file = do
    tests <- parseTestFile file
    mapM_ f tests
    where
        f (Test loc inp out) = do
            ideas <- applyHintFile defaultParseFlags setting file $ Just inp
            let good = case out of
                    Nothing -> null ideas
                    Just x -> length ideas == 1 &&
                              seq (length (show ideas)) True && -- force, mainly for hpc
                              isJust (ideaTo $ head ideas) && -- detects parse failure
                              match x (head ideas)
            let bad =
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
            if null bad then passed else sequence_ bad

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


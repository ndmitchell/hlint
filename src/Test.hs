{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

module Test(test) where

import Control.Applicative
import Control.Arrow
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
import System.Console.CmdArgs.Explicit
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


testInputOutput :: ([String] -> IO ()) -> IO Result
testInputOutput main = do
    xs <- getDirectoryContents "tests"
    (bulk, xs) <- return $ partition ((==) ".test" . takeExtension) xs
    (add,sub) <- fmap (second concat . unzip . concat) $ forM bulk $ \file -> do
        ios <- parseInputOutputs <$> readFile ("tests" </> file)
        forM (zip [1..] ios) $ \(i,InputOutput{..}) -> do
            forM_ files $ \(name,contents) -> do
                createDirectoryIfMissing True $ takeDirectory name
                writeFile name contents
            let name = "_" ++ takeBaseName file ++ "_" ++ show i
            writeFile ("tests" </> name <.> "flags") $ unlines run
            writeFile ("tests" </> name <.> "output") output
            return (map (name <.>) ["flags","output"], map fst files)
    res <- results $ mapM (checkInputOutput main) $ add ++ groupBy ((==) `on` takeBaseName)
        (sort $ filter (not . null . takeExtension) $ filter (not . isPrefixOf "_") $ filter (not . isPrefixOf ".") $ xs \\ map takeFileName sub)
    mapM_ removeFile sub
    forM_ (concat add) $ \x -> removeFile $ "tests" </> x
    return res

data InputOutput = InputOutput
    {files :: [(FilePath, String)]
    ,run :: [String]
    ,output :: String
    } deriving Eq

parseInputOutputs :: String -> [InputOutput]
parseInputOutputs = f z . lines
    where
        z = InputOutput [] [] ""
        interest x = any (`isPrefixOf` x) ["----","FILE","RUN","OUTPUT"]

        f io ((stripPrefix "RUN " -> Just flags):xs) = f io{run = splitArgs flags} xs
        f io ((stripPrefix "FILE " -> Just file):xs) | (str,xs) <- g xs = f io{files = files io ++ [(file,unlines str)]} xs
        f io ("OUTPUT":xs) | (str,xs) <- g xs = f io{output = unlines str} xs
        f io ((isPrefixOf "----" -> True):xs) = [io | io /= z] ++ f z xs
        f io [] = [io | io /= z]
        f io (x:xs) = error $ "Unknown test item, " ++ x

        g = first (reverse . dropWhile null . reverse) . break interest

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


---------------------------------------------------------------------
-- CHECK INPUT/OUTPUT PAIRS

checkInputOutput :: ([String] -> IO ()) -> [FilePath] -> IO Result
checkInputOutput main xs = do
    let pre = takeBaseName $ head xs
        has x = (pre <.> x) `elem` xs
        reader x = readFile' $ "tests" </> pre <.> x

    flags <-
        if has "flags" then lines <$> reader "flags"
        else if has "hs" then return ["tests/" ++ pre <.> "hs"]
        else if has "lhs" then return ["tests/" ++ pre <.> "lhs"]
        else error $ "checkInputOutput, couldn't find or figure out flags for " ++ pre

    got <- fmap (reverse . dropWhile null . reverse . map rtrim . lines) $ captureOutput $
        handle (\(e::SomeException) -> print e) $
        handle (\(e::ExitCode) -> return ()) $
        main flags
    want <- lines <$> reader "output"
    (want,got) <- return $ matchStarStar want got

    if length got == length want && and (zipWith matchStar want got) then
        return pass
     else do
        let trail = replicate (max (length got) (length want)) "<EOF>"
        let (i,g,w):_ = [(i,g,w) | (i,g,w) <- zip3 [1..] (got++trail) (want++trail), not $ matchStar w g]
        putStrLn $ unlines
            ["TEST FAILURE IN tests/" ++ pre
            ,"DIFFER ON LINE: " ++ show i
            ,"GOT : " ++ g
            ,"WANT: " ++ w]
        when (null want) $ putStrLn $ unlines $ "FULL OUTPUT FOR GOT:" : got
        return failure


-- | First string may have stars in it (the want)
matchStar :: String -> String -> Bool
matchStar ('*':xs) ys = any (matchStar xs) $ tails ys
matchStar (x:xs) (y:ys) = x == y && matchStar xs ys
matchStar [] [] = True
matchStar _ _ = False


matchStarStar :: [String] -> [String] -> ([String], [String])
matchStarStar want got = case break (== "**") want of
    (_, []) -> (want, got)
    (w1,_:w2) -> (w1++w2, g1 ++ revTake (length w2) g2)
        where (g1,g2) = splitAt (length w1) got

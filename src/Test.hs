{-# LANGUAGE ViewPatterns #-}

module Test where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Either
import System.Directory
import System.FilePath

import Settings
import Type
import Util
import HSE.All
import Hint.All
import Paths_hlint


-- Input, Output
-- Output = Nothing, should not match
-- Output = Just xs, should match xs
data Test = Test SrcLoc Decl (Maybe String)


test :: IO ()
test = do
    dat <- getDataDir
    datDir <- getDirectoryContents dat

    src <- doesDirectoryExist "src/Hint"
    (fail,total) <- liftM ((sum *** sum) . unzip) $ sequence $
        [runTestDyn (dat </> h) | h <- datDir, takeExtension h == ".hs"] ++
        [runTest h ("src/Hint" </> name <.> "hs") | (name,h) <- staticHints, src]
    unless src $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"
    if fail == 0
        then putStrLn $ "Tests passed (" ++ show total ++ ")"
        else putStrLn $ "Tests failed (" ++ show fail ++ " of " ++ show total ++ ")"


runTestDyn :: FilePath -> IO (Int,Int)
runTestDyn file = do
    settings <- readSettings [file]
    runTest (dynamicHints settings) file


-- return the number of fails/total
runTest :: Hint -> FilePath -> IO (Int,Int)
runTest hint file = do
    tests <- parseTestFile file
    let failures = concatMap f tests
    putStr $ unlines failures
    return (length failures, length tests)
    where
        f (nm, Test loc inp out) =
                ["Test failed " ++ showSrcLoc loc ++ " " ++
                 concatMap ((++) " | " . show) ideas ++ "\n" ++
                 prettyPrint inp | not good]
            where
                ideas = declHint hint nm inp
                good = case out of
                    Nothing -> ideas == []
                    Just x -> length ideas == 1 && (null x || to (head ideas) == x)


parseTestFile :: FilePath -> IO [(NameMatch, Test)]
parseTestFile file = do
    src <- readFile file
    return [(nm, createTest eqn)
           | code <- f $ lines src, let modu = fromParseResult $ parseString Nothing False file code
           , let nm = nameMatch $ moduleImports modu
           , eqn <- concatMap getEquations $ moduleDecls modu]
    where
        open = isPrefixOf "<TEST>"
        shut = isPrefixOf "</TEST>"
        f [] = []
        f xs = unlines inner : f (drop 1 test)
            where (inner,test) = break shut $ drop 1 $ dropWhile (not . open) xs


createTest :: Decl -> Test
createTest x = Test (declSrcLoc x) x2 (if negative then Nothing else Just res)
    where
        s = map toLower $ fromNamed x
        negative = "no" `isPrefixOf` s || "-" `isPrefixOf` s
        (res,x2) = getRes x


getRes :: Decl -> (String, Decl)
getRes (FunBind [Match x1 x2 x3 x4 x5 (BDecls binds)]) =
        (headDef "<error: no res clause>" res, FunBind [Match x1 x2 x3 x4 x5 (BDecls binds2)])
    where (res, binds2) = partitionEithers $ map f binds
          f (PatBind _ (fromNamed -> "res") _ (UnGuardedRhs res) _) =
              Left $ if isString res then fromString res else prettyPrint res
          f x = Right x
getRes x = ("", x)

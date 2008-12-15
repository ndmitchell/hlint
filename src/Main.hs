
module Main where

import Control.Arrow
import Control.Monad
import Data.List
import Language.Haskell.Exts
import System.Directory
import Data.Generics.PlateData

import CmdLine
import Report
import Type
import Util
import Hint.All


main = do
    mode <- getMode
    if modeTest mode then do
        hints <- mapM (readHints . (:[])) (modeHints mode)
        src <- doesDirectoryExist "src/Hint"
        (fail,total) <- liftM ((sum *** sum) . unzip) $ sequence $
                zipWith runTest hints (modeHints mode) ++
                [runTest h ("src/Hint/" ++ name ++ ".hs") | (name,h) <- allHints, src]
        when (not src) $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"
        if fail == 0
            then putStrLn $ "Tests passed (" ++ show total ++ ")"
            else putStrLn $ "Tests failed (" ++ show fail ++ " of " ++ show total ++ ")"
     else do
        hints <- readHints $ modeHints mode
        ideas <- liftM concat $ mapM (runFile hints) (modeFiles mode)
        let n = length ideas
        if n == 0 then do
            when (not $ null $ modeReports mode) $ putStrLn "Skipping writing reports"
            putStrLn "No relevant suggestions"
         else do
            flip mapM_ (modeReports mode) $ \x -> do
                putStrLn $ "Writing report to " ++ x ++ " ..."
                writeReport x ideas
            putStrLn $ "Found " ++ show n ++ " suggestions"


-- return the number of hints given
runFile :: Hint -> FilePath -> IO [Idea]
runFile hint file = do
    src <- parseHsModule file
    let ideas = applyHint hint src
    mapM_ print ideas
    return ideas



-- return the number of fails/total
runTest :: Hint -> FilePath -> IO (Int,Int)
runTest hint file = do
    tests <- parseTestFile file
    let failures = concatMap f tests
    mapM_ putStrLn failures
    return (length failures, length tests)
    where
        f o | ("no" `isPrefixOf` name) == null ideas && length ideas <= 1 = []
            | otherwise = ["Test failed in " ++ name ++ concatMap ((++) " | " . show) ideas]
            where
                ideas = hint o
                name = declName o


parseTestFile :: FilePath -> IO [Decl]
parseTestFile file = do
    src <- readFile file
    src <- return $ unlines $ f $ lines src
    case parseFileContents src of
        ParseOk x -> return $ childrenBi $ operatorPrec x
        _ -> error $ "Parse failure in test block of " ++ file ++ "\n" ++ src
    where
        open = isPrefixOf "<TEST>"
        shut = isPrefixOf "</TEST>"
        f [] = []
        f xs = inner ++ f (drop 1 test)
            where (inner,test) = break shut $ drop 1 $ dropWhile (not . open) xs

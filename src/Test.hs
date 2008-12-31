
module Test where

import Control.Arrow
import Control.Monad
import Data.List
import System.Directory
import Data.Generics.PlateData

import CmdLine
import Settings
import Report
import Type
import HSE.All
import Hint.All
import Paths_hlint


test :: IO ()
test = do
    dat <- getDataDir
    settings <- readSettings []
    let hints = readHints settings
        
    src <- doesDirectoryExist "src/Hint"
    (fail,total) <- liftM ((sum *** sum) . unzip) $ sequence $
            runTest hints (dat ++ "/Hints.hs") :
            [runTest h ("src/Hint/" ++ name ++ ".hs") | (name,h) <- allHints, src]
    when (not src) $ putStrLn "Warning, couldn't find source code, so non-hint tests skipped"
    if fail == 0
        then putStrLn $ "Tests passed (" ++ show total ++ ")"
        else putStrLn $ "Tests failed (" ++ show fail ++ " of " ++ show total ++ ")"



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
    return $ childrenBi $ parseString file src
    where
        open = isPrefixOf "<TEST>"
        shut = isPrefixOf "</TEST>"
        f [] = []
        f xs = inner ++ f (drop 1 test)
            where (inner,test) = break shut $ drop 1 $ dropWhile (not . open) xs

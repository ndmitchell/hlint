
module Test where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import Data.Generics.PlateData

import CmdLine
import Settings
import Report
import Type
import HSE.All
import Hint.All
import Paths_hlint


-- Input, Output
-- Output = Nothing, should not match
-- Output = Just Nothing, should match, no required match
-- Output = Just (Just x), should match x
data Test = Test String Decl (Maybe (Maybe Decl))


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
    putStr $ unlines failures
    return (length failures, length tests)
    where
        f (Test name inp out)
            | isNothing out == null ideas && length ideas <= 1 = []
            | otherwise = ["Test failed in " ++ name ++ concatMap ((++) " | " . show) ideas]
            where ideas = hint inp


parseTestFile :: FilePath -> IO [Test]
parseTestFile file = do
    src <- readFile file
    src <- return $ unlines $ f $ lines src
    return $ map createTest $ moduleDecls $ parseString file src
    where
        open = isPrefixOf "<TEST>"
        shut = isPrefixOf "</TEST>"
        f [] = []
        f xs = inner ++ f (drop 1 test)
            where (inner,test) = break shut $ drop 1 $ dropWhile (not . open) xs


createTest :: Decl -> Test
createTest x = Test name x $
    if "no" `isPrefixOf` name then Nothing else Just Nothing
    where name = declName x

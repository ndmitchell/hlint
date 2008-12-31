
module Main where

import Control.Arrow
import Control.Monad
import Data.List
import System.Directory
import Data.Generics.PlateData

import CmdLine
import Settings
import Report
import Type
import Test
import HSE.All
import Hint.All
import Paths_hlint


main = do
    mode <- getMode
    if modeTest mode then test else do
        settings <- readSettings $ modeHints mode
        let ignore idea = let f = classify settings in f (text idea) ("","") == Skip
        let hints = readHints settings

        ideas <- liftM concat $ mapM (runFile ignore hints) (modeFiles mode)
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
runFile :: (Idea -> Bool) -> Hint -> FilePath -> IO [Idea]
runFile ignore hint file = do
    src <- parseFile file
    let ideas = filter (not . ignore) $ applyHint hint src
    mapM_ print ideas
    return ideas

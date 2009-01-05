{-# LANGUAGE RecordWildCards #-}

module Main where

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
import Test
import HSE.All
import Hint.All
import Paths_hlint


main = do
    Cmd{..} <- getCmd
    if cmdTest then test else do
        settings <- readSettings cmdHintFiles
        let apply = map (classify settings) . applyHint (readHints settings)
        ideas <- liftM concat $ mapM (liftM apply . parseFile) cmdFiles
        mapM_ print [i | i <- ideas, rank i /= Skip]
        
        let n = length ideas
        if n == 0 then do
            when (cmdReports /= []) $ putStrLn "Skipping writing reports"
            putStrLn "No relevant suggestions"
         else do
            flip mapM_ cmdReports $ \x -> do
                putStrLn $ "Writing report to " ++ x ++ " ..."
                writeReport x ideas
            putStrLn $ "Found " ++ show n ++ " suggestions"

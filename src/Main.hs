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
        mapM_ print [i | i <- ideas, cmdShowSkip || rank i /= Skip]

        -- figure out statistics        
        let counts = map (head &&& length) $ group $ sort $ map rank ideas
        let [skip,warn,fix] = map (fromMaybe 0 . flip lookup counts) [Skip,Warn,Fix]
        let total = skip + warn + fix
        let shown = if cmdShowSkip then total else total-skip
        let skipped = total-shown

        if shown == 0 then do
            when (cmdReports /= []) $ putStrLn "Skipping writing reports"
            printMsg "No relevant suggestions" [(skipped,"skipped")]
         else do
            flip mapM_ cmdReports $ \x -> do
                putStrLn $ "Writing report to " ++ x ++ " ..."
                writeReport x ideas
            printMsg ("Found " ++ show shown ++ " suggestion" ++ ['s'|shown/=1])
                     [(fix,"serious"),(skipped,"skipped")]


printMsg :: String -> [(Int, String)] -> IO ()
printMsg msg extras =
    putStrLn $ msg ++ if null xs then "" else
        " (" ++ intercalate ", " xs ++ ")"
    where
        xs = concatMap f extras
        f (0,_) = []
        f (i,x) = [unwords [show i, if i == 1 then "was" else "were", x]]

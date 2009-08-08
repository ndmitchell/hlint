{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import System.Exit

import CmdLine
import Settings
import Report
import Type
import Test
import Util
import Parallel
import Hint.All
import HSE.All


main = do
    Cmd{..} <- getCmd
    if cmdTest then test else do
        settings <- readSettings cmdHintFiles
        let extra = [Classify Ignore x ("","") | x <- cmdIgnore]
        let apply :: FilePath -> IO [Idea]
            apply = fmap (fmap $ classify $ settings ++ extra) . applyHint parseFlags{cpphs=Just cmdCpphs} (allHints settings)
        ideas <- liftM concat $ parallel [listM' =<< apply x | x <- cmdFiles]
        showItem <- if cmdColor then showANSI else return show
        mapM_ putStrLn [showItem i | i <- ideas, cmdShowAll || rank i /= Ignore]

        -- figure out statistics        
        let counts = map (head &&& length) $ group $ sort $ map rank ideas
        let [ignore,warn,err] = map (fromMaybe 0 . flip lookup counts) [Ignore,Warning,Error]
        let total = ignore + warn + err
        let shown = if cmdShowAll then total else total - ignore

        let ignored = [show i ++ " ignored" | let i = total - shown, i /= 0]
        let errors = [show err ++ " error" ++ ['s'|err/=1] | err /= 0]

        if shown == 0 then do
            when (cmdReports /= []) $ putStrLn "Skipping writing reports"
            printMsg "No relevant suggestions" ignored
         else do
            forM_ cmdReports $ \x -> do
                putStrLn $ "Writing report to " ++ x ++ " ..."
                writeReport x ideas
            printMsg ("Found " ++ show shown ++ " suggestion" ++ ['s'|shown/=1]) (errors++ignored)

        when (err > 0) $
            exitWith $ ExitFailure 1


printMsg :: String -> [String] -> IO ()
printMsg msg xs =
    putStrLn $ msg ++ if null xs then "" else
        " (" ++ intercalate ", " xs ++ ")"

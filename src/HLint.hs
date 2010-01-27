{-# LANGUAGE RecordWildCards #-}

module HLint(hlint) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe

import CmdLine
import Settings
import Report
import Type
import Hint
import Test
import Util
import Parallel
import Hint.All
import HSE.All


-- | This function takes the command line arguments, and returns the number
--   of errors reported.
hlint :: [String] -> IO Int
hlint args = do
    Cmd{..} <- getCmd args
    if cmdTest then test cmdDataDir else do
        settings <- readSettings cmdDataDir cmdHintFiles
        let extra = [Classify Ignore x ("","") | x <- cmdIgnore]
        let apply :: FilePath -> IO [Idea]
            apply = fmap (fmap $ classify $ settings ++ extra) . applyHint flags (allHints settings)
            flags = parseFlags{cpphs=Just cmdCpphs, encoding=cmdEncoding} 
        ideas <- fmap concat $ parallel [listM' =<< apply x | x <- cmdFiles]
        let visideas = filter (\i -> cmdShowAll || rank i /= Ignore) ideas
        showItem <- if cmdColor then showANSI else return show
        mapM_ (putStrLn . showItem) visideas

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
                writeReport cmdDataDir x visideas
            printMsg ("Found " ++ show shown ++ " suggestion" ++ ['s'|shown/=1]) (errors++ignored)
        return err


printMsg :: String -> [String] -> IO ()
printMsg msg xs =
    putStrLn $ msg ++ if null xs then "" else
        " (" ++ intercalate ", " xs ++ ")"

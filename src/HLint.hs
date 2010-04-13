{-# LANGUAGE RecordWildCards #-}

module HLint(hlint) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe

import CmdLine
import Settings
import Report
import Idea
import Apply
import Test
import Util
import Parallel
import HSE.All


-- | This function takes the command line arguments, and returns the number
--   of errors reported.
hlint :: [String] -> IO Int
hlint args = do
    cmd@Cmd{..} <- getCmd args
    let flags = parseFlags{cpphs=Just cmdCpphs, encoding=cmdEncoding} 
    if cmdTest then
        test cmdDataDir
     else if null cmdFiles && notNull cmdFindHints then
        mapM_ (\x -> putStrLn . fst =<< findSettings flags x) cmdFindHints >> return 0
     else if null cmdFiles then
        exitWithHelp
     else
        runHints cmd flags


runHints :: Cmd -> ParseFlags -> IO Int
runHints Cmd{..} flags = do
    settings1 <- readSettings cmdDataDir cmdHintFiles
    settings2 <- concatMapM (fmap snd . findSettings flags) cmdFindHints
    settings3 <- return [Classify Ignore x ("","") | x <- cmdIgnore]
    let settings = settings1 ++ settings2 ++ settings3

    let apply :: FilePath -> IO [Idea]
        apply = applyHint flags settings
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

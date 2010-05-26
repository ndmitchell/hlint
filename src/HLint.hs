{-# LANGUAGE RecordWildCards #-}

module HLint(hlint, Suggestion, suggestionLocation) where

import Control.Monad
import Data.List

import CmdLine
import Settings
import Report
import Idea
import Apply
import Test
import Util
import Parallel
import HSE.All


-- | The type of a HLint suggestion. Note the instances available.
newtype Suggestion = Suggestion {fromSuggestion :: Idea}
                     deriving (Show,Eq,Ord)

-- | The location of a suggestion in a file.
suggestionLocation :: Suggestion -> SrcLoc
suggestionLocation = loc . fromSuggestion



-- | This function takes the command line arguments, and returns the suggestions
--   reported.
hlint :: [String] -> IO [Suggestion]
hlint args = do
    cmd@Cmd{..} <- getCmd args
    let flags = parseFlags{cpphs=cmdCpphs, encoding=cmdEncoding, language=cmdLanguage}
    if cmdTest then
        test cmdDataDir >> return []
     else if null cmdFiles && notNull cmdFindHints then
        mapM_ (\x -> putStrLn . fst =<< findSettings flags x) cmdFindHints >> return []
     else if null cmdFiles then
        exitWithHelp
     else
        runHints cmd flags


runHints :: Cmd -> ParseFlags -> IO [Suggestion]
runHints Cmd{..} flags = do
    settings1 <- readSettings cmdDataDir cmdHintFiles
    settings2 <- concatMapM (fmap snd . findSettings flags) cmdFindHints
    settings3 <- return [Classify Ignore x ("","") | x <- cmdIgnore]
    let settings = settings1 ++ settings2 ++ settings3

    ideas <- fmap concat $ parallel [listM' =<< applyHint flags settings x | x <- cmdFiles]
    let (showideas,hideideas) = partition (\i -> cmdShowAll || rank i /= Ignore) ideas
    showItem <- if cmdColor then showANSI else return show
    mapM_ (putStrLn . showItem) showideas

    if null showideas then
        when (cmdReports /= []) $ putStrLn "Skipping writing reports"
     else
        forM_ cmdReports $ \x -> do
            putStrLn $ "Writing report to " ++ x ++ " ..."
            writeReport cmdDataDir x showideas
    putStrLn $
        (let i = length showideas in if i == 0 then "No suggestions" else show i ++ " suggestion" ++ ['s'|i/=1]) ++
        (let i = length hideideas in if i == 0 then "" else " (" ++ show i ++ " ignored)")
    return $ map Suggestion showideas

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


-- | A suggestion - the @Show@ instance is of particular use.
newtype Suggestion = Suggestion {fromSuggestion :: Idea}
                     deriving (Eq,Ord)

instance Show Suggestion where
    show = show . fromSuggestion

-- | From a suggestion, extract the file location it refers to.
suggestionLocation :: Suggestion -> SrcLoc
suggestionLocation = loc . fromSuggestion



-- | This function takes a list of command line arguments, and returns the given suggestions.
--   To see a list of arguments type @hlint --help@ at the console.
--   This function writes to the stdout/stderr streams, unless @--quiet@ is specified.
--
--   As an example:
--
-- > do hints <- hlint ["src", "--ignore=Use map","--quiet"]
-- >    when (length hints > 3) $ error "Too many hints!"
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
    let outStrLn x = unless cmdQuiet $ putStrLn x
    settings1 <- readSettings cmdDataDir cmdHintFiles
    settings2 <- concatMapM (fmap snd . findSettings flags) cmdFindHints
    settings3 <- return [Classify Ignore x ("","") | x <- cmdIgnore]
    let settings = settings1 ++ settings2 ++ settings3

    ideas <- fmap concat $ parallel [listM' =<< applyHint flags settings x | x <- cmdFiles]
    let (showideas,hideideas) = partition (\i -> cmdShowAll || rank i /= Ignore) ideas
    showItem <- if cmdColor then showANSI else return show
    mapM_ (outStrLn . showItem) showideas

    if null showideas then
        when (cmdReports /= []) $ outStrLn "Skipping writing reports"
     else
        forM_ cmdReports $ \x -> do
            outStrLn $ "Writing report to " ++ x ++ " ..."
            writeReport cmdDataDir x showideas
    outStrLn $
        (let i = length showideas in if i == 0 then "No suggestions" else show i ++ " suggestion" ++ ['s'|i/=1]) ++
        (let i = length hideideas in if i == 0 then "" else " (" ++ show i ++ " ignored)")
    return $ map Suggestion showideas

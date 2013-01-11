{-# LANGUAGE RecordWildCards #-}

module HLint(hlint, Suggestion, suggestionLocation, suggestionSeverity, Severity(..)) where

import Control.Monad
import Data.List
import Data.Maybe

import CmdLine
import Settings
import Report
import Idea
import Apply
import Test
import Proof
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


-- | From a suggestion, determine how severe it is.
suggestionSeverity :: Suggestion -> Severity
suggestionSeverity = severity . fromSuggestion



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
    let flags = parseFlags{cppFlags=cmdCpp, encoding=cmdEncoding, language=cmdLanguage}
    if cmdTest then
        test (\x -> hlint x >> return ()) cmdDataDir cmdGivenHints >> return []
     else if notNull cmdProof then do
        s <- readAllSettings cmd flags
        let reps = if cmdReports == ["report.html"] then ["report.txt"] else cmdReports
        mapM_ (proof reps s) cmdProof
        return []
     else if isNothing cmdFiles && notNull cmdFindHints then
        mapM_ (\x -> putStrLn . fst =<< findSettings flags x) cmdFindHints >> return []
     else if isNothing cmdFiles then
        exitWithHelp
     else if cmdFiles == Just [] then
        error "No files found"
     else
        runHints cmd flags

readAllSettings :: Cmd -> ParseFlags -> IO [Setting]
readAllSettings Cmd{..} flags = do
    settings1 <- readSettings cmdDataDir cmdHintFiles cmdWithHints
    settings2 <- concatMapM (fmap snd . findSettings flags) cmdFindHints
    settings3 <- return [Classify Ignore x ("","") | x <- cmdIgnore]
    return $ settings1 ++ settings2 ++ settings3


runHints :: Cmd -> ParseFlags -> IO [Suggestion]
runHints cmd@Cmd{..} flags = do
    let outStrLn x = unless cmdQuiet $ putStrLn x
    settings <- readAllSettings cmd flags

    let files = fromMaybe [] cmdFiles
    ideas <- if cmdCross
        then applyHintFiles flags settings files
        else fmap concat $ parallel [listM' =<< applyHintFile flags settings x | x <- files]
    let (showideas,hideideas) = partition (\i -> cmdShowAll || severity i /= Ignore) ideas
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

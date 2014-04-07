{-# LANGUAGE RecordWildCards #-}

module HLint(hlint, Suggestion, suggestionLocation, suggestionSeverity, Severity(..)) where

import Control.Applicative
import Control.Monad
import System.Console.CmdArgs.Verbosity
import Data.List
import System.Exit

import CmdLine
import Settings
import Report
import Idea
import Apply
import Test.Standard
import Grep
import Test.Proof
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
suggestionLocation = getPointLoc . ideaSpan . fromSuggestion


-- | From a suggestion, determine how severe it is.
suggestionSeverity :: Suggestion -> Severity
suggestionSeverity = ideaSeverity . fromSuggestion



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
    cmd <- getCmd args
    case cmd of
        CmdMain{} -> hlintMain cmd
        CmdGrep{} -> hlintGrep cmd >> return []
        CmdHSE{}  -> hlintHSE  cmd >> return []
        CmdTest{} -> hlintTest cmd >> return []

hlintHSE :: Cmd -> IO ()
hlintHSE CmdHSE{..} = do
    v <- getVerbosity
    forM_ cmdFiles $ \x -> do
        putStrLn $ "Parse result of " ++ x ++ ":"
        res <- parseFile x
        case res of
            x@ParseFailed{} -> print x
            ParseOk m -> case v of
                Loud -> print m
                Quiet -> print $ prettyPrint m
                _ -> print $ fmap (const ()) m
        putStrLn ""

hlintTest :: Cmd -> IO ()
hlintTest cmd@CmdTest{..} = do
    if notNull cmdProof then do
        files <- cmdHintFiles cmd
        s <- readSettings2 cmdDataDir files []
        let reps = if cmdReports == ["report.html"] then ["report.txt"] else cmdReports
        mapM_ (proof reps s) cmdProof
     else do
        failed <- test (\args -> do errs <- hlint args; when (length errs > 0) $ exitWith $ ExitFailure 1) cmdDataDir cmdGivenHints
        when (failed > 0) exitFailure

hlintGrep :: Cmd -> IO ()
hlintGrep cmd@CmdGrep{..} = do
    encoding <- readEncoding cmdEncoding
    let flags = parseFlagsSetExtensions (cmdExtensions cmd) $ defaultParseFlags{cppFlags=cmdCpp cmd, encoding=encoding}
    if null cmdFiles then
        exitWithHelp
     else do
        files <- concatMapM (resolveFile cmd) cmdFiles
        if null files then
            error "No files found"
         else
            runGrep cmdPattern flags files

hlintMain :: Cmd -> IO [Suggestion]
hlintMain cmd@CmdMain{..} = do
    encoding <- readEncoding cmdEncoding
    let flags = parseFlagsSetExtensions (cmdExtensions cmd) $ defaultParseFlags{cppFlags=cmdCpp cmd, encoding=encoding}
    if null cmdFiles && notNull cmdFindHints then do
        hints <- concatMapM (resolveFile cmd) cmdFindHints
        mapM_ (\x -> putStrLn . fst =<< findSettings2 flags x) hints >> return []
     else if null cmdFiles then
        exitWithHelp
     else do
        files <- concatMapM (resolveFile cmd) cmdFiles
        if null files then
            error "No files found"
         else
            runHints cmd{cmdFiles=files} flags

readAllSettings :: Cmd -> ParseFlags -> IO [Setting]
readAllSettings cmd@CmdMain{..} flags = do
    files <- cmdHintFiles cmd
    settings1 <- readSettings2 cmdDataDir files cmdWithHints
    settings2 <- concatMapM (fmap snd . findSettings2 flags) cmdFindHints
    settings3 <- return [SettingClassify $ Classify Ignore x "" "" | x <- cmdIgnore]
    return $ settings1 ++ settings2 ++ settings3


runHints :: Cmd -> ParseFlags -> IO [Suggestion]
runHints cmd@CmdMain{..} flags = do
    let outStrLn = whenNormal . putStrLn
    settings <- readAllSettings cmd flags

    ideas <- if cmdCross
        then applyHintFiles flags settings cmdFiles
        else concat <$> parallel [listM' =<< applyHintFile flags settings x Nothing | x <- cmdFiles]
    let (showideas,hideideas) = partition (\i -> cmdShowAll || ideaSeverity i /= Ignore) ideas
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

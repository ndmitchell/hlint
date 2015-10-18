{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module HLint(hlint, Suggestion, suggestionLocation, suggestionSeverity, Severity(..)) where

import Control.Applicative
import Control.Monad.Extra
import System.Console.CmdArgs.Verbosity
import Data.List
import System.Exit
import System.IO
import System.IO.Extra
import Prelude

import Data.Version
import System.Process.Extra
import Data.Maybe
import System.Directory
import Text.ParserCombinators.ReadP

import CmdLine
import Settings
import Report
import Idea
import Apply
import Test.All
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
hlintHSE c@CmdHSE{..} = do
    v <- getVerbosity
    forM_ cmdFiles $ \x -> do
        putStrLn $ "Parse result of " ++ x ++ ":"
        res <- parseFileWithExts (cmdExtensions c) x
        case res of
            x@ParseFailed{} -> print x
            ParseOk m -> case v of
                Loud -> print m
                Quiet -> print $ prettyPrint m
                _ -> print $ void m
        putStrLn ""

hlintTest :: Cmd -> IO ()
hlintTest cmd@CmdTest{..} =
    if not $ null cmdProof then do
        files <- cmdHintFiles cmd
        s <- readSettings2 cmdDataDir files []
        let reps = if cmdReports == ["report.html"] then ["report.txt"] else cmdReports
        mapM_ (proof reps s) cmdProof
     else do
        failed <- test cmd (\args -> do errs <- hlint args; unless (null errs) $ exitWith $ ExitFailure 1) cmdDataDir cmdGivenHints
        when (failed > 0) exitFailure

hlintGrep :: Cmd -> IO ()
hlintGrep cmd@CmdGrep{..} = do
    encoding <- if cmdUtf8 then return utf8 else readEncoding cmdEncoding
    let flags = parseFlagsSetExtensions (cmdExtensions cmd) $ defaultParseFlags{cppFlags=cmdCpp cmd, encoding=encoding}
    if null cmdFiles then
        exitWithHelp
     else do
        files <- concatMapM (resolveFile cmd Nothing) cmdFiles
        if null files then
            error "No files found"
         else
            runGrep cmdPattern flags files

hlintMain :: Cmd -> IO [Suggestion]
hlintMain cmd@CmdMain{..} = do
    encoding <- if cmdUtf8 then return utf8 else readEncoding cmdEncoding
    let flags = parseFlagsSetExtensions (cmdExtensions cmd) $ defaultParseFlags{cppFlags=cmdCpp cmd, encoding=encoding}
    if null cmdFiles && not (null cmdFindHints) then do
        hints <- concatMapM (resolveFile cmd Nothing) cmdFindHints
        mapM_ (\x -> putStrLn . fst =<< findSettings2 flags x) hints >> return []
     else if null cmdFiles then
        exitWithHelp
     else if cmdRefactor then do
         withTempFile (\t ->  runHlintMain cmd (Just t) flags)
     else runHlintMain cmd Nothing flags

runHlintMain :: Cmd -> Maybe FilePath -> ParseFlags -> IO [Suggestion]
runHlintMain cmd@(CmdMain{..}) fp flags = do
  files <- concatMapM (resolveFile cmd fp) cmdFiles
  if null files
    then error "No files found"
    else runHints cmd{cmdFiles=files} flags

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
        else concat <$> parallel [evaluateList =<< applyHintFile flags settings x Nothing | x <- cmdFiles]
    let (showideas,hideideas) = partition (\i -> cmdShowAll || ideaSeverity i /= Ignore) ideas
    usecolour <- cmdUseColour cmd
    showItem <- if usecolour then showANSI else return show
    if cmdJson
        then putStrLn . showIdeasJson $ showideas
        else if cmdSerialise then do
          hSetBuffering stdout NoBuffering
          print $ map (\i -> (show i, ideaRefactoring i)) showideas
        else if cmdRefactor then do
          case cmdFiles of
            [file] -> do
              -- Ensure that we can find the executable
              path <- checkRefactor (if cmdWithRefactor == "" then Nothing else Just cmdWithRefactor)
              -- writeFile "hlint.refact"
              let hints =  show $ map (\i -> (show i, ideaRefactoring i)) showideas
              runRefactoring path file hints cmdRefactorOptions
                -- Exit with the exit code from 'refactor'
                >>= exitWith
            _ -> error "Refactor flag can only be used with an individual file"

        else do
            mapM_ (outStrLn . showItem) showideas
            if null showideas then
                when (cmdReports /= []) $ outStrLn "Skipping writing reports"
             else
                forM_ cmdReports $ \x -> do
                    outStrLn $ "Writing report to " ++ x ++ " ..."
                    writeReport cmdDataDir x showideas
            unless cmdNoSummary $
                outStrLn $
                    (let i = length showideas in if i == 0 then "No suggestions" else show i ++ " suggestion" ++ ['s' | i/=1]) ++
                    (let i = length hideideas in if i == 0 then "" else " (" ++ show i ++ " ignored)")
    return $ map Suggestion showideas

runRefactoring :: FilePath -> FilePath -> String -> String -> IO ExitCode
runRefactoring rpath fin hints opts =  do
  let args = [fin, "-v0"] ++ words opts
  (Just hin, Just hout, _stderr, phand) <- createProcess $ (proc rpath args) { std_in = CreatePipe
                                                                             , std_out = CreatePipe }
  hPutStr hin hints
  hClose hin
  hGetContents hout >>= putStr
  -- Propagate the exit code from the spawn process
  waitForProcess phand

checkRefactor :: Maybe FilePath -> IO FilePath
checkRefactor rpath = do
  let excPath = (fromMaybe "refactor" rpath)
  mexc <- findExecutable excPath
  case mexc of
    Just exc ->  do
      vers <- readP_to_S parseVersion . tail <$> (readProcess exc ["--version"] "")
      case vers of
        [] -> putStrLn "Unabled to determine version of refactor" >> (return exc)
        (last -> (version, _)) -> if versionBranch version >= [0,1,0,0]
                                    then return exc
                                    else error "Your version of refactor is too old, please upgrade to the latest version"
    Nothing ->  error $ unlines ["Could not find refactor"
                                , "Tried with: " ++ excPath ]

evaluateList :: [a] -> IO [a]
evaluateList xs = length xs `seq` return xs

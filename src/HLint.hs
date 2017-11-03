{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module HLint(hlint) where

import Control.Applicative
import Control.Monad.Extra
import Control.Exception
import Control.Concurrent.Extra
import System.Console.CmdArgs.Verbosity
import Data.List.Extra
import GHC.Conc
import System.Exit
import System.IO.Extra
import Data.Tuple.Extra
import Prelude

import Data.Version.Extra
import System.Process.Extra
import Data.Maybe
import System.Directory
import System.FilePath

import CmdLine
import Config.Read
import Config.Type
import Config.Compute
import Report
import Idea
import Apply
import Test.All
import Hint.All
import Grep
import Test.Proof
import Parallel
import HSE.All
import CC


-- | This function takes a list of command line arguments, and returns the given hints.
--   To see a list of arguments type @hlint --help@ at the console.
--   This function writes to the stdout/stderr streams, unless @--quiet@ is specified.
--
--   As an example:
--
-- > do hints <- hlint ["src", "--ignore=Use map","--quiet"]
-- >    when (length hints > 3) $ error "Too many hints!"
--
--   /Warning:/ The flags provided by HLint are relatively stable, but do not have the same
--   API stability guarantees as the rest of the strongly-typed API. Do not run this function
--   on a your server with untrusted input.
hlint :: [String] -> IO [Idea]
hlint args = do
    cmd <- getCmd args
    case cmd of
        CmdMain{} -> do xs <- hlintMain args cmd; return $ if cmdNoExitCode cmd then [] else xs
        CmdGrep{} -> hlintGrep cmd >> return []
        CmdHSE{}  -> hlintHSE  cmd >> return []
        CmdTest{} -> hlintTest cmd >> return []

hlintHSE :: Cmd -> IO ()
hlintHSE c@CmdHSE{..} = do
    v <- getVerbosity
    forM_ cmdFiles $ \x -> do
        putStrLn $ "Parse result of " ++ x ++ ":"
        let (lang,exts) = cmdExtensions c
        -- We deliberately don't use HSE.All here to avoid any bugs in HLint
        -- polluting our bug reports (which is the main use of HSE)
        res <- parseFileWithMode defaultParseMode{baseLanguage=lang, extensions=exts} x
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
        s <- readFilesConfig $ map (,Nothing) files
        let reps = if cmdReports == ["report.html"] then ["report.txt"] else cmdReports
        mapM_ (proof reps s) cmdProof
     else do
        failed <- test cmd (\args -> do errs <- hlint args; unless (null errs) $ exitWith $ ExitFailure 1) cmdDataDir cmdGivenHints
        when (failed > 0) exitFailure

cmdParseFlags :: Cmd -> ParseFlags
cmdParseFlags cmd = parseFlagsSetLanguage (cmdExtensions cmd) $ defaultParseFlags{cppFlags=cmdCpp cmd}

hlintGrep :: Cmd -> IO ()
hlintGrep cmd@CmdGrep{..} =
    if null cmdFiles then
        exitWithHelp
     else do
        files <- concatMapM (resolveFile cmd Nothing) cmdFiles
        if null files then
            error "No files found"
         else
            runGrep cmdPattern (cmdParseFlags cmd) files

withVerbosity :: Verbosity -> IO a -> IO a
withVerbosity new act = do
    old <- getVerbosity
    (setVerbosity new >> act) `finally` setVerbosity old

hlintMain :: [String] -> Cmd -> IO [Idea]
hlintMain args cmd@CmdMain{..}
    | cmdDefault = do
        ideas <- if null cmdFiles then return [] else withVerbosity Quiet $
            runHlintMain args cmd{cmdJson=False,cmdSerialise=False,cmdRefactor=False} Nothing
        let bad = nubOrd $ map ideaHint ideas
        src <- readFile $ cmdDataDir </> "default.yaml"
        if null bad then putStr src else do
            let group1:groups = splitOn ["",""] $ lines src
            let group2 = "# Warnings currently triggered by your code" :
                         ["- ignore: {name: " ++ show x ++ "}" | x <- bad]
            putStr $ unlines $ intercalate ["",""] $ group1:group2:groups
        return []
    | null cmdFiles && not (null cmdFindHints) = do
        hints <- concatMapM (resolveFile cmd Nothing) cmdFindHints
        mapM_ (putStrLn . fst <=< computeSettings (cmdParseFlags cmd)) hints >> return []
    | null cmdFiles =
        exitWithHelp
    | cmdRefactor =
        withTempFile $ runHlintMain args cmd . Just
    | otherwise =
        runHlintMain args cmd Nothing

runHlintMain :: [String] -> Cmd -> Maybe FilePath -> IO [Idea]
runHlintMain args cmd tmpFile = do
    (cmd, settings) <- readAllSettings args cmd
    runHints args settings =<< resolveFiles cmd tmpFile

resolveFiles :: Cmd -> Maybe FilePath -> IO Cmd
resolveFiles cmd@CmdMain{..} tmpFile = do
    files <- concatMapM (resolveFile cmd tmpFile) cmdFiles
    if null files
        then error "No files found"
        else pure cmd { cmdFiles = files }
resolveFiles cmd _ = pure cmd

readAllSettings :: [String] -> Cmd -> IO (Cmd, [Setting])
readAllSettings args1 cmd@CmdMain{..} = do
    files <- cmdHintFiles cmd
    settings1 <- readFilesConfig $ map (,Nothing) files ++ [("CommandLine.hs",Just x) | x <- cmdWithHints]
    let args2 = [x | SettingArgument x <- settings1]
    cmd@CmdMain{..} <- if null args2 then return cmd else getCmd $ args1 ++ args2
    settings2 <- concatMapM (fmap snd . computeSettings (cmdParseFlags cmd)) cmdFindHints
    settings3 <- return [SettingClassify $ Classify Ignore x "" "" | x <- cmdIgnore]
    return (cmd, settings1 ++ settings2 ++ settings3)

runHints :: [String] -> [Setting] -> Cmd -> IO [Idea]
runHints args settings cmd@CmdMain{..} = do
    j <- if cmdThreads == 0 then getNumProcessors else return cmdThreads
    withNumCapabilities j $ do
        let outStrLn = whenNormal . putStrLn
        ideas <- getIdeas cmd settings
        ideas <- return $ if cmdShowAll then ideas else  filter (\i -> ideaSeverity i /= Ignore) ideas
        if cmdJson then
            putStrLn $ showIdeasJson ideas
         else if cmdCC then
            mapM_ (printIssue . fromIdea) ideas
         else if cmdSerialise then do
            hSetBuffering stdout NoBuffering
            print $ map (show &&& ideaRefactoring) ideas
         else if cmdRefactor then
            handleRefactoring ideas cmdFiles cmd
         else do
            usecolour <- cmdUseColour cmd
            showItem <- if usecolour then showANSI else return show
            mapM_ (outStrLn . showItem) ideas
            handleReporting ideas cmd
        return ideas

getIdeas :: Cmd -> [Setting] -> IO [Idea]
getIdeas cmd@CmdMain{..} settings = do
    settings <- return $ settings ++ map (Builtin . fst) builtinHints
    let flags = cmdParseFlags cmd
    ideas <- if cmdCross
        then applyHintFiles flags settings cmdFiles
        else concat <$> parallel cmdThreads [evaluateList =<< applyHintFile flags settings x Nothing | x <- cmdFiles]
    return $ if not (null cmdOnly)
        then [i | i <- ideas, ideaHint i `elem` cmdOnly]
        else ideas

handleRefactoring :: [Idea] -> [String] -> Cmd -> IO ()
handleRefactoring ideas files cmd@CmdMain{..} =
    case cmdFiles of
        [file] -> do
            -- Ensure that we can find the executable
            path <- checkRefactor (if cmdWithRefactor == "" then Nothing else Just cmdWithRefactor)
            -- writeFile "hlint.refact"
            let hints =  show $ map (show &&& ideaRefactoring) ideas
            withTempFile $ \f -> do
                writeFile f hints
                exitWith =<< runRefactoring path file f cmdRefactorOptions
        _ -> error "Refactor flag can only be used with an individual file"


handleReporting :: [Idea] -> Cmd -> IO ()
handleReporting showideas cmd@CmdMain{..} = do
    let outStrLn = whenNormal . putStrLn
    if null showideas then
        when (cmdReports /= []) $ outStrLn "Skipping writing reports"
     else
        forM_ cmdReports $ \x -> do
            outStrLn $ "Writing report to " ++ x ++ " ..."
            writeReport cmdDataDir x showideas
    unless cmdNoSummary $ do
        let n = length showideas
        outStrLn $ if n == 0 then "No hints" else show n ++ " hint" ++ ['s' | n/=1]

runRefactoring :: FilePath -> FilePath -> FilePath -> String -> IO ExitCode
runRefactoring rpath fin hints opts =  do
    let args = [fin, "-v0"] ++ words opts ++ ["--refact-file", hints]
    (_, _, _, phand) <- createProcess $ proc rpath args
    try $ hSetBuffering stdin LineBuffering :: IO (Either IOException ())
    hSetBuffering stdout LineBuffering
    -- Propagate the exit code from the spawn process
    waitForProcess phand

checkRefactor :: Maybe FilePath -> IO FilePath
checkRefactor rpath = do
    let excPath = fromMaybe "refactor" rpath
    mexc <- findExecutable excPath
    case mexc of
        Just exc ->  do
            ver <- readVersion . tail <$> readProcess exc ["--version"] ""
            if versionBranch ver >= [0,1,0,0]
                then return exc
                else error "Your version of refactor is too old, please upgrade to the latest version"
        Nothing -> error $ unlines [ "Could not find refactor", "Tried with: " ++ excPath ]

evaluateList :: [a] -> IO [a]
evaluateList xs = do
    evaluate $ length xs
    return xs

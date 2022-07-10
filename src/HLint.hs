{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module HLint(hlint, readAllSettings) where

import Control.Applicative
import Control.Monad.Extra
import Control.Exception.Extra
import Control.Concurrent.Extra
import System.Console.CmdArgs.Verbosity
import GHC.Util.DynFlags
import Data.List.Extra
import GHC.Conc
import System.Directory
import System.Exit
import System.IO.Extra
import System.Time.Extra
import Data.Tuple.Extra
import Prelude

import CmdLine
import Config.Read
import Config.Type
import Config.Compute
import Report
import Summary
import Idea
import Apply
import Test.All
import Hint.All
import Refact
import Timing
import Parallel
import GHC.All
import CC
import EmbedData


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
--   on your server with untrusted input.
hlint :: [String] -> IO [Idea]
hlint args = do
    startTimings
    cmd <- getCmd args
    timedIO "Initialise" "global flags" initGlobalDynFlags
    if cmdTest cmd then
        hlintTest cmd >> pure []
     else do
        (time, xs) <- duration $ hlintMain args cmd
        when (cmdTiming cmd) $ do
            printTimings
            putStrLn $ "Took " ++ showDuration time
        pure $ if cmdNoExitCode cmd then [] else xs

hlintTest :: Cmd -> IO ()
hlintTest cmd@CmdMain{..} = do
    failed <- test cmd (\args -> do errs <- hlint args; unless (null errs) $ exitWith $ ExitFailure 1) cmdDataDir cmdGivenHints
    when (failed > 0) exitFailure

cmdParseFlags :: Cmd -> ParseFlags
cmdParseFlags cmd = parseFlagsSetLanguage (cmdExtensions cmd) $ defaultParseFlags{cppFlags=cmdCpp cmd}

withVerbosity :: Verbosity -> IO a -> IO a
withVerbosity new act = do
    old <- getVerbosity
    (setVerbosity new >> act) `finally` setVerbosity old

hlintMain :: [String] -> Cmd -> IO [Idea]
hlintMain args cmd@CmdMain{..}
    | cmdDefault = do
        ideas <- if null cmdFiles then pure [] else withVerbosity Quiet $
            runHlintMain args cmd{cmdJson=False,cmdSerialise=False,cmdRefactor=False} Nothing
        let bad = group $ sort $ map ideaHint ideas
        if null bad then putStr defaultYaml else do
            let group1:groups = splitOn ["",""] $ lines defaultYaml
            let group2 = "# Warnings currently triggered by your code" :
                         ["- ignore: {name: " ++ show x ++ "} # "
                         ++ if null tl then "1 hint" else show (length xs) ++ " hints"
                         | xs@(x : tl) <- bad
                         ]

            putStr $ unlines $ intercalate ["",""] $ group1:group2:groups
        pure []
    | cmdGenerateMdSummary /= [] = do
        forM_ cmdGenerateMdSummary $ \file -> timedIO "Summary" file $ do
            whenNormal $ putStrLn $ "Writing Markdown summary to " ++ file ++ " ..."
            summary <- generateMdSummary . snd =<< readAllSettings args cmd
            writeFileBinary file summary
        pure []
    | cmdGenerateJsonSummary /= [] = do
        forM_ cmdGenerateJsonSummary $ \file -> timedIO "Summary" file $ do
            whenNormal $ putStrLn $ "Writing JSON summary to " ++ file ++ " ..."
            summary <- generateJsonSummary . snd =<< readAllSettings args cmd
            writeFileBinary file summary
        pure []
    | cmdGenerateExhaustiveConf /= [] = do
        forM_ cmdGenerateExhaustiveConf $ \severity ->
            let file = show severity ++ "-all.yaml"
             in timedIO "Exhaustive config file" file $ do
                whenNormal $ putStrLn $ "Writing " ++ show severity ++ "-all list to " ++ file ++ " ..."
                exhaustiveConfig <- generateExhaustiveConfig severity . snd =<< readAllSettings args cmd
                writeFileBinary file exhaustiveConfig
        pure []
    | null cmdFiles && not (null cmdFindHints) = do
        hints <- concatMapM (resolveFile cmd Nothing) cmdFindHints
        mapM_ (putStrLn . fst <=< computeSettings (cmdParseFlags cmd)) hints >> pure []
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
    -- if the first file is named 'lint' and there is no 'lint' file
    -- then someone is probably invoking the older hlint multi-mode command
    -- so skip it
    cmdFiles <- if not $ ["lint"] `isPrefixOf` cmdFiles then pure cmdFiles else do
        b <- doesDirectoryExist "lint"
        pure $ if b then cmdFiles else drop1 cmdFiles

    files <- concatMapM (resolveFile cmd tmpFile) cmdFiles
    if null files
        then error "No files found"
        else pure cmd { cmdFiles = files }

readAllSettings :: [String] -> Cmd -> IO (Cmd, [Setting])
readAllSettings args1 cmd@CmdMain{..} = do
    files <- cmdHintFiles cmd
    settings1 <-
        readFilesConfig $
        files
        ++ [("CommandLine.yaml",Just (enableGroup x)) | x <- cmdWithGroups]
    let args2 = [x | SettingArgument x <- settings1]
    cmd@CmdMain{..} <- if null args2 then pure cmd else getCmd $ args2 ++ args1 -- command line arguments are passed last
    settings2 <- concatMapM (fmap snd . computeSettings (cmdParseFlags cmd)) cmdFindHints
    let settings3 = [SettingClassify $ Classify Ignore x "" "" | x <- cmdIgnore]
    cmdThreads <- if cmdThreads == 0 then getNumProcessors else pure cmdThreads
    cmd <- pure CmdMain {..}
    pure (cmd, settings1 ++ settings2 ++ settings3)
    where
        enableGroup groupName =
            unlines
            ["- group:"
            ,"    name: " ++ groupName
            ,"    enabled: true"
            ]

runHints :: [String] -> [Setting] -> Cmd -> IO [Idea]
runHints args settings cmd@CmdMain{..} =
    withNumCapabilities cmdThreads $ do
        let outStrLn = whenNormal . putStrLn
        ideas <- getIdeas cmd settings
        ideas <- pure $ if cmdShowAll then ideas else  filter (\i -> ideaSeverity i /= Ignore) ideas
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
            let showItem = if usecolour then showIdeaANSI else show
            mapM_ (outStrLn . showItem) ideas
            handleReporting ideas cmd
        pure ideas

getIdeas :: Cmd -> [Setting] -> IO [Idea]
getIdeas cmd@CmdMain{..} settings = do
    settings <- pure $ settings ++ map (Builtin . fst) builtinHints
    let flags = cmdParseFlags cmd
    ideas <- if cmdCross
        then applyHintFiles flags settings cmdFiles
        else concat <$> parallel cmdThreads [evaluateList =<< applyHintFile flags settings x Nothing | x <- cmdFiles]
    pure $ if not (null cmdOnly)
        then [i | i <- ideas, ideaHint i `elem` cmdOnly]
        else ideas

-- #746: run refactor even if no hint, which ensures consistent output
-- whether there are hints or not.
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
                let ParseFlags{enabledExtensions, disabledExtensions} = cmdParseFlags cmd
                exitWith =<< runRefactoring path file f enabledExtensions disabledExtensions cmdRefactorOptions
        _ -> errorIO "Refactor flag can only be used with an individual file"

handleReporting :: [Idea] -> Cmd -> IO ()
handleReporting showideas cmd@CmdMain{..} = do
    let outStrLn = whenNormal . putStrLn
    forM_ cmdReports $ \x -> do
        outStrLn $ "Writing report to " ++ x ++ " ..."
        writeReport cmdDataDir x showideas
    unless cmdNoSummary $ do
        let n = length showideas
        outStrLn $ if n == 0 then "No hints" else show n ++ " hint" ++ ['s' | n/=1]

evaluateList :: [a] -> IO [a]
evaluateList xs = do
    evaluate $ length xs
    pure xs

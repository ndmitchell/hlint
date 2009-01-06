
module CmdLine(Cmd(..), getCmd) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import HSE.All

import Util
import Paths_hlint
import Data.Version


data Cmd = Cmd
    {cmdTest :: Bool                 -- ^ run in test mode?
    ,cmdFiles :: [FilePath]          -- ^ which files to run it on
    ,cmdHintFiles :: [FilePath]      -- ^ which settingsfiles to use
    ,cmdReports :: [FilePath]        -- ^ where to generate reports
    ,cmdIgnore :: [String]           -- ^ the hints to ignore
    ,cmdShowAll :: Bool              -- ^ display all skipped items
    }


data Opts = Help | Test
          | Hints FilePath
          | Report FilePath
          | Skip String | ShowAll
            deriving Eq


opts = [Option "?" ["help"] (NoArg Help) "Display help message"
       ,Option "r" ["report"] (OptArg (Report . fromMaybe "report.html") "file") "Generate a report in HTML"
       ,Option "h" ["hint"] (ReqArg Hints "file") "Hint/ignore file to use"
       ,Option "i" ["ignore"] (ReqArg Skip "message") "Ignore a particular hint"
       ,Option "s" ["show"] (NoArg ShowAll) "Show all ignored ideas"
       ,Option "t" ["test"] (NoArg Test) "Run in test mode"
       ]


-- | Exit out if you need to display help info
getCmd :: IO Cmd
getCmd = do
    args <- getArgs
    let (opt,files,err) = getOpt Permute opts args
    let test = Test `elem` opt
    unless (null err) $
        error $ unlines $ "Unrecognised arguments:" : err

    when (Help `elem` opt || (null files && not test)) $ do
        helpMsg
        exitWith ExitSuccess

    files <- liftM concat $ mapM getFile files
    return Cmd
        {cmdTest = test
        ,cmdFiles = files
        ,cmdHintFiles = [x | Hints x <- opt]
        ,cmdReports = [x | Report x <- opt]
        ,cmdIgnore = [x | Skip x <- opt]
        ,cmdShowAll = ShowAll `elem` opt
        }


helpMsg :: IO ()
helpMsg = putStr $ unlines
    ["HLint v" ++ showVersion version ++ ", (C) Neil Mitchell 2006-2009, University of York"
    ,""
    ,"  hlint [files/directories] [options]"
    ,usageInfo "" opts
    ,"HLint makes hints on how to improve some Haskell code."
    ,""
    ,"For example, to check all .hs and .lhs files in the folder src and"
    ,"generate a report:"
    ,"  hlint src --report"
    ]


getFile :: FilePath -> IO [FilePath]
getFile file = do
    b <- doesDirectoryExist file
    if b then do
        xs <- getDirectoryContentsRecursive file
        return [x | x <- xs, takeExtension x `elem` [".hs",".lhs"]]
     else do
        b <- doesFileExist file
        unless b $ error $ "Couldn't find file: " ++ file
        return [file]

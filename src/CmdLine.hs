
module CmdLine(Cmd(..), getCmd) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Language.Preprocessor.Cpphs

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
    ,cmdColor :: Bool                -- ^ color the result
    ,cmdCpphs :: CpphsOptions        -- ^ options for cpphs
    }


data Opts = Help | Ver | Test
          | Hints FilePath
          | Report FilePath
          | Skip String | ShowAll
          | Color
          | Define String
          | Include String
            deriving Eq


opts = [Option "?" ["help"] (NoArg Help) "Display help message"
       ,Option "v" ["version"] (NoArg Ver) "Display version information"
       ,Option "r" ["report"] (OptArg (Report . fromMaybe "report.html") "file") "Generate a report in HTML"
       ,Option "h" ["hint"] (ReqArg Hints "file") "Hint/ignore file to use"
       ,Option "c" ["color","colour"] (NoArg Color) "Color the output (requires ANSI terminal)"
       ,Option "i" ["ignore"] (ReqArg Skip "message") "Ignore a particular hint"
       ,Option "s" ["show"] (NoArg ShowAll) "Show all ignored ideas"
       ,Option "t" ["test"] (NoArg Test) "Run in test mode"
       ,Option ""  ["cpp-define"] (ReqArg Define "name[=value]") "CPP #define"
       ,Option ""  ["cpp-include"] (ReqArg Include "dir") "CPP include path"
       ]


-- | Exit out if you need to display help info
getCmd :: IO Cmd
getCmd = do
    args <- getArgs
    let (opt,files,err) = getOpt Permute opts args
    let test = Test `elem` opt
    unless (null err) $
        error $ unlines $ "Unrecognised arguments:" : err

    when (Ver `elem` opt) $ do
        putStr versionText
        exitWith ExitSuccess

    when (Help `elem` opt || (null files && not test)) $ do
        putStr helpText
        exitWith ExitSuccess

    files <- concatMapM getFile files
    
    let hintFiles = [x | Hints x <- opt]
    hints <- mapM getHintFile $ hintFiles ++ ["HLint" | null hintFiles]

    let cpphs = defaultCpphsOptions
            {boolopts=defaultBoolOptions{locations=False}
            ,includes = [x | Include x <- opt]
            ,defines = [(a,drop 1 b) | Define x <- opt, let (a,b) = break (== '=') x]
            }

    return Cmd
        {cmdTest = test
        ,cmdFiles = files
        ,cmdHintFiles = hints
        ,cmdReports = [x | Report x <- opt]
        ,cmdIgnore = [x | Skip x <- opt]
        ,cmdShowAll = ShowAll `elem` opt
        ,cmdColor = Color `elem` opt
        ,cmdCpphs = cpphs
        }


versionText :: String
versionText = "HLint v" ++ showVersion version ++ ", (C) Neil Mitchell 2006-2009\n"

helpText :: String
helpText = unlines
    [versionText
    ,"  hlint [files/directories] [options]"
    ,usageInfo "" opts
    ,"HLint gives hints on how to improve Haskell code."
    ,""
    ,"To check all Haskell files in 'src' and generate a report type:"
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


getHintFile :: FilePath -> IO FilePath
getHintFile x = do
        dat <- getDataDir
        let poss = nub $ concat [x : [x <.> "hs" | takeExtension x /= ".hs"] | x <- [x,dat </> x]]
        f poss poss
    where
        f o [] = error $ unlines $ [
            "Couldn't find file: " ++ x,
            "Tried with:"] ++ map ("  "++) o
        f o (x:xs) = do
            b <- doesFileExist x
            if b then return x else f o xs

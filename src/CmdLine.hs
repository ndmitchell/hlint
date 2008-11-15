
module CmdLine(Mode(..), getMode) where

import Control.Monad
import Data.List
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Util

import Paths_drhaskell
import Data.Version


data Mode = Mode
    {modeHints :: [FilePath]  -- ^ which hint files to use
    ,modeFiles :: [FilePath]  -- ^ which files to run it on
    ,modeTest :: Bool         -- ^ run in test mode?
    }


data Opts = Help | HintFile FilePath | Test
            deriving Eq

opts = [Option "?" ["help"] (NoArg Help) "Display help message"
       ,Option "h" ["hint"] (ReqArg HintFile "file") "Hint file to use"
       ,Option "t" ["test"] (NoArg Test) "Run in test mode"
       ]


-- | Exit out if you need to display help info
getMode :: IO Mode
getMode = do
    args <- getArgs
    let (opt,files,err) = getOpt Permute opts args
    let test = Test `elem` opt
    when (not $ null err) $
        error $ unlines $ "Unrecognised arguments:" : err

    when (Help `elem` opt || (null files && not test)) $ do
        putStr $ unlines ["Dr Haskell v" ++ showVersion version ++ ", (C) Neil Mitchell 2006-2008, University of York"
                         ,""
                         ,"  drhaskell [files] [options]"
                         ,usageInfo "" opts
                         ,"Dr Haskell makes hints on how to improve some Haskell code."]
        exitWith ExitSuccess

    let hints = ifNull [x | HintFile x <- opt] ["Hints.hs"]
    files <- liftM concat $ mapM getFile files
    files <- return $ if null files && test then ["Test.hs"] else files
    return Mode{modeHints=hints, modeFiles=files, modeTest=test}


ifNull :: [a] -> [a] -> [a]
ifNull x y = if null x then y else x


getFile :: FilePath -> IO [FilePath]
getFile file = do
    b <- doesDirectoryExist file
    if b then f file else do
        b <- doesFileExist file
        if b then return [file] else do
            dat <- getDataDir
            b <- doesFileExist (dat </> file)
            if b then return [file] else error $ "Couldn't find file: " ++ file
    where
        f file | takeExtension file `elem` [".hs",".lhs"] = return [file]
        f file = do
            b <- doesDirectoryExist file
            if not b then return [] else do
                s <- getDirectoryContents file
                liftM concat $ mapM (f . (</>) file) $ filter (not . isBadDir) s


isBadDir :: FilePath -> Bool
isBadDir x = "." `isPrefixOf` x || "_" `isPrefixOf` x

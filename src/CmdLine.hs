{-# LANGUAGE PatternGuards, RecordWildCards, DeriveDataTypeable #-}

module CmdLine(Cmd(..), cmdCpp, CppFlags(..), getCmd, cmdExtensions, cmdHintFiles, exitWithHelp, resolveFile) where

import Data.Char
import Data.List
import System.Console.CmdArgs.Implicit
import System.Directory
import System.Exit
import System.FilePath
import Language.Preprocessor.Cpphs
import Language.Haskell.Exts.Extension
import System.Environment

import Util
import Paths_hlint
import Data.Version

getCmd :: [String] -> IO Cmd
getCmd args = withArgs args $ automatic =<< cmdArgsRun mode


automatic :: Cmd -> IO Cmd
automatic Cmd{..} = do
    cmdDataDir <- if cmdDataDir == "" then getDataDir else return cmdDataDir
    cmdPath <- return $ if null cmdPath then ["."] else cmdPath
    cmdExtension <- return $ if null cmdExtension then ["hs", "lhs"] else cmdExtension
    return Cmd{..}


exitWithHelp :: IO a
exitWithHelp = do
    putStr $ show mode
    exitSuccess


-- | What C pre processor should be used.
data CppFlags
    = NoCpp -- ^ No pre processing is done.
    | CppSimple -- ^ Lines prefixed with @#@ are stripped.
    | Cpphs CpphsOptions -- ^ The @cpphs@ library is used.


data Cmd = Cmd
    {cmdFiles :: [FilePath]    -- ^ which files to run it on, nothing = none given
    ,cmdReports :: [FilePath]        -- ^ where to generate reports
    ,cmdGivenHints :: [FilePath]     -- ^ which settignsfiles were explicitly given
    ,cmdWithHints :: [String]        -- ^ hints that are given on the command line
    ,cmdColor :: Bool                -- ^ color the result
    ,cmdIgnore :: [String]           -- ^ the hints to ignore
    ,cmdShowAll :: Bool              -- ^ display all skipped items
    ,cmdExtension :: [String]        -- ^ extensions
    ,cmdLanguage :: [String]      -- ^ the extensions (may be prefixed by "No")
    ,cmdUtf8 :: Bool
    ,cmdEncoding :: String         -- ^ the text encoding
    ,cmdCross :: Bool                -- ^ work between source files, applies to hints such as duplicate code between modules
    ,cmdFindHints :: [FilePath]      -- ^ source files to look for hints in
    ,cmdTest :: Bool                 -- ^ run in test mode?
    ,cmdDataDir :: FilePath          -- ^ the data directory
    ,cmdPath :: [String]
    ,cmdProof :: [FilePath]          -- ^ a proof script to check against
    ,cmdCppDefine :: [String]
    ,cmdCppInclude :: [FilePath]
    ,cmdCppSimple :: Bool
    ,cmdCppAnsi :: Bool
    } deriving (Data,Typeable,Show)

mode = cmdArgsMode $ Cmd
    {cmdFiles = def &= args &= typ "FILE/DIR"
    ,cmdReports = nam "report" &= opt "report.html" &= typFile &= help "Generate a report in HTML"
    ,cmdGivenHints = nam "hint" &= typFile &= help "Hint/ignore file to use"
    ,cmdWithHints = nam "with" &= typ "HINT" &= help "Extra hints to use"
    ,cmdColor = nam "colour" &= name "color" &= help "Color output (requires ANSI terminal)"
    ,cmdIgnore = nam "ignore" &= typ "HINT" &= help "Ignore a particular hint"
    ,cmdShowAll = nam "show" &= help "Show all ignored ideas"
    ,cmdExtension = nam "extension" &= typ "EXT" &= help "File extensions to search (default hs/lhs)"
    ,cmdLanguage = nam_ "language" &= name "X" &= typ "EXTENSION" &= help "Language extensions (Arrows, NoCPP)"
    ,cmdUtf8 = nam "utf8" &= help "Use UTF-8 text encoding"
    ,cmdEncoding = nam_ "encoding" &= typ "ENCODING" &= help "Choose the text encoding"
    ,cmdCross = nam_ "cross" &= help "Work between modules"
    ,cmdFindHints = nam "find" &= typFile &= help "Find hints in a Haskell file"
    ,cmdTest = nam "test" &= help "Run in test mode"
    ,cmdDataDir = nam "datadir" &= typDir &= help "Override the data directory"
    ,cmdPath = nam "path" &= help "Directory in which to search for files"
    ,cmdProof = nam_ "proof" &= typFile &= help "Isabelle/HOLCF theory file"
    ,cmdCppDefine = nam_ "cpp-define" &= typ "NAME[=VALUE]" &= help "CPP #define"
    ,cmdCppInclude = nam_ "cpp-include" &= typDir &= help "CPP include path"
    ,cmdCppSimple = nam_ "cpp-simple" &= help "Use a simple CPP (strip # lines)"
    ,cmdCppAnsi = nam_ "cpp-ansi" &= help "Use CPP in ANSI compatibility mode"
    } &= explicit &= name "hlint" &= program "hlint"
    &= summary ("HLint v" ++ showVersion version ++ ", (C) Neil Mitchell 2006-2014")
    &= details ["HLint gives hints on how to improve Haskell code."
             ,""
             ,"To check all Haskell files in 'src' and generate a report type:"
             ,"  hlint src --report"]
    where
        nam xs@(x:_) = nam_ xs &= name [x]
        nam_ xs = def &= explicit &= name xs

cmdHintFiles :: Cmd -> IO [FilePath]
cmdHintFiles Cmd{..} = mapM (getHintFile cmdDataDir) $ cmdGivenHints ++ ["HLint" | null cmdGivenHints && null cmdWithHints]

cmdExtensions :: Cmd -> [Extension]
cmdExtensions = getExtensions . cmdLanguage


cmdCpp :: Cmd -> CppFlags
cmdCpp cmd@Cmd{..}
    | cmdCppSimple = CppSimple
    | EnableExtension CPP `elem` cmdExtensions cmd = Cpphs defaultCpphsOptions
        {boolopts=defaultBoolOptions{hashline=False, stripC89=True, ansi=cmdCppAnsi}
        ,includes = cmdCppInclude
        ,defines = [(a,drop 1 b) | x <- cmdCppDefine, let (a,b) = break (== '=') x]
        }
    | otherwise = NoCpp


"." <\> x = x
x <\> y = x </> y


resolveFile :: Cmd -> FilePath -> IO [FilePath]
resolveFile Cmd{..} = getFile cmdPath cmdExtension


getFile :: [FilePath] -> [String] -> FilePath -> IO [FilePath]
getFile path _ "-" = return ["-"]
getFile [] exts file = error $ "Couldn't find file: " ++ file
getFile (p:ath) exts file = do
    isDir <- doesDirectoryExist $ p <\> file
    if isDir then do
        xs <- getDirectoryContentsRecursive $ p <\> file
        return [x | x <- xs, drop 1 (takeExtension x) `elem` exts]
     else do
        isFil <- doesFileExist $ p <\> file
        if isFil then return [p <\> file]
         else do
            res <- getModule p exts file
            case res of
                Just x -> return [x]
                Nothing -> getFile ath exts file


getModule :: FilePath -> [String] -> FilePath -> IO (Maybe FilePath)
getModule path exts x | not (any isSpace x) && all isMod xs = f exts
    where
        xs = words $ map (\x -> if x == '.' then ' ' else x) x
        isMod (x:xs) = isUpper x && all (\x -> isAlphaNum x || x == '_') xs
        isMod _ = False
        pre = path <\> joinPath xs

        f [] = return Nothing
        f (x:xs) = do
            let s = pre <.> x
            b <- doesFileExist s
            if b then return $ Just s else f xs
getModule _ _ _ = return Nothing


getHintFile :: FilePath -> FilePath -> IO FilePath
getHintFile _ "-" = return "-"
getHintFile dataDir x = do
        let poss = nub $ concat [x : [x <.> "hs" | takeExtension x /= ".hs"] | x <- [x,dataDir </> x]]
        f poss poss
    where
        f o [] = error $ unlines $ [
            "Couldn't find file: " ++ x,
            "Tried with:"] ++ map ("  "++) o
        f o (x:xs) = do
            b <- doesFileExist x
            if b then return x else f o xs


getExtensions :: [String] -> [Extension]
getExtensions = foldl f defaultExtensions
    where
        f a "Haskell98" = []
        f a ('N':'o':x) | Just x <- readExtension x = delete x a
        f a x | Just x <- readExtension x = x : delete x a
        f a x = error $ "Unknown extension: " ++ x


readExtension :: String -> Maybe Extension
readExtension x = case classifyExtension x of
    UnknownExtension _ -> Nothing
    x -> Just x

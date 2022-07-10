{-# LANGUAGE PatternGuards, DeriveDataTypeable, TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-fields -fno-cse -O0 #-}

module CmdLine(
    Cmd(..), getCmd,
    CppFlags(..), cmdCpp, cmdExtensions, cmdHintFiles, cmdUseColour,
    exitWithHelp, resolveFile
    ) where

import Control.Monad.Extra
import Control.Exception.Extra
import qualified Data.ByteString as BS
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Functor
import GHC.All(CppFlags(..))
import GHC.LanguageExtensions.Type
import Language.Haskell.GhclibParserEx.GHC.Driver.Session as GhclibParserEx
import GHC.Driver.Session hiding (verbosity)

import Language.Preprocessor.Cpphs
import System.Console.ANSI(hSupportsANSIWithoutEmulation)
import System.Console.CmdArgs.Explicit(helpText, HelpFormat(..))
import System.Console.CmdArgs.Implicit
import System.Directory.Extra
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Process
import System.FilePattern

import EmbedData
import Util
import Timing
import Extension
import Paths_hlint
import Data.Version
import Prelude
import Config.Type (Severity (Warning))


getCmd :: [String] -> IO Cmd
getCmd args = withArgs (map f args) $ automatic =<< cmdArgsRun mode
    where f x = if x == "-?" || x == "--help" then "--help=all" else x


automatic :: Cmd -> IO Cmd
automatic cmd = dataDir =<< path =<< git =<< extension cmd
    where
        path cmd = pure $ if null $ cmdPath cmd then cmd{cmdPath=["."]} else cmd
        extension cmd = pure $ if null $ cmdExtension cmd then cmd{cmdExtension=["hs","lhs"]} else cmd
        dataDir cmd
            | cmdDataDir cmd  /= "" = pure cmd
            | otherwise = do
                x <- getDataDir
                b <- doesDirectoryExist x
                if b then pure cmd{cmdDataDir=x} else do
                    exe <- getExecutablePath
                    pure cmd{cmdDataDir = takeDirectory exe </> "data"}
        git cmd
            | cmdGit cmd = do
                mgit <- findExecutable "git"
                case mgit of
                    Nothing -> errorIO "Could not find git"
                    Just git -> do
                        let args = ["ls-files", "--cached", "--others", "--exclude-standard"] ++
                                   map ("*." ++) (cmdExtension cmd)
                        files <- timedIO "Execute" (unwords $ git:args) $
                            readProcess git args ""
                        pure cmd{cmdFiles = cmdFiles cmd ++ lines files}
            | otherwise = pure cmd


exitWithHelp :: IO a
exitWithHelp = do
    putStr $ show $ helpText [] HelpFormatAll mode
    exitSuccess


-- | When to colour terminal output.
data ColorMode
    = Never  -- ^ Terminal output will never be coloured.
    | Always -- ^ Terminal output will always be coloured.
    | Auto   -- ^ Terminal output will be coloured if $TERM and stdout appear to support it.
      deriving (Show, Typeable, Data)


instance Default ColorMode where
  def = Auto


data Cmd
    = CmdMain
        {cmdFiles :: [FilePath]    -- ^ which files to run it on, nothing = none given
        ,cmdReports :: [FilePath]        -- ^ where to generate reports
        ,cmdGivenHints :: [FilePath]     -- ^ which settignsfiles were explicitly given
        ,cmdWithGroups :: [String]       -- ^ groups that are given on the command line
        ,cmdGit :: Bool                  -- ^ use git ls-files to find files
        ,cmdColor :: ColorMode           -- ^ color the result
        ,cmdThreads :: Int              -- ^ Numbmer of threads to use, 0 = whatever GHC has
        ,cmdIgnore :: [String]           -- ^ the hints to ignore
        ,cmdShowAll :: Bool              -- ^ display all skipped items
        ,cmdExtension :: [String]        -- ^ extensions
        ,cmdLanguage :: [String]      -- ^ the extensions (may be prefixed by "No")
        ,cmdCross :: Bool                -- ^ work between source files, applies to hints such as duplicate code between modules
        ,cmdFindHints :: [FilePath]      -- ^ source files to look for hints in
        ,cmdDataDir :: FilePath          -- ^ the data directory
        ,cmdDefault :: Bool              -- ^ Print a default file to stdout
        ,cmdPath :: [String]
        ,cmdCppDefine :: [String]
        ,cmdCppInclude :: [FilePath]
        ,cmdCppFile :: [FilePath]
        ,cmdCppSimple :: Bool
        ,cmdCppAnsi :: Bool
        ,cmdJson :: Bool                -- ^ display hint data as JSON
        ,cmdCC :: Bool                  -- ^ display hint data as Code Climate Issues
        ,cmdNoSummary :: Bool           -- ^ do not show the summary info
        ,cmdOnly :: [String]            -- ^ specify which hints explicitly
        ,cmdNoExitCode :: Bool
        ,cmdTiming :: Bool
        ,cmdSerialise :: Bool           -- ^ Display hints in serialisation format
        ,cmdRefactor :: Bool            -- ^ Run the `refactor` executable to automatically perform hints
        ,cmdRefactorOptions :: String   -- ^ Options to pass to the `refactor` executable.
        ,cmdWithRefactor :: FilePath    -- ^ Path to refactor tool
        ,cmdIgnoreGlob :: [FilePattern]
        ,cmdGenerateMdSummary :: [FilePath]  -- ^ Generate a summary of available hints, in Markdown format
        ,cmdGenerateJsonSummary :: [FilePath]  -- ^ Generate a summary of built-in hints, in JSON format
        ,cmdGenerateExhaustiveConf :: [Severity]  -- ^ Generate a hlint config file with all built-in hints set to the specified level
        ,cmdTest :: Bool
        }
    deriving (Data,Typeable,Show)

mode = cmdArgsMode $ modes
    [CmdMain
        {cmdFiles = def &= args &= typ "FILE/DIR"
        ,cmdReports = nam "report" &= opt "report.html" &= typFile &= help "Generate a report in HTML"
        ,cmdGivenHints = nam "hint" &= typFile &= help "Hint/ignore file to use"
        ,cmdWithGroups = nam_ "with-group" &= typ "GROUP" &= help "Extra hint groups to use"
        ,cmdGit = nam "git" &= help "Run on files tracked by git"
        ,cmdColor = nam "colour" &= name "color" &= opt Always &= typ "always/never/auto" &= help "Color output (requires an ANSI terminal; 'auto' means on if the standard output channel can support ANSI; by itself, selects 'always')"
        ,cmdThreads = 1 &= name "threads" &= name "j" &= opt (0 :: Int) &= help "Number of threads to use (-j for all)"
        ,cmdIgnore = nam "ignore" &= typ "HINT" &= help "Ignore a particular hint"
        ,cmdShowAll = nam "show" &= help "Show all ignored ideas"
        ,cmdExtension = nam "extension" &= typ "EXT" &= help "File extensions to search (default hs/lhs)"
        ,cmdLanguage = nam_ "language" &= name "X" &= typ "EXTENSION" &= help "Language extensions (Arrows, NoCPP)"
        ,cmdCross = nam_ "cross" &= help "Work between modules"
        ,cmdFindHints = nam "find" &= typFile &= help "Find hints in a Haskell file"
        ,cmdDataDir = nam_ "datadir" &= typDir &= help "Override the data directory"
        ,cmdDefault = nam "default" &= help "Print a default file to stdout"
        ,cmdPath = nam "path" &= help "Directory in which to search for files"
        ,cmdCppDefine = nam_ "cpp-define" &= typ "NAME[=VALUE]" &= help "CPP #define"
        ,cmdCppInclude = nam_ "cpp-include" &= typDir &= help "CPP include path"
        ,cmdCppFile = nam_ "cpp-file" &= typFile &= help "CPP pre-include file"
        ,cmdCppSimple = nam_ "cpp-simple" &= help "Use a simple CPP (strip # lines)"
        ,cmdCppAnsi = nam_ "cpp-ansi" &= help "Use CPP in ANSI compatibility mode"
        ,cmdJson = nam_ "json" &= help "Display hint data as JSON"
        ,cmdCC = nam_ "cc" &= help "Display hint data as Code Climate Issues"
        ,cmdNoSummary = nam_ "no-summary" &= help "Do not show summary information"
        ,cmdOnly = nam "only" &= typ "HINT" &= help "Specify which hints explicitly"
        ,cmdNoExitCode = nam_ "no-exit-code" &= help "Do not give a negative exit if hints"
        ,cmdTiming = nam_ "timing" &= help "Display timing information"
        ,cmdSerialise = nam_ "serialise" &= help "Serialise hint data for consumption by apply-refact"
        ,cmdRefactor = nam_ "refactor" &= help "Automatically invoke `refactor` to apply hints"
        ,cmdRefactorOptions = nam_ "refactor-options" &= typ "OPTIONS" &= help "Options to pass to the `refactor` executable"
        ,cmdWithRefactor = nam_ "with-refactor" &= help "Give the path to refactor"
        ,cmdIgnoreGlob = nam_ "ignore-glob" &= help "Ignore paths matching glob pattern (e.g. foo/bar/*.hs)"
        ,cmdGenerateMdSummary = nam_ "generate-summary" &= opt "hints.md" &= help "Generate a summary of available hints, in Mardown format"
        ,cmdGenerateJsonSummary = nam_ "generate-summary-json" &= opt "hints.json" &= help "Generate a summary of available hints, in JSON format"
        ,cmdGenerateExhaustiveConf = nam_ "generate-config" &= opt Warning &= typ "LEVEL" &= help "Generate a .hlint.yaml config file with all hints set to the specified severity level (default level: warn, alternatives: ignore, suggest, error)"
        ,cmdTest = nam_ "test" &= help "Run the test suite"
        } &= auto &= explicit &= name "lint"
        &= details ["HLint gives hints on how to improve Haskell code."
                   ,""
                   ,"To check all Haskell files in 'src' and generate a report type:"
                   ,"  hlint src --report"]
    ] &= program "hlint" &= verbosity
    &=  summary ("HLint v" ++ showVersion version ++ ", (C) Neil Mitchell 2006-2022")
    where
        nam xs = nam_ xs &= name [head xs]
        nam_ xs = def &= explicit &= name xs

-- | Where should we find the configuration files?
--   Either we use the implicit search, or we follow the cmdGivenHints
--   We want more important hints to go last, since they override
cmdHintFiles :: Cmd -> IO [(FilePath, Maybe String)]
cmdHintFiles cmd = do
    let explicit = cmdGivenHints cmd
    bad <- filterM (notM . doesFileExist) explicit
    when (bad /= []) $
        fail $ unlines $ "Failed to find requested hint files:" : map ("  "++) bad

    -- if the user has given any explicit hints, ignore the local ones
    implicit <- if explicit /= []
                  || cmdGenerateMdSummary cmd /= []
                  || cmdGenerateJsonSummary cmd /= []
                  || cmdGenerateExhaustiveConf cmd /= [] then pure Nothing else do
        -- we follow the stylish-haskell config file search policy
        -- 1) current directory or its ancestors; 2) home directory
        curdir <- getCurrentDirectory
        -- Ignores home directory when it isn't present.
        home <- catchIOError ((:[]) <$> getHomeDirectory) (const $ pure [])
        findM doesFileExist $
            map (</> ".hlint.yaml") (ancestors curdir ++ home) -- to match Stylish Haskell
    pure $ hlintYaml : map (,Nothing) (maybeToList implicit ++ explicit)
    where
        ancestors = init . map joinPath . reverse . inits . splitPath

cmdExtensions :: Cmd -> (Maybe Language, ([Extension], [Extension]))
cmdExtensions = getExtensions . cmdLanguage


cmdCpp :: Cmd -> CppFlags
cmdCpp cmd
    | cmdCppSimple cmd = CppSimple
    | otherwise = Cpphs defaultCpphsOptions
        {boolopts=defaultBoolOptions{hashline=False, stripC89=True, ansi=cmdCppAnsi cmd}
        ,includes = cmdCppInclude cmd
        ,preInclude = cmdCppFile cmd
        ,defines = ("__HLINT__","1") : [(a,drop1 b) | x <- cmdCppDefine cmd, let (a,b) = break (== '=') x]
        }


-- | Determines whether to use colour or not.
cmdUseColour :: Cmd -> IO Bool
cmdUseColour cmd = case cmdColor cmd of
  Always -> pure True
  Never  -> pure False
  Auto   -> do
    supportsANSI <- hSupportsANSIWithoutEmulation stdout
    pure $ Just True == supportsANSI


"." <\> x = x
x <\> y = x </> y


resolveFile
    :: Cmd
    -> Maybe FilePath -- ^ Temporary file
    -> FilePath       -- ^ File to resolve, may be "-" for stdin
    -> IO [FilePath]
resolveFile cmd = getFile (toPredicate $ cmdIgnoreGlob cmd) (cmdPath cmd) (cmdExtension cmd)
    where
        toPredicate :: [FilePattern] -> FilePath -> Bool
        toPredicate [] = const False
        toPredicate globs = \x -> not $ null $ m [((), cleanup x)]
            where m = matchMany (map ((),) globs)

        cleanup :: FilePath -> FilePath
        cleanup ('.':x:xs) | isPathSeparator x, not $ null xs = xs
        cleanup x = x


getFile :: (FilePath -> Bool) -> [FilePath] -> [String] -> Maybe FilePath -> FilePath -> IO [FilePath]
getFile _ path _ (Just tmpfile) "-" =
    -- make sure we don't reencode any Unicode
    BS.getContents >>= BS.writeFile tmpfile >> pure [tmpfile]
getFile _ path _ Nothing "-" = pure ["-"]
getFile _ [] exts _ file = exitMessage $ "Couldn't find file: " ++ file
getFile ignore (p:ath) exts t file = do
    isDir <- doesDirectoryExist $ p <\> file
    if isDir then do
        let ignoredDirectories = ["dist", "dist-newstyle"]
            avoidDir x = let y = takeFileName x in "_" `isPrefixOf` y || ("." `isPrefixOf` y && not (all (== '.') y)) || y `elem` ignoredDirectories || ignore x
            avoidFile x = let y = takeFileName x in "." `isPrefixOf` y || ignore x
        xs <- listFilesInside (pure . not . avoidDir) $ p <\> file
        pure [x | x <- xs, drop1 (takeExtension x) `elem` exts, not $ avoidFile x]
     else do
        isFil <- doesFileExist $ p <\> file
        if isFil then pure [p <\> file]
         else do
            res <- getModule p exts file
            case res of
                Just x -> pure [x]
                Nothing -> getFile ignore ath exts t file


getModule :: FilePath -> [String] -> FilePath -> IO (Maybe FilePath)
getModule path exts x | not (any isSpace x) && all isMod xs = f exts
    where
        xs = words $ map (\x -> if x == '.' then ' ' else x) x
        isMod (x:xs) = isUpper x && all (\x -> isAlphaNum x || x == '_') xs
        isMod _ = False
        pre = path <\> joinPath xs

        f [] = pure Nothing
        f (x:xs) = do
            let s = pre <.> x
            b <- doesFileExist s
            if b then pure $ Just s else f xs
getModule _ _ _ = pure Nothing


getExtensions :: [String] -> (Maybe Language, ([Extension], [Extension]))
getExtensions args = (lang, foldl f (startExts, []) exts)
  where
        -- If a language specifier is provided e.g. Haskell98 or
        -- Haskell2010 or GHC2021, then it represents a specific set
        -- of extensions which we default enable.

        -- If no language specifier is provided we construct our own
        -- set of extensions to default enable. The set that we
        -- construct default enables more extensions than GHC would
        -- default enable were it to be invoked without an explicit
        -- language specifier given.
        startExts :: [Extension]
        startExts = case lang of
          Nothing -> defaultExtensions
          Just _ -> GHC.Driver.Session.languageExtensions lang

        -- If multiple languages are given, the last language "wins".
        lang :: Maybe Language
        lang = fromJust . flip lookup ls . snd <$> unsnoc langs

        langs, exts :: [String]
        (langs, exts) = partition (isJust . flip lookup ls) args
        ls = [ (show x, x) | x <- [Haskell98, Haskell2010 , GHC2021] ]

        f :: ([Extension], [Extension]) -> String -> ([Extension], [Extension])
        f (a, e) ('N':'o':x) | Just x <- GhclibParserEx.readExtension x, let xs = expandDisable x = (deletes xs a, xs ++ deletes xs e)
        f (a, e) x | Just x <- GhclibParserEx.readExtension x = (x : delete x a, delete x e)
        f (a, e) x = error $ "Unknown extension: '" ++ x ++ "'"

        deletes :: [Extension] -> [Extension] -> [Extension]
        deletes [] ys = ys
        deletes (x : xs) ys = deletes xs $ delete x ys

        -- if you disable a feature that implies another feature, sometimes we should disable both
        -- e.g. no one knows what TemplateHaskellQuotes is https://github.com/ndmitchell/hlint/issues/1038
        expandDisable :: Extension -> [Extension]
        expandDisable TemplateHaskell = [TemplateHaskell, TemplateHaskellQuotes]
        expandDisable x = [x]

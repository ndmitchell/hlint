{-# LANGUAGE PatternGuards, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-cse -O0 #-}

module CmdLine(
    Cmd(..), getCmd,
    CppFlags(..), cmdCpp, cmdExtensions, cmdHintFiles, cmdUseColour,
    exitWithHelp, resolveFile
    ) where

import Control.Monad.Extra
import Data.Char
import Data.List
import System.Console.ANSI(hSupportsANSI)
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Explicit(helpText, HelpFormat(..))
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.IO
import System.Process(readProcess)
import Language.Preprocessor.Cpphs
import HSE.All(CppFlags(..))
import Language.Haskell.Exts(defaultParseMode, baseLanguage)
import Language.Haskell.Exts.Extension
import Data.Maybe
import System.Environment.Extra
import System.Info.Extra

import Util
import Paths_hlint
import Data.Version


getCmd :: [String] -> IO Cmd
getCmd args = withArgs (map f args) $ automatic =<< cmdArgsRun mode
    where f x = if x == "-?" || x == "--help" then "--help=all" else x


automatic :: Cmd -> IO Cmd
automatic cmd = case cmd of
    CmdMain{} -> dataDir =<< path =<< git =<< extension cmd
    CmdGrep{} -> path =<< extension cmd
    CmdTest{} -> dataDir cmd
    _ -> return cmd
    where
        path cmd = return $ if null $ cmdPath cmd then cmd{cmdPath=["."]} else cmd
        extension cmd = return $ if null $ cmdExtension cmd then cmd{cmdExtension=["hs","lhs"]} else cmd
        dataDir cmd
            | cmdDataDir cmd  /= "" = return cmd
            | otherwise = do
                x <- getDataDir
                b <- doesDirectoryExist x
                if b then return cmd{cmdDataDir=x} else do
                    exe <- getExecutablePath
                    return cmd{cmdDataDir = takeDirectory exe </> "data"}
        git cmd
            | cmdGit cmd = do
                mgit <- findExecutable "git"
                case mgit of
                    Nothing -> error "Could not find git"
                    Just git -> do
                        let args = ["ls-files", "--cached", "--others", "--exclude-standard"] ++
                                   map ("*." ++) (cmdExtension cmd)
                        files <- readProcess git args ""
                        return cmd{cmdFiles = cmdFiles cmd ++ lines files}
            | otherwise = return cmd


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
  def = if isWindows then Never else Auto


data Cmd
    = CmdMain
        {cmdFiles :: [FilePath]    -- ^ which files to run it on, nothing = none given
        ,cmdReports :: [FilePath]        -- ^ where to generate reports
        ,cmdGivenHints :: [FilePath]     -- ^ which settignsfiles were explicitly given
        ,cmdWithHints :: [String]        -- ^ hints that are given on the command line
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
        ,cmdSerialise :: Bool           -- ^ Display hints in serialisation format
        ,cmdRefactor :: Bool            -- ^ Run the `refactor` executable to automatically perform hints
        ,cmdRefactorOptions :: String   -- ^ Options to pass to the `refactor` executable.
        ,cmdWithRefactor :: FilePath    -- ^ Path to refactor tool
        }
    | CmdGrep
        {cmdFiles :: [FilePath]    -- ^ which files to run it on, nothing = none given
        ,cmdPattern :: String
        ,cmdExtension :: [String]        -- ^ extensions
        ,cmdLanguage :: [String]      -- ^ the extensions (may be prefixed by "No")
        ,cmdPath :: [String]
        ,cmdCppDefine :: [String]
        ,cmdCppInclude :: [FilePath]
        ,cmdCppFile :: [FilePath]
        ,cmdCppSimple :: Bool
        ,cmdCppAnsi :: Bool
        }
    | CmdTest
        {cmdProof :: [FilePath]          -- ^ a proof script to check against
        ,cmdGivenHints :: [FilePath]     -- ^ which settings files were explicitly given
        ,cmdDataDir :: FilePath          -- ^ the data directory
        ,cmdReports :: [FilePath]        -- ^ where to generate reports
        ,cmdWithHints :: [String]        -- ^ hints that are given on the command line
        ,cmdTempDir :: FilePath          -- ^ temporary directory to put the files in
        ,cmdQuickCheck :: Bool
        ,cmdTypeCheck :: Bool
        }
    | CmdHSE
        {cmdFiles :: [FilePath]
        ,cmdLanguage :: [String]      -- ^ the extensions (may be prefixed by "No")
        }
    deriving (Data,Typeable,Show)

mode = cmdArgsMode $ modes
    [CmdMain
        {cmdFiles = def &= args &= typ "FILE/DIR"
        ,cmdReports = nam "report" &= opt "report.html" &= typFile &= help "Generate a report in HTML"
        ,cmdGivenHints = nam "hint" &= typFile &= help "Hint/ignore file to use"
        ,cmdWithHints = nam "with" &= typ "HINT" &= help "Extra hints to use"
        ,cmdGit = nam "git" &= help "Run on files tracked by git"
        ,cmdColor = nam "colour" &= name "color" &= opt Always &= typ "always/never/auto" &= help "Color output (requires ANSI terminal; auto means on when $TERM is supported; by itself, selects always)"
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
        ,cmdSerialise = nam_ "serialise" &= help "Serialise hint data for consumption by apply-refact"
        ,cmdRefactor = nam_ "refactor" &= help "Automatically invoke `refactor` to apply hints"
        ,cmdRefactorOptions = nam_ "refactor-options" &= typ "OPTIONS" &= help "Options to pass to the `refactor` executable"
        , cmdWithRefactor = nam_ "with-refactor" &= help "Give the path to refactor"
        } &= auto &= explicit &= name "lint"
    ,CmdGrep
        {cmdFiles = def &= args &= typ "FILE/DIR"
        ,cmdPattern = def &= argPos 0 &= typ "PATTERN"
        } &= explicit &= name "grep"
    ,CmdTest
        {cmdProof = nam_ "proof" &= typFile &= help "Isabelle/HOLCF theory file"
        ,cmdTypeCheck = nam_ "typecheck" &= help "Use GHC to type check the hints"
        ,cmdQuickCheck = nam_ "quickcheck" &= help "Use QuickCheck to check the hints"
        ,cmdTempDir = nam_ "tempdir" &= help "Where to put temporary files (not cleaned up)"
        } &= explicit &= name "test"
        &= details ["HLint gives hints on how to improve Haskell code."
                   ,""
                   ,"To check all Haskell files in 'src' and generate a report type:"
                   ,"  hlint src --report"]
    ,CmdHSE
        {} &= explicit &= name "hse"
    ] &= program "hlint" &= verbosity
    &=  summary ("HLint v" ++ showVersion version ++ ", (C) Neil Mitchell 2006-2017")
    where
        nam xs = nam_ xs &= name [head xs]
        nam_ xs = def &= explicit &= name xs

-- | Where should we find the configuration files?
--   * If someone passes cmdWithHints, only look at files they explicitly request
--   * If someone passes an explicit hint name, automatically merge in data/hlint.yaml
--   We want more important hints to go last, since they override
cmdHintFiles :: Cmd -> IO [FilePath]
cmdHintFiles cmd = do
    let explicit1 = [cmdDataDir cmd </> "hlint.yaml" | null $ cmdWithHints cmd]
    let explicit2 = cmdGivenHints cmd
    bad <- filterM (notM . doesFileExist) $ explicit1 ++ explicit2
    when (bad /= []) $
        fail $ unlines $ "Failed to find requested hint files:" : map ("  "++) bad
    if cmdWithHints cmd /= [] then return $ explicit1 ++ explicit2 else do
        -- we follow the stylish-haskell config file search policy
        -- 1) current directory or its ancestors; 2) home directory
        curdir <- getCurrentDirectory
        home <- getHomeDirectory
        b <- doesFileExist ".hlint.yaml"
        implicit <- findM doesFileExist $
            "HLint.hs" : -- the default in HLint 1.*
            map (</> ".hlint.yaml") (ancestors curdir ++ [home]) -- to match Stylish Haskell
        return $ explicit1 ++ maybeToList implicit ++ explicit2
    where
        ancestors = init . map joinPath . reverse . inits . splitPath

cmdExtensions :: Cmd -> (Language, [Extension])
cmdExtensions = getExtensions . cmdLanguage


cmdCpp :: Cmd -> CppFlags
cmdCpp cmd
    | cmdCppSimple cmd = CppSimple
    | EnableExtension CPP `elem` snd (cmdExtensions cmd) = Cpphs defaultCpphsOptions
        {boolopts=defaultBoolOptions{hashline=False, stripC89=True, ansi=cmdCppAnsi cmd}
        ,includes = cmdCppInclude cmd
        ,preInclude = cmdCppFile cmd
        ,defines = [(a,drop 1 b) | x <- cmdCppDefine cmd, let (a,b) = break (== '=') x]
        }
    | otherwise = NoCpp


-- | Determines whether to use colour or not.
cmdUseColour :: Cmd -> IO Bool
cmdUseColour cmd = case cmdColor cmd of
  Always -> return True
  Never  -> return False
  Auto   -> hSupportsANSI stdout


"." <\> x = x
x <\> y = x </> y


resolveFile
    :: Cmd
    -> Maybe FilePath -- ^ Temporary file
    -> FilePath       -- ^ File to resolve, may be "-" for stdin
    -> IO [FilePath]
resolveFile cmd = getFile (cmdPath cmd) (cmdExtension cmd)


getFile :: [FilePath] -> [String] -> Maybe FilePath -> FilePath -> IO [FilePath]
getFile path _ (Just tmpfile) "-" =
  getContents >>= writeFile tmpfile >> return [tmpfile]
getFile path _ Nothing "-" = return ["-"]
getFile [] exts _ file = exitMessage $ "Couldn't find file: " ++ file
getFile (p:ath) exts t file = do
    isDir <- doesDirectoryExist $ p <\> file
    if isDir then do
        let avoidDir x = let y = takeFileName x in "_" `isPrefixOf` y || ("." `isPrefixOf` y && not (all (== '.') y))
            avoidFile x = let y = takeFileName x in "." `isPrefixOf` y
        xs <- listFilesInside (return . not . avoidDir) $ p <\> file
        return [x | x <- xs, drop 1 (takeExtension x) `elem` exts, not $ avoidFile x]
     else do
        isFil <- doesFileExist $ p <\> file
        if isFil then return [p <\> file]
         else do
            res <- getModule p exts file
            case res of
                Just x -> return [x]
                Nothing -> getFile ath exts t file


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


getExtensions :: [String] -> (Language, [Extension])
getExtensions args = (lang, foldl f (if null langs then defaultExtensions else []) exts)
    where
        lang = if null langs then baseLanguage defaultParseMode else fromJust $ lookup (last langs) ls
        (langs, exts) = partition (isJust . flip lookup ls) args
        ls = [(show x, x) | x <- knownLanguages]

        f a "Haskell98" = []
        f a ('N':'o':x) | Just x <- readExtension x = delete x a
        f a x | Just x <- readExtension x = x : delete x a
        f a x = exitMessageImpure $ "Unknown extension: " ++ x


readExtension :: String -> Maybe Extension
readExtension x = case classifyExtension x of
    UnknownExtension _ -> Nothing
    x -> Just x

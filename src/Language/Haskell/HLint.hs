{-# LANGUAGE PatternGuards, RecordWildCards #-}

-- |  This module provides a way to apply HLint hints. If you want to just run @hlint@ in-process
--   and collect the results see 'hlint'.
--
--   If you want to approximate the @hlint@ experience with
--   a more structured API try:
--
-- @
-- (flags, classify, hint) <- 'autoSettings'
-- Right m <- 'parseModuleEx' flags \"MyFile.hs\" Nothing
-- print $ 'applyHints' classify hint [m]
-- @
module Language.Haskell.HLint(
    -- * Generate hints
    hlint, applyHints,
    -- * Idea data type
    Idea(..), Severity(..), Note(..), unpackSrcSpan, showIdeaANSI,
    -- * Settings
    Classify(..),
    getHLintDataDir, autoSettings, argsSettings,
    findSettings, readSettingsFile,
    -- * Hints
    Hint,
    -- * Modules
    ModuleEx, parseModuleEx, createModuleEx, createModuleExWithFixities, ParseError(..),
    -- * Parse flags
    defaultParseFlags,
    ParseFlags(..), CppFlags(..), FixityInfo,
    parseFlagsAddFixities,
    ) where

import Config.Type
import Config.Read
import Idea
import qualified Apply as H
import HLint
import Fixity
import GHC.Data.FastString ( unpackFS )
import GHC.All
import Hint.All hiding (resolveHints)
import qualified Hint.All as H
import GHC.Types.SrcLoc
import CmdLine
import Paths_hlint

import Data.List.Extra
import Data.Maybe
import System.FilePath
import Data.Functor
import Prelude
import qualified Hint.Restrict as Restrict


-- | Get the Cabal configured data directory of HLint.
getHLintDataDir :: IO FilePath
getHLintDataDir = getDataDir


-- | The function produces a tuple containg 'ParseFlags' (for 'parseModuleEx'),
--   and 'Classify' and 'Hint' for 'applyHints'.
--   It approximates the normal HLint configuration steps, roughly:
--
-- 1. Use 'findSettings' with 'readSettingsFile' to find and load the HLint settings files.
--
-- 1. Use 'parseFlagsAddFixities' and 'resolveHints' to transform the outputs of 'findSettings'.
--
--   If you want to do anything custom (e.g. using a different data directory, storing intermediate outputs,
--   loading hints from a database) you are expected to copy and paste this function, then change it to your needs.
autoSettings :: IO (ParseFlags, [Classify], Hint)
autoSettings = do
    (fixities, classify, hints) <- findSettings (readSettingsFile Nothing) Nothing
    pure (parseFlagsAddFixities fixities defaultParseFlags, classify, hints)


-- | A version of 'autoSettings' which respects some of the arguments supported by HLint.
--   If arguments unrecognised by HLint are used it will result in an error.
--   Arguments which have no representation in the return type are silently ignored.
argsSettings :: [String] -> IO (ParseFlags, [Classify], Hint)
argsSettings args = do
    cmd@CmdMain{..} <- getCmd args
    -- FIXME: One thing that could be supported (but isn't) is 'cmdGivenHints'
    (_,settings) <- readAllSettings args cmd
    let (fixities, classify, hints) = splitSettings settings
    let flags = parseFlagsSetLanguage (cmdExtensions cmd) $ parseFlagsAddFixities fixities $
                defaultParseFlags{cppFlags = cmdCpp cmd}
    let ignore = [Classify Ignore x "" "" | x <- cmdIgnore]
    pure (flags, classify ++ ignore, hints)


-- | Given a directory (or 'Nothing' to imply 'getHLintDataDir'), and a module name
--   (e.g. @HLint.Default@), find the settings file associated with it, returning the
--   name of the file, and (optionally) the contents.
--
--   This function looks for all settings files starting with @HLint.@ in the directory
--   argument, and all other files relative to the current directory.
readSettingsFile :: Maybe FilePath -> String -> IO (FilePath, Maybe String)
readSettingsFile dir x
    | takeExtension x `elem` [".yml",".yaml"] = do
        dir <- maybe getHLintDataDir pure dir
        pure (dir </> x, Nothing)
    | Just x <- "HLint." `stripPrefix` x = do
        dir <- maybe getHLintDataDir pure dir
        pure (dir </> x <.> "hs", Nothing)
    | otherwise = pure (x <.> "hs", Nothing)


-- | Given a function to load a module (typically 'readSettingsFile'), and a module to start from
--   (defaults to @hlint.yaml@) find the information from all settings files.
findSettings :: (String -> IO (FilePath, Maybe String)) -> Maybe String -> IO ([FixityInfo], [Classify], Hint)
findSettings load start = do
    (file,contents) <- load $ fromMaybe "hlint.yaml" start
    splitSettings <$> readFilesConfig [(file,contents)]

-- | Split a list of 'Setting' for separate use in parsing and hint resolution
splitSettings :: [Setting] -> ([FixityInfo], [Classify], Hint)
splitSettings xs =
    ([x | Infix x <- xs]
    ,[x | SettingClassify x <- xs]
    ,H.resolveHints ([Right x | SettingMatchExp x <- xs] ++ map Left enumerate)
    <> mempty { hintModule = Restrict.restrictHint . (xs++)}
    )


-- | Given a way of classifying results, and a 'Hint', apply to a set of modules generating a list of 'Idea's.
--   The 'Idea' values will be ordered within a file.
--
--   Given a set of modules, it may be faster to pass each to 'applyHints' in a singleton list.
--   When given multiple modules at once this function attempts to find hints between modules,
--   which is slower and often pointless (by default HLint passes modules singularly, using
--   @--cross@ to pass all modules together).
applyHints :: [Classify] -> Hint -> [ModuleEx] -> [Idea]
applyHints = H.applyHints

-- | Snippet from the documentation, if this changes, update the documentation
_docs :: IO ()
_docs = do
    (flags, classify, hint) <- autoSettings
    Right m <- parseModuleEx flags "MyFile.hs" Nothing
    print $ applyHints classify hint [m]

-- | Unpack a 'SrcSpan' value. Useful to allow using the 'Idea' information without
--   adding a dependency on @ghc@ or @ghc-lib-parser@. Unpacking gives:
--
-- > (filename, (startLine, startCol), (endLine, endCol))
--
--   Following the GHC API, he end column is the column /after/ the end of the error.
--   Lines and columns are 1-based. Returns 'Nothing' if there is no helpful location information.
unpackSrcSpan :: SrcSpan -> Maybe (FilePath, (Int, Int), (Int, Int))
unpackSrcSpan (RealSrcSpan x _) = Just
    (unpackFS $ srcSpanFile x
    ,(srcSpanStartLine x, srcSpanStartCol x)
    ,(srcSpanEndLine x, srcSpanEndCol x))
unpackSrcSpan _ = Nothing


-- | /WARNING: This module represents the evolving second version of the HLint API./
--   /It will be deleted in favour of "Language.Haskell.HLint3" in the next major version./
--
--   This module provides a way to apply HLint hints. As an example of approximating the @hlint@ experience:
--
-- @
-- (flags, classify, hint) <- 'autoSettings'
-- Right m <- 'parseModuleEx' flags \"MyFile.hs\" Nothing
-- print $ 'applyHints' classify hint [m]
-- @
module Language.Haskell.HLint2(
    applyHints,
    -- * Idea data type
    Idea(..), Severity(..), Note(..),
    -- * Settings
    Classify(..),
    getHLintDataDir,
    autoSettings, autoSettings', findSettings, readSettings,
    -- * Hints
    Hint(..), builtinHints,
    HintRule(..), hintRules,
    -- * Scopes
    Scope, scopeCreate, scopeMatch, scopeMove,
    -- * Haskell-src-exts
    parseModuleEx, defaultParseFlags, ParseError(..), ParseFlags(..), CppFlags(..),
    -- * File encodings
    Encoding, defaultEncoding, readEncoding, useEncoding
    ) where

import Settings
import Idea
import Apply
import Hint.Type
import Hint.All
import Util
import CmdLine
import Paths_hlint

import Control.Applicative
import Data.Tuple.Extra
import Data.List.Extra
import Data.Monoid
import System.FilePath


-- | Get the Cabal configured data directory of HLint
getHLintDataDir :: IO FilePath
getHLintDataDir = getDataDir

-- | The function produces a tuple containg 'ParseFlags' (for 'parseModuleEx'), and 'Classify' and 'Hint' for 'applyHints'.
--   It approximates the normal HLint configuration steps, roughly:
--
-- 1. Use 'findSettings' to find and load the HLint settings files.
--
-- 1. Use 'readSettings' to interpret the settings files, producing 'HintRule' values (@LHS ==> RHS@ replacements)
--   and 'Classify' values to assign 'Severity' ratings to hints.
--
-- 1. Use 'builtinHints' and 'hintRules' to generate a 'Hint' value.
--
-- 1. Take all fixities from the 'findSettings' modules and put them in the 'ParseFlags'.
autoSettings :: IO (ParseFlags, [Classify], Hint)
autoSettings = getHLintDataDir >>= autoSettings'

autoSettings' :: FilePath -> IO (ParseFlags, [Classify], Hint)
autoSettings' dataDir = do
    (builtin, matches) <- first resolveBuiltin <$> findSettings dataDir (dataDir </> "HLint.hs") Nothing
    let (classify, rules) = second hintRules $ concatUnzip $ map readSettings matches
    let fixities = getFixity =<< moduleDecls =<< matches
    return (parseFlagsAddFixities fixities defaultParseFlags, classify, mconcat $ rules : builtin)


-- | Snippet from the documentation, if this changes, update the documentation
_docs :: IO ()
_docs = do
    (flags, classify, hint) <- autoSettings
    Right m <- parseModuleEx flags "MyFile.hs" Nothing
    print $ applyHints classify hint [m]

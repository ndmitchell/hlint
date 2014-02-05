
-- | /WARNING: This module represents the evolving API of HLint, do not use./
--
--   This module provides a way to apply HLint hints. To replicate the full @hlint@ experience you would:
--
-- 1. Use 'findSettings' to find and load the HLint settings files.
--
-- 1. Use 'readSettings' to interpret the settings files, producing 'HintRule' values (@LHS ==> RHS@ replacements)
--   and 'Classify' values to assign 'Severity' ratings to hints.
--
-- 1. Use 'builtinHints' and 'hintRules' to generate a 'Hint' value.
--
-- 1. Use 'parseModuleEx' to parse the input files, using any fixity declarations from 'findSettings'.
--
-- 1. Use 'applyHints' to execute the hints on the modules, generating 'Idea's.
module Temporary.API(
    applyHints,
    -- * Idea data type
    Idea(..), Severity(..), Note(..),
    -- * Settings
    Classify(..),
    getHLintDataDir,
    autoSettings, findSettings, readSettings,
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
import Control.Arrow
import Data.List
import Data.Maybe
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
autoSettings = do
    dataDir <- getHLintDataDir
    (builtin, matches) <- first resolveBuiltin <$> findSettings dataDir (dataDir </> "HLint.hs") Nothing
    let (classify, rules) = second hintRules $ concat2 $ map readSettings matches
    let fixities = getFixity =<< moduleDecls =<< matches
    return (parseFlagsAddFixities fixities defaultParseFlags, classify, mconcat $ rules : builtin)

resolveBuiltin :: [String] -> [Hint]
resolveBuiltin builtin = map f $ nub $ concat [if x == "All" then map fst builtinHints else [x] | x <- builtin]
    where f x = fromMaybe (error $ "Unknown builtin hints: HLint.Builtin." ++ x) $ lookup x builtinHints


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
    findSettings, readSettings,
    -- * Hints
    Hint(..), builtinHints,
    HintRule(..), hintRules,
    -- * Scopes
    Scope, scopeCreate, scopeMatch, scopeMove,
    -- * Haskell-src-exts
    parseModuleEx, ParseError(..), ParseFlags(..), CppFlags(..), defaultParseFlags,
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

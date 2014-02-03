
-- | /WARNING: This module represents the evolving API of HLint, do not use./
module Temporary.API(
    -- * Entry point
    applyHints,
    Idea(..), Note(..), Severity(..),
    -- * Reading settings
    Classify(..),
    findHintModules, moduleSettings,
    -- * Hint.All
    hintRules, HintRule(..),
    -- * Hints
    builtinHints, Hint(..),
    -- * Scopes
    Scope, scopeCreate, scopeMatch, scopeMove,
    -- * HSE
    ParseError(..), ParseFlags(..), CppFlags(..), defaultParseFlags, parseModuleEx,
    -- * File encodings
    Encoding, defaultEncoding, newEncoding, useEncoding
    ) where

import Settings
import Idea
import Apply
import Hint.Type
import Hint.All
import Util
import CmdLine

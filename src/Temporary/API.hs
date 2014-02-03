
-- | /WARNING: This module represents the evolving API of HLint, do not use./
module Temporary.API(
    applyHints,
    -- * Idea data type
    Idea(..), Severity(..), Note(..),
    -- * Setting
    Classify(..),
    findHintModules, moduleSettings,
    -- * Hints
    Hint(..), builtinHints,
    HintRule(..), hintRules,
    -- * Scopes
    Scope, scopeCreate, scopeMatch, scopeMove,
    -- * Haskell-src-exts
    parseModuleEx, ParseError(..), ParseFlags(..), CppFlags(..), defaultParseFlags,
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

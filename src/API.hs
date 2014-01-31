
-- | This module represents the evolving API of HLint, it is not yet stable, complete or usable
module API(
    -- * Settings
    module Settings,
    -- * Idea
    module Idea,
    -- * Apply
    module Apply,
    -- * Hint.All
    dynamicHints,
    -- * Hints
    builtinHints, Hint(..),
    -- * Scopes
    Scope, scopeCreate, scopeMatch, scopeMove,
    -- * HSE
    ParseFlags(..), CppFlags(..), defaultParseFlags, parseModuleEx,
    -- * File encodings
    Encoding, defaultEncoding, newEncoding, readFileEncoding
    ) where

import Settings
import Idea
import Apply
import Hint.Type
import Hint.All
import Util
import CmdLine


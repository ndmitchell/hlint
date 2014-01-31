
-- | This module represents the evolving API of HLint, it is not yet stable, complete or usable
module API(
    -- * Settings
    module Settings,
    -- * Idea
    module Idea,
    -- * Apply
    module Apply,
    -- * Hint.Type
    Hint(..),
    -- * Hint.All
    builtinHints, dynamicHints,
    -- * HSE.Scope
    Scope, scopeCreate, scopeMatch, scopeMove,
    -- * HSE
    Module_, Decl_, Exp_, S,
    ParseFlags(..), defaultParseFlags, parseModuleEx,
    -- * File encodings
    Encoding, defaultEncoding, newEncoding, readFileEncoding
    ) where

import Settings
import Idea
import Apply
import Hint.Type hiding (Module_, Exp_, S)
import Hint.All
import HSE.All
import Util


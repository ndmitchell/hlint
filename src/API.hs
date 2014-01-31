
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
    staticHints, dynamicHints,
    -- * HSE.Scope
    Scope, scopeCreate, scopeMatch, scopeMove,
    -- * HSE
    Module_, Exp_, S
    ) where

import Settings
import Idea
import Apply
import Hint.Type hiding (Module_, Exp_, S)
import Hint.All
import HSE.All


{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- for GHC 7.9 compatibility

-- | Alternative Prelude which exports more things, so we can be
--   warning-compatible with GHC 7.10.
module PreludeEx(
    module Prelude,
    Monoid(..), Applicative(..)
    ) where

import Data.Monoid
import Control.Applicative

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Util.SrcLoc (
    stripLocs'
  , SrcSpanD(..)
  ) where

import SrcLoc
import Outputable

import Data.Default
import Data.Data
import Data.Generics.Uniplate.Data

-- 'stripLocs x' is 'x' with all contained source locs replaced by
-- 'noSrcSpan'.
stripLocs' :: (Data from, HasSrcSpan from) => from -> from
stripLocs' = transformBi (const noSrcSpan)

-- 'Duplicates.hs' requires 'SrcSpan' be in 'Default'.
newtype SrcSpanD = SrcSpanD SrcSpan
  deriving (Outputable, Eq, Ord)
instance Default SrcSpanD where def = SrcSpanD noSrcSpan

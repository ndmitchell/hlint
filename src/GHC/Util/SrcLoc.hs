{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Util.SrcLoc (
    getAncLoc
  , stripLocs
  , SrcSpanD(..)
  ) where

import GHC.Parser.Annotation
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Data.FastString
import qualified GHC.Data.Strict

import Data.Default
import Data.Data
import Data.Generics.Uniplate.DataOnly

-- Get the 'SrcSpan' out of a value located by an 'Anchor' (e.g.
-- comments).
getAncLoc :: GenLocated Anchor a -> SrcSpan
getAncLoc o = RealSrcSpan (anchor (getLoc o)) GHC.Data.Strict.Nothing

-- 'stripLocs x' is 'x' with all contained source locs replaced by
-- 'noSrcSpan'.
stripLocs :: Data from => from -> from
stripLocs =
  transformBi (const dummySpan) . transformBi (const noSrcSpan)
  where
    dummyLoc = mkRealSrcLoc (fsLit "dummy") 1 1
    dummySpan = mkRealSrcSpan dummyLoc dummyLoc

-- TODO (2020-10-03, SF): Maybe move the following definitions down to
-- ghc-lib-parser at some point.

-- 'Duplicates.hs' requires 'SrcSpan' be in 'Default' and 'Ord'.
newtype SrcSpanD = SrcSpanD SrcSpan
  deriving (Outputable, Eq)
instance Default SrcSpanD where def = SrcSpanD noSrcSpan

newtype FastStringD = FastStringD FastString
  deriving Eq
compareFastStrings (FastStringD f) (FastStringD g) =
  lexicalCompareFS f g
instance Ord FastStringD where compare = compareFastStrings

-- SrcSpan no longer provides 'Ord' so we are forced to roll our own.
--
-- Note: This implementation chooses that any span compares 'EQ to an
-- 'UnhelpfulSpan'. Ex falso quodlibet!
compareSrcSpans (SrcSpanD a) (SrcSpanD b) =
  case a of
    RealSrcSpan a1 _ ->
      case b of
        RealSrcSpan b1 _ ->
          a1 `compareRealSrcSpans` b1
        _ -> EQ -- error "'Duplicate.hs' invariant error: can't compare unhelpful spans"
    _ -> EQ -- error "'Duplicate.hs' invariant error: can't compare unhelpful spans"
compareRealSrcSpans a b =
  let (a1, a2, a3, a4, a5) = (LexicalFastString (srcSpanFile a), srcSpanStartLine a, srcSpanStartCol a, srcSpanEndLine a, srcSpanEndCol a)
      (b1, b2, b3, b4, b5) = (LexicalFastString (srcSpanFile b), srcSpanStartLine b, srcSpanStartCol b, srcSpanEndLine b, srcSpanEndCol b)
  in compare (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
instance Ord SrcSpanD where compare = compareSrcSpans

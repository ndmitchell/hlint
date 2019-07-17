{-# LANGUAGE PackageImports #-}
module Refact
    ( toRefactSrcSpan
    , toSS
    , toSrcSpan'
    ) where

import qualified Refact.Types as R
import HSE.All

import qualified "ghc-lib-parser" SrcLoc as Hs

toRefactSrcSpan :: SrcSpan -> R.SrcSpan
toRefactSrcSpan ss = R.SrcSpan (srcSpanStartLine ss)
                               (srcSpanStartColumn ss)
                               (srcSpanEndLine ss)
                               (srcSpanEndColumn ss)

toSS :: Annotated a => a S -> R.SrcSpan
toSS = toRefactSrcSpan . srcInfoSpan . ann

-- | Don't crash in case ghc gives us a \"fake\" span,
-- opting instead to show @0 0 0 0@ coordinates.
toSrcSpan' :: Hs.HasSrcSpan a => a -> R.SrcSpan
toSrcSpan' x = case Hs.getLoc x of
    Hs.RealSrcSpan span ->
        R.SrcSpan (Hs.srcSpanStartLine span)
                  (Hs.srcSpanStartCol span)
                  (Hs.srcSpanEndLine span)
                  (Hs.srcSpanEndCol span)
    Hs.UnhelpfulSpan _ ->
        R.SrcSpan 0 0 0 0

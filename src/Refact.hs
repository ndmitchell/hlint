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

toSrcSpan' :: Hs.HasSrcSpan a => a -> R.SrcSpan
toSrcSpan' x =
    R.SrcSpan (Hs.srcSpanStartLine span)
              (Hs.srcSpanStartCol span)
              (Hs.srcSpanEndLine span)
              (Hs.srcSpanEndCol span)
    where
        span :: Hs.RealSrcSpan
        span = case Hs.getLoc x of
                 Hs.RealSrcSpan s -> s
                 _ -> error "Got a bad ghc SrcSpan! Report a bug to hlint please!"

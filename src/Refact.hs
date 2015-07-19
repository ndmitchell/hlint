module Refact where

import qualified Refact.Types as R
import HSE.All

toRefactSrcSpan :: SrcSpan -> R.SrcSpan
toRefactSrcSpan ss = R.SrcSpan (srcSpanStart ss) (srcSpanEnd ss)

toSS :: Annotated a => a S -> R.SrcSpan
toSS = toRefactSrcSpan . toSrcSpan . ann



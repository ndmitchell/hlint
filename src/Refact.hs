module Refact where


import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R
import HSE.All

-- Utility functions for creating refactorings
refactReplace :: Annotated a => RType -> a S -> [(String, S)] -> String -> Refactoring R.SrcSpan
refactReplace typ ss subt template =
  Replace typ (toSS ss) (map (fmap f) subt) template
  where
    f = toRefactSrcSpan . toSrcSpan

toRefactSrcSpan :: SrcSpan -> R.SrcSpan
toRefactSrcSpan ss = R.SrcSpan (srcSpanStart ss) (srcSpanEnd ss)

toSS :: Annotated a => a S -> R.SrcSpan
toSS = toRefactSrcSpan . toSrcSpan . ann



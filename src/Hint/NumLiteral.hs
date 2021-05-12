{-
    Suggest the usage of underscore when NumericUnderscores is enabled.

<TEST>
123456
{-# LANGUAGE NumericUnderscores #-} \
12345 -- @Suggestion 12_345 @NoRefactor
{-# LANGUAGE NumericUnderscores #-} \
123456789.0441234e-123456 -- @Suggestion 123_456_789.044_123_4e-123_456 @NoRefactor
{-# LANGUAGE NumericUnderscores #-} \
0x12abc.523defp+172345 -- @Suggestion 0x1_2abc.523d_efp+172_345 @NoRefactor
{-# LANGUAGE NumericUnderscores #-} \
3.14159265359 -- @Suggestion 3.141_592_653_59 @NoRefactor
{-# LANGUAGE NumericUnderscores #-} \
12_33574_56
</TEST>

-}

module Hint.NumLiteral (numLiteralHint) where

import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Util.ApiAnnotation (extensions)
import Data.Char (isDigit, isOctDigit, isHexDigit)
import Data.List (intercalate)
import Data.Generics.Uniplate.DataOnly (universeBi)
import Refact.Types

import Hint.Type (DeclHint, toSSA, modComments)
import Idea (Idea, suggest)

numLiteralHint :: DeclHint
numLiteralHint _ modu =
  if NumericUnderscores `elem` extensions (modComments modu) then
     concatMap suggestUnderscore . universeBi
  else
     const []

suggestUnderscore :: LHsExpr GhcPs -> [Idea]
suggestUnderscore x@(L _ (HsOverLit _ ol@(OverLit _ (HsIntegral intLit@(IL (SourceText srcTxt) _ _)) _))) =
  [ suggest "Use underscore" (reLoc x) (reLoc y) [r] | '_' `notElem` srcTxt, srcTxt /= underscoredSrcTxt ]
  where
    underscoredSrcTxt = addUnderscore srcTxt
    y = noLocA $ HsOverLit EpAnnNotUsed $ ol{ol_val = HsIntegral intLit{il_text = SourceText underscoredSrcTxt}}
    r = Replace Expr (toSSA x) [("a", toSSA y)] "a"
suggestUnderscore x@(L _ (HsOverLit _ ol@(OverLit _ (HsFractional fracLit@(FL (SourceText srcTxt) _ _ _ _)) _))) =
  [ suggest "Use underscore" (reLoc x) (reLoc y) [r] | '_' `notElem` srcTxt, srcTxt /= underscoredSrcTxt ]
  where
    underscoredSrcTxt = addUnderscore srcTxt
    y = noLocA $ HsOverLit EpAnnNotUsed $ ol{ol_val = HsFractional fracLit{fl_text = SourceText underscoredSrcTxt}}
    r = Replace Expr (toSSA x) [("a", toSSA y)] "a"
suggestUnderscore _ = mempty

addUnderscore :: String -> String
addUnderscore intStr = numLitToStr underscoredNumLit
 where
   numLit = toNumLiteral intStr
   underscoredNumLit = numLit{ nl_intPart = underscoreFromRight chunkSize $ nl_intPart numLit
                             , nl_fracPart = underscore chunkSize $ nl_fracPart numLit
                             , nl_exp = underscoreFromRight 3 $ nl_exp numLit -- Exponential part is always decimal
                             }
   chunkSize = if null (nl_prefix numLit) then 3 else 4

   underscore chunkSize = intercalate "_" . chunk chunkSize
   underscoreFromRight chunkSize = reverse . underscore chunkSize . reverse
   chunk chunkSize [] = []
   chunk chunkSize xs = a:chunk chunkSize b where (a, b) = splitAt chunkSize xs

data NumLiteral = NumLiteral
  { nl_prefix :: String
  , nl_intPart :: String
  , nl_decSep :: String -- decimal separator
  , nl_fracPart :: String
  , nl_expSep :: String -- e, e+, e-, p, p+, p-
  , nl_exp :: String
  } deriving (Show, Eq)

toNumLiteral :: String -> NumLiteral
toNumLiteral str = case str of
  '0':'b':digits -> (afterPrefix isBinDigit digits){nl_prefix = "0b"}
  '0':'B':digits -> (afterPrefix isBinDigit digits){nl_prefix = "0B"}
  '0':'o':digits -> (afterPrefix isOctDigit digits){nl_prefix = "0o"}
  '0':'O':digits -> (afterPrefix isOctDigit digits){nl_prefix = "0O"}
  '0':'x':digits -> (afterPrefix isHexDigit digits){nl_prefix = "0x"}
  '0':'X':digits -> (afterPrefix isHexDigit digits){nl_prefix = "0X"}
  _              -> afterPrefix isDigit str
  where
    isBinDigit x = x == '0' || x == '1'

    afterPrefix isDigit str = (afterIntPart isDigit suffix){nl_intPart = intPart}
      where (intPart, suffix) = span isDigit str

    afterIntPart isDigit ('.':suffix) = (afterDecSep isDigit suffix){nl_decSep = "."}
    afterIntPart isDigit str = afterFracPart str

    afterDecSep isDigit str = (afterFracPart suffix){nl_fracPart = fracPart}
      where (fracPart, suffix) = span isDigit str

    afterFracPart str = NumLiteral "" "" "" "" expSep exp
      where (expSep, exp) = break isDigit str

numLitToStr :: NumLiteral -> String
numLitToStr (NumLiteral p ip ds fp es e) = p ++ ip ++ ds ++ fp ++ es ++ e

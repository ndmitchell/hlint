{-# LANGUAGE PackageImports #-}

module GHC.Util.ApiAnnotation (
    comment, commentText, isCommentMultiline
  , pragmas, flags, langExts
  , mkFlags, mkLangExts
) where

import "ghc-lib-parser" ApiAnnotation
import "ghc-lib-parser" SrcLoc

import Control.Applicative
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List.Extra

trimCommentStart :: String -> String
trimCommentStart s
    | Just s <- stripPrefix "{-" s = s
    | Just s <- stripPrefix "--" s = s
    | otherwise = s

trimCommentEnd :: String -> String
trimCommentEnd s
    | Just s <- stripSuffix "-}" s = s
    | otherwise = s

trimCommentDelims :: String -> String
trimCommentDelims = trimCommentEnd . trimCommentStart

-- | A comment as a string.
comment :: Located AnnotationComment -> String
comment (L _ (AnnBlockComment s)) = s
comment (L _ (AnnLineComment s)) = s
comment (L _ (AnnDocOptions s)) = s
comment (L _ (AnnDocCommentNamed s)) = s
comment (L _ (AnnDocCommentPrev s)) = s
comment (L _ (AnnDocCommentNext s)) = s
comment (L _ (AnnDocSection _ s)) = s

-- | The comment string with delimiters removed.
commentText :: Located AnnotationComment -> String
commentText = trimCommentDelims . comment

isCommentMultiline :: Located AnnotationComment -> Bool
isCommentMultiline (L _ (AnnBlockComment _)) = True
isCommentMultiline _ = False

-- GHC parse trees don't contain pragmas. We work around this with
-- (nasty) parsing of comments.

-- Pragmas. Comments not associated with a span in the annotations
-- that have the form @{-# ...#-}@.
pragmas :: ApiAnns -> [(Located AnnotationComment, String)]
pragmas anns =
  -- 'ApiAnns' stores pragmas in reverse order to how they were
  -- encountered in the source file with the last at the head of the
  -- list (makes sense when you think about it).
  reverse
    [ (c, s) |
        c@(L _ (AnnBlockComment comm)) <- fromMaybe [] $ Map.lookup noSrcSpan (snd anns)
      , let body = trimCommentDelims comm
      , Just rest <- [stripSuffix "#" =<< stripPrefix "#" body]
      , let s = trim rest
    ]

-- Utility for a case insensitive prefix strip.
stripPrefixCI :: String -> String -> Maybe String
stripPrefixCI pref str =
  let pref' = lower pref
      (str_pref, rest) = splitAt (length pref') str
  in if lower str_pref == pref' then Just rest else Nothing

-- Flags. The first element of the pair is the (located) annotation
-- comment that sets the flags enumerated in the second element of the
-- pair.
flags :: [(Located AnnotationComment, String)]
      -> [(Located AnnotationComment, [String])]
flags ps =
  -- Old versions of GHC accepted 'OPTIONS' rather than 'OPTIONS_GHC' (but
  -- this is deprecated).
  [(c, opts) | (c, s) <- ps
             , Just rest <- [stripPrefixCI "OPTIONS_GHC " s
                             <|> stripPrefixCI "OPTIONS " s]
             , let opts = words rest]

-- Language extensions. The first element of the pair is the (located)
-- annotation comment that enables the extensions enumerated by he
-- second element of the pair.
langExts :: [(Located AnnotationComment, String)]
         -> [(Located AnnotationComment, [String])]
langExts ps =
  [(c, exts) | (c, s) <- ps
             , Just rest <- [stripPrefixCI "LANGUAGE " s]
             , let exts = map trim (splitOn "," rest)]

-- Given a list of flags, make a GHC options pragma.
mkFlags :: SrcSpan -> [String] -> Located AnnotationComment
mkFlags loc flags =
  LL loc $ AnnBlockComment ("{-# " ++ "OPTIONS_GHC " ++ unwords flags ++ " #-}")

mkLangExts :: SrcSpan -> [String] -> Located AnnotationComment
mkLangExts loc exts =
  LL loc $ AnnBlockComment ("{-# " ++ "LANGUAGE " ++ intercalate ", " exts ++ " #-}")

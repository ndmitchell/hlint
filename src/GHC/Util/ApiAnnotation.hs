
module GHC.Util.ApiAnnotation (
    comment_, commentText, isCommentMultiline
  , pragmas, flags, languagePragmas
  , mkFlags, mkLanguagePragmas
  , extensions
) where

import GHC.LanguageExtensions.Type (Extension)
import GHC.Parser.Annotation
import GHC.Hs.DocString
import GHC.Types.SrcLoc

import Language.Haskell.GhclibParserEx.GHC.Driver.Session

import Control.Applicative
import Data.List.Extra
import Data.Maybe
import qualified Data.Set as Set

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
comment_ :: LEpaComment -> String
comment_ (L _ (EpaComment (EpaDocComment ds ) _)) = renderHsDocString ds
comment_ (L _ (EpaComment (EpaDocOptions s) _)) = s
comment_ (L _ (EpaComment (EpaLineComment s) _)) = s
comment_ (L _ (EpaComment (EpaBlockComment s) _)) = s
comment_ (L _ (EpaComment EpaEofComment _)) = ""

-- | The comment string with delimiters removed.
commentText :: LEpaComment -> String
commentText = trimCommentDelims . comment_

isCommentMultiline :: LEpaComment -> Bool
isCommentMultiline (L _ (EpaComment (EpaBlockComment _) _)) = True
isCommentMultiline _ = False

-- Pragmas have the form @{-# ...#-}@.
pragmas :: EpAnnComments -> [(LEpaComment, String)]
pragmas x =
  -- 'EpaAnnComments' stores pragmas in reverse order to how they were
  -- encountered in the source file with the last at the head of the
  -- list (makes sense when you think about it).
  reverse
    [ (c, s) |
        c@(L _ (EpaComment (EpaBlockComment comm) _)) <- priorComments x
      , let body = trimCommentDelims comm
      , Just rest <- [stripSuffix "#" =<< stripPrefix "#" body]
      , let s = trim rest
    ]

-- All the extensions defined to be used.
extensions :: EpAnnComments -> Set.Set Extension
extensions = Set.fromList . mapMaybe readExtension .
    concatMap snd . languagePragmas . pragmas

-- Utility for a case insensitive prefix strip.
stripPrefixCI :: String -> String -> Maybe String
stripPrefixCI pref str =
  let pref' = lower pref
      (str_pref, rest) = splitAt (length pref') str
  in if lower str_pref == pref' then Just rest else Nothing

-- Flags. The first element of the pair is the comment that
-- sets the flags enumerated in the second element of the pair.
flags :: [(LEpaComment, String)] -> [(LEpaComment, [String])]
flags ps =
  -- Old versions of GHC accepted 'OPTIONS' rather than 'OPTIONS_GHC' (but
  -- this is deprecated).
  [(c, opts) | (c, s) <- ps
             , Just rest <- [stripPrefixCI "OPTIONS_GHC " s
                             <|> stripPrefixCI "OPTIONS " s]
             , let opts = words rest]

-- Language pragmas. The first element of the
-- pair is the (located) annotation comment that enables the
-- pragmas enumerated by he second element of the pair.
languagePragmas :: [(LEpaComment, String)] -> [(LEpaComment, [String])]
languagePragmas ps =
  [(c, exts) | (c, s) <- ps
             , Just rest <- [stripPrefixCI "LANGUAGE " s]
             , let exts = map trim (splitOn "," rest)]

-- Given a list of flags, make a GHC options pragma.
mkFlags :: Anchor -> [String] -> LEpaComment
mkFlags anc flags =
  L anc $ EpaComment (EpaBlockComment ("{-# " ++ "OPTIONS_GHC " ++ unwords flags ++ " #-}")) (anchor anc)

mkLanguagePragmas :: Anchor -> [String] -> LEpaComment
mkLanguagePragmas anc exts =
  L anc $ EpaComment (EpaBlockComment ("{-# " ++ "LANGUAGE " ++ intercalate ", " exts ++ " #-}")) (anchor anc)

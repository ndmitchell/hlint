{-# LANGUAGE ImportQualifiedPost #-}

{-
<TEST>
{- MISSING HASH #-} -- {-# MISSING HASH #-}
<COMMENT> {- INLINE X -}
{- INLINE Y -} -- {-# INLINE Y #-}
{- INLINE[~k] f -} -- {-# INLINE[~k] f #-}
{- NOINLINE Y -} -- {-# NOINLINE Y #-}
{- UNKNOWN Y -}
<COMMENT> INLINE X
</TEST>
-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Hint.Comment(commentHint) where

import Hint.Type
import Data.Char
import Data.List.Extra
import Refact.Types(Refactoring(ModifyComment))
import Refact.Types qualified as R (SrcSpan)
import GHC.Types.SrcLoc
import GHC.Parser.Annotation
import GHC.Util
import GHC.Data.Strict qualified

directives :: [String]
directives = words $
    "LANGUAGE OPTIONS_GHC INCLUDE WARNING DEPRECATED MINIMAL INLINE NOINLINE INLINABLE " ++
    "CONLIKE LINE SPECIALIZE SPECIALISE UNPACK NOUNPACK SOURCE"


commentHint :: ModuHint
commentHint _ m = concatMap (check singleLines someLines) comments
  where
    comments = ghcComments m
    singleLines = sort $ commentLine <$> filter isSingle comments
    someLines = sort $ commentLine <$> filter isSingleSome comments

-- | Does the commment start with "--"? Can be empty. Excludes haddock single
-- line comments, "-- |" and "-- ^".
isSingle :: LEpaComment -> Bool
isSingle comm@(L (anchor -> span) _) =
  isOneLineRealSpan span
  && not (isPointRealSpan span)
  && not (isCommentMultiline comm || isHaddock comm)

-- | A single line comment about something where something is:
-- * Not a haddock comment "-- |" or "-- ^"
-- * Not a multi-line comment "{- ... -}"
-- * Not a whitespace comment "--       "
isSingleSome :: LEpaComment -> Bool
isSingleSome comm@(L (anchor -> span) _) =
  isOneLineSpan (RealSrcSpan span GHC.Data.Strict.Nothing)
  && not (isPointRealSpan span)
  && not (isCommentMultiline comm || isHaddock comm || isCommentWhitespace comm)

-- | The start line number of a comment.
commentLine :: LEpaComment -> Int
commentLine (L (anchor -> span) _) = srcLocLine $ realSrcSpanStart span

-- | Is the next line in a string of empty comments leading up to a non-emtpy
-- comment?
nextLineIsComment :: Int -> [Int] -> [Int] -> Bool
nextLineIsComment x singles somes = Just True == do
  next <- find (x <) somes
  pure $ [x + 1 .. next] `isInfixOf` singles

check :: [Int] -> [Int] -> LEpaComment -> [Idea]
check singles somes comm@(L{})
  | isHaddockWhitespace comm =
      if | isMultiline -> [emptyHaddockMulti comm]
         | nextLineIsComment (commentLine comm) singles somes -> []
         | otherwise -> [emptyHaddockSingle comm]
  | isCommentWhitespace comm =
      if | isMultiline -> [emptyCommentMulti comm]
         | not $ nextLineIsComment (commentLine comm) singles somes -> [emptyCommentSingle comm]
         | otherwise -> []
  | isMultiline, null (commentText comm) = [emptyCommentMulti comm]
  | isMultiline, "#" `isSuffixOf` s && not ("#" `isPrefixOf` s) = [grab "Fix pragma markup" comm $ '#':s]
  | isMultiline, name `elem` directives = [grab "Use pragma syntax" comm $ "# " ++ trim s ++ " #"]
    where
      isMultiline = isCommentMultiline comm
      s = commentText comm
      name = takeWhile (\x -> isAlphaNum x || x == '_') $ trimStart s
check _ _ _ = []

isHaddockWhitespace :: LEpaComment -> Bool
isHaddockWhitespace comm = isHaddock comm && isStringWhitespace (drop 2 $ commentText comm)

isHaddock :: LEpaComment -> Bool
isHaddock (take 2 . commentText -> s) = " |" == s || " ^" == s

isStringWhitespace :: String -> Bool
isStringWhitespace = not . any (`notElem` " \t\r\n")

isCommentWhitespace :: LEpaComment -> Bool
isCommentWhitespace comm@(L (anchor -> span) _ ) =
  not (isPointRealSpan span) && isStringWhitespace (commentText comm)

emptyCommentSingle, emptyHaddockSingle  :: LEpaComment -> Idea
emptyCommentSingle = emptyComment ("--" ++) "Empty single-line comment"
emptyHaddockSingle = emptyComment ("--" ++) "Empty single-line haddock"

emptyCommentMulti, emptyHaddockMulti :: LEpaComment -> Idea
emptyCommentMulti = emptyComment (\s -> "{-" ++ s ++ "-}") "Empty multi-line comment"
emptyHaddockMulti = emptyComment (\s -> "{-" ++ s ++ "-}") "Empty multi-line haddock"

refact :: SrcSpan -> String -> [Refactoring R.SrcSpan]
refact loc s = [ModifyComment (toRefactSrcSpan loc) s]

emptyComment :: (String -> String) -> String -> LEpaComment -> Idea
emptyComment f msg o@(L pos _) =
  let s1 = commentText o
      loc = RealSrcSpan (anchor pos) GHC.Data.Strict.Nothing
  in ideaRemove Suggestion msg loc (f s1) (refact loc "")

grab :: String -> LEpaComment -> String -> Idea
grab msg o@(L pos _) s2 =
  let s1 = commentText o
      loc = RealSrcSpan (anchor pos) GHC.Data.Strict.Nothing
      f s = if isCommentMultiline o then "{-" ++ s ++ "-}" else "--" ++ s
  in rawIdea Suggestion msg loc (f s1) (Just $ f s2) [] (refact loc $ f s2)

-- | 'True' if the span is known to straddle only one line. GHC doesn't export
-- isOneLineRealSpan.
isOneLineRealSpan :: RealSrcSpan -> Bool
isOneLineRealSpan span = isOneLineSpan (RealSrcSpan span GHC.Data.Strict.Nothing)

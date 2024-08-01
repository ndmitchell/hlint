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

import Debug.Trace

import Hint.Type
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

commentRuns :: ModuleEx -> [[LEpaComment]]
commentRuns m =
    traceShow (map (map commentText) xs)
    xs
  where
    comments :: [LEpaComment]
    comments = ghcComments m

    xs =
      foldl'
        (\xs y@(L (anchor -> spanY) _) ->
          case xs of
            [] -> [[y]]
            head@(((L (anchor -> spanX) _)) : _) : tails ->
              let startX = srcSpanStartLine spanX
                  startY = srcSpanStartLine spanY
                  endX = srcSpanEndLine spanX
                  endY = srcSpanEndLine spanY
              in
                traceShow ((startY, endY), (startX, endX)) $
                if endY + 1 == startX then (y : head) : tails else [y] : xs
            )
        []
        (reverse comments)

dropBlankLinesHint :: [LEpaComment] -> (Bool, [Idea])
dropBlankLinesHint comments =
  (True, traceShow ys $ traceShow xs $ trace content'' $ trace content $ traceShow comments [])
  -- (True, [])
  where
    xs = commentText <$> comments
    content = unlines $ ("- --" ++) <$> xs

    ys = (\l ->
      [ x
      | (x,y) <- zip l (tail l)
      , x /= y || x /= ""
      ]) xs

    content'' = unlines $ ("+ --" ++) <$> ys

commentHint :: ModuHint
commentHint _ m =
  if any fst runs
    then concatMap snd runs
    else concatMap (check singleLines someLines) comments
  where
    comments = ghcComments m
    singleLines = sort $ commentLine <$> filter isSingle comments
    someLines = sort $ commentLine <$> filter isSingleSome comments
    runs = dropBlankLinesHint <$> commentRuns m

-- | Does the commment start with "--"? Can be empty. Excludes haddock single
-- line comments, "-- |" and "-- ^".
isSingle :: LEpaComment -> Bool
isSingle comm@(L (anchor -> span) _) =
  isOneLineRealSpan span
  && not (isPointRealSpan span)
  && not (isCommentMultiline comm || isHaddock comm || isDoctest comm)

-- | A single line comment about something where something is:
-- * Not a haddock comment "-- |" or "-- ^"
-- * Not a multi-line comment "{- ... -}"
-- * Not a whitespace comment "--       "
isSingleSome :: LEpaComment -> Bool
isSingleSome comm@(L (anchor -> span) _) =
  isOneLineSpan (RealSrcSpan span GHC.Data.Strict.Nothing)
  && not (isPointRealSpan span)
  && not (isCommentMultiline comm || isHaddock comm || isDoctest comm || isCommentWhitespace comm)

-- | The start line number of a comment.
commentLine :: LEpaComment -> Int
commentLine (L (anchor -> span) _) = srcLocLine $ realSrcSpanStart span

-- | Do we have two consecutive empty single comment lines?
doubleEmpty :: [Int] -> [Int] -> Bool
doubleEmpty singles somes = let empties = somes \\ singles in
  0 `elem` zipWith (-) (drop 1 $ empties) empties

-- | Do we have trailing empty single comment lines?
trailingEmpty :: [Int] -> [Int] -> Bool
trailingEmpty singles somes =
  traceShow ("trailing", singles, somes) $
  leadingEmpty (reverse singles) (reverse somes)

-- | Do we have leading empty single comment lines?
leadingEmpty :: [Int] -> [Int] -> Bool
leadingEmpty singles somes =
  let empties = singles \\ somes in
  traceShow ("leading", empties, singles, somes) $
  case (empties, somes) of
      (_, []) -> True
      ([], _) -> False
      (e : _, s : _) -> traceShow ("e vs s", e < s) $ e < s

data EmptyComment = EmptyHaddock | EmptyDoctest | EmptyComment deriving Eq

ppr :: EmptyComment -> String
ppr EmptyHaddock = "haddock"
ppr EmptyDoctest = "doctest"
ppr EmptyComment = "comment"

commentFirstLine :: LEpaComment -> Maybe EmptyComment
commentFirstLine comm@(L _ _) = let s = commentText comm
  in case s of
    "" -> Just EmptyComment
    " |" -> Just EmptyHaddock
    " >>>" -> Just EmptyDoctest
    _ -> Nothing

check :: [Int] -> [Int] -> LEpaComment -> [Idea]
check singles somes comm@(L{})
  | isHaddockWhitespace comm = traceShow ("haddock", comm) $
      if | isMultiline -> [emptyHaddockMulti comm]
         | leadingEmptyHaddock ->
            traceShow (line, singles, somes) $
            [replaceComment "Try this" comm]
            --[leadingEmptyIdea EmptyHaddock comm]
         | leadingEmpty singles somes -> [leadingEmptyIdea EmptyHaddock comm]
        --  | trailingEmpty singles somes -> [trailingEmptyIdea EmptyHaddock comm]
        --  | doubleEmpty singles somes -> [doubleEmptyIdea EmptyHaddock comm]
         | otherwise -> []
  -- | isDoctestWhitespace comm =
  --     if | leadingEmpty singles somes -> [leadingEmptyIdea EmptyDoctest comm]
  --        | trailingEmpty singles somes -> [trailingEmptyIdea EmptyDoctest comm]
  --        | doubleEmpty singles somes -> [doubleEmptyIdea EmptyDoctest comm]
  --        | otherwise -> []
  -- | isCommentWhitespace comm =
  --     if | isMultiline -> [emptyCommentMulti comm]
  --        | leadingEmpty singles somes -> [leadingEmptyIdea EmptyComment comm]
  --        | trailingEmpty singles somes -> [trailingEmptyIdea EmptyComment comm]
  --        | doubleEmpty singles somes -> [doubleEmptyIdea EmptyComment comm]
  --        | otherwise -> []
  -- | isMultiline, null (commentText comm) = [emptyCommentMulti comm]
  -- | isMultiline, "#" `isSuffixOf` s && not ("#" `isPrefixOf` s) = [grab "Fix pragma markup" comm $ '#':s]
  -- | isMultiline, name `elem` directives = [grab "Use pragma syntax" comm $ "# " ++ trim s ++ " #"]
    where
      isMultiline = isCommentMultiline comm
      s = commentText comm
      leadingEmptyHaddock = commentFirstLine comm == Just EmptyHaddock
      line = commentLine comm
      -- name = takeWhile (\x -> isAlphaNum x || x == '_') $ trimStart s
check _ _ _ = []

isHaddock :: LEpaComment -> Bool
isHaddock (take 2 . commentText -> s) = " |" == s || " ^" == s

isDoctest :: LEpaComment -> Bool
isDoctest (commentText -> s) = " >>>" `isPrefixOf` s

isStringWhitespace :: String -> Bool
isStringWhitespace = not . any (`notElem` " \t\r\n")

isCommentWhitespace :: LEpaComment -> Bool
isCommentWhitespace comm@(L (anchor -> span) _ ) =
  not (isPointRealSpan span) && isStringWhitespace (commentText comm)

isHaddockWhitespace :: LEpaComment -> Bool
isHaddockWhitespace comm = isHaddock comm && isStringWhitespace (drop 2 $ commentText comm)

isDoctestWhitespace :: LEpaComment -> Bool
isDoctestWhitespace comm@(L (anchor -> span) _ ) = not (isPointRealSpan span) && isDoctest comm

doubleEmptyIdea :: EmptyComment -> LEpaComment -> Idea
doubleEmptyIdea s = emptyComment ("--" ++) $ "Double empty single-line " ++ ppr s

trailingEmptyIdea :: EmptyComment -> LEpaComment -> Idea
trailingEmptyIdea s = emptyComment ("--" ++) $ "Trailing empty single-line " ++ ppr s

leadingEmptyIdea :: EmptyComment -> LEpaComment -> Idea
leadingEmptyIdea s = emptyComment ("--" ++) $ "Leading empty single-line " ++ ppr s

emptyMultiIdea :: String -> LEpaComment -> Idea
emptyMultiIdea s = emptyComment (\s -> "{-" ++ s ++ "-}") $ "Empty multi-line " ++ s

emptyCommentMulti, emptyHaddockMulti :: LEpaComment -> Idea
emptyCommentMulti = emptyMultiIdea "comment"
emptyHaddockMulti = emptyMultiIdea "haddock"

refact :: SrcSpan -> String -> [Refactoring R.SrcSpan]
refact loc s = [ModifyComment (toRefactSrcSpan loc) s]

replaceComment :: String -> LEpaComment -> Idea
replaceComment update o@(L pos _) =
  let s1 = commentText o
      loc = RealSrcSpan (anchor pos) GHC.Data.Strict.Nothing
  in
    rawIdea
      Suggestion
      "Remove comment blank lines"
      loc
      s1
      (Just update)
      []
      (refact loc "")
      --[ModifyComment (toRefactSrcSpan pos) "Do this"]

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

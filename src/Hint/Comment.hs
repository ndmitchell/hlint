{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Maybe (fromMaybe)

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

data Comments = Comments
    { commPragma :: ![LEpaComment]
    , commBlockHaddocks :: ![LEpaComment]
    , commBlocks :: ![LEpaComment]
    -- TODO: Process the different types of block comments; [" |",""].
    -- * Haddock comments
    -- * Simple comments
    , commRunHaddocks :: ![[LEpaComment]]
    , commRuns :: ![[LEpaComment]]
    , commLineHaddocks :: ![LEpaComment]
    , commLines :: ![LEpaComment]
    }

classifyComments :: [LEpaComment] -> Comments
classifyComments xs = Comments pragmas blockHaddocks blocks runHaddocks runs lineHaddocks lines where
  (partition isCommentPragma -> (pragmas, allBlocks), singles) = partition isCommentMultiline xs
  (blockHaddocks, blocks) = partition isCommentHaddock allBlocks
  (concat -> singles', rawRuns) = partition ((== 1) . length) $ commentRuns singles
  (runHaddocks, runs) = partition (\case x : _  -> isCommentHaddock x; _ -> False) rawRuns
  (lineHaddocks, lines) = partition isCommentHaddock singles'

commentRuns :: [LEpaComment] -> [[LEpaComment]]
commentRuns comments =
    traceShow (map (map commentText) xs)
    xs
  where
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

isHaddockLeader :: String -> Bool
isHaddockLeader h = h == " |" || h == " ^"

dropBlankLinesHint :: [LEpaComment] -> [Idea]
dropBlankLinesHint comments =
  traceShow xs $
  traceShow ys $
  traceShow ys' $
  trace content $
  trace content'' $
  replaceComment "Drop blank lines" (head comments) : map (emptyComment (\s -> "{-" ++ s ++ "-}") "Drop blank lines") (tail comments)
  where
    xs = commentText <$> comments
    content = unlines $ ("- --" ++) <$> xs

    ys = (\l ->
      [ traceShow ("x", "y", (x, y)) x
      | (x,y) <- zip l (tail l)
      , x /= y || x /= ""
      ]) (xs ++ [""])

    -- Get rid of leading empty lines with haddock comments.
    -- TODO: Add configuration on how to merge into haddock leader.
    ys' = case ys of
      -- Merge first non-empty line with haddock leader.
      h : "" : y : ys | isHaddockLeader h -> (h ++ y) : ys

      -- Place first non-empty line one line after haddock leader.
      h : "" : ys | isHaddockLeader h -> h : ys

      _ -> ys

    content'' = unlines $ ("+ --" ++) <$> ys'

commentHint :: ModuHint
commentHint _ m =
  -- PLAN: Split the comment into;
  -- a) block comments {- .. -}
  -- b) runs of single-line comments
  -- c) single-line comments
  -- TODO: Remove (True, _) runs and then run the other checks on the rest.
  traceShow ("pragmas", commentText <$> pragmas) $
  traceShow ("blockHaddocks", commentText <$> blockHaddocks) $
  traceShow ("blocks", commentText <$> blocks) $
  traceShow ("runHaddocks", fmap commentText <$> runHaddocks) $
  traceShow ("runs", fmap commentText <$> runs) $
  traceShow ("lineHaddocks", commentText <$> lineHaddocks) $
  traceShow ("lines", commentText <$> lines) $
  pragmaIdeas
  ++ blockHaddockIdeas
  ++ blockIdeas
  -- ++ runHaddockIdeas
  ++ ideas
  where
    -- Comments need to be sorted by line number for detecting runs of single
    -- line comments but @ghcComments@ doesn't always do that even though most
    -- of the time it seems to.
    comments :: [LEpaComment]
    comments = sortOn (\(L (anchor -> span) _) -> srcSpanStartLine span) $ ghcComments m

    singleLines = sort $ commentLine <$> filter isSingle comments
    someLines = sort $ commentLine <$> filter isSingleSome comments

    Comments pragmas blockHaddocks blocks runHaddocks runs lineHaddocks lines = classifyComments comments

    pragmaIdeas = concatMap checkEmptyPragma pragmas
    blockHaddockIdeas = concatMap checkEmptyBlockHaddock blockHaddocks
    blockIdeas = concatMap checkEmptyBlock blocks
    -- runHaddockIdeas = concatMap (\x -> traceShow ("YYYY", x) checkEmptyRunHaddock x) runHaddocks
    -- runHaddockIdeas = concatMap (\cs@(c : _) -> let s = commentText <$> cs in
    --   traceShow ("ZZZZ", s) checkEmptyRunHaddock s c) runHaddocks
    runHaddockIdeas = case runHaddocks of
      [xs@(x : _), ys@(y : _)] ->
        -- (let s = commentText <$> xs in traceShow ("XXXX", s, x) checkEmptyRunHaddock s x)
        -- ++
        (let s = commentText <$> ys in traceShow ("YYYY", s, y) checkEmptyRunHaddock s y)
      _ -> []
    runIdeas = [] -- concatMap checkEmptyRun runs

    runReplacements = runHaddockIdeas ++ runIdeas

    ideas = if not (null runReplacements)
      then runReplacements
      else concatMap (check singleLines someLines) comments

checkEmptyRunHaddock :: [String] -> LEpaComment -> [Idea]
checkEmptyRunHaddock cs c@(L pos _) = trace "CHECK-EMPTY-RUN-HADDOCK" $
  let s = unlines cs
      s' = unlines ["--" ++ t | t <- lines s]
      msg = s' -- "QQQ: at " ++ show pos ++ " with content: " ++ s'
  in
    traceShow ("run", s, s', commentText c, pos) $
    if | isHaddockStringWhitespace s -> [emptyComment (const msg) ("Empty haddock run: " ++ show pos) c]
       | isHaddockDoctestWhitespace cs -> [emptyComment (const msg) ("Empty doctest run: " ++ show pos) c]
       | otherwise -> []

-- checkEmptyRunHaddock :: [String] -> LEpaComment -> [Idea]
-- checkEmptyRunHaddock cs c@(L pos _) = traceShow ("run", s, s', commentText c, pos) $
--   [emptyComment (const ("XXX: at " ++ show pos ++ " with content: " ++ s')) "Empty haddock run" c | isHaddockStringWhitespace s]
--   where
--     s = unlines cs
--     s' = unlines ["--" ++ t | t <- lines s]

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

checkEmptyBlockHaddock :: LEpaComment -> [Idea]
checkEmptyBlockHaddock comm = [emptyHaddockMulti comm | isHaddockWhitespace comm]

checkEmptyBlock :: LEpaComment -> [Idea]
checkEmptyBlock comm = [emptyCommentMulti comm | isCommentWhitespace comm]

checkEmptyPragma :: LEpaComment -> [Idea]
checkEmptyPragma comm = [emptyPragma comm | isPragmaWhitespace comm]

-- checkEmptyRunHaddock :: [LEpaComment] -> [Idea]
-- checkEmptyRunHaddock cs@(c@(L pos _) : _) = traceShow ("run", s, s', commentText c, pos) $
--   [emptyComment (const ("XXX: at " ++ show pos ++ " with content: " ++ s')) "Empty haddock run" c | isHaddockStringWhitespace s]
--   where
--     s = unlines $ commentText <$> cs
--     s' = unlines ["--" ++ t | t <- lines s]

checkEmptyRun :: [LEpaComment] -> [Idea]
checkEmptyRun = dropBlankLinesHint

check :: [Int] -> [Int] -> LEpaComment -> [Idea]
check singles somes comm@(L{})
  -- Multi-line haddock comments are handled elsewhere.
  | isHaddockWhitespace comm && not (isCommentMultiline comm) = traceShow ("haddock", comm) $
      if | leadingEmptyHaddock ->
            traceShow (line, singles, somes)
            -- [replaceComment "Try this" comm]
            -- [leadingEmptyIdea EmptyHaddock comm]
            []
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
isHaddockWhitespace comm = isHaddock comm && isHaddockStringWhitespace (commentText comm)

isHaddockStringWhitespace :: String -> Bool
isHaddockStringWhitespace s = isStringWhitespace (drop 2 s)

isPragmaWhitespace :: LEpaComment -> Bool
isPragmaWhitespace comm = maybe False isStringWhitespace
  (stripSuffix "#" =<< stripPrefix "#" (commentText comm))

isHaddockDoctestWhitespace :: [String] -> Bool
isHaddockDoctestWhitespace [] = False
isHaddockDoctestWhitespace (x : xs) = isHaddockStringWhitespace x && and
  [ null s || " >>>" == s
  | s <- xs
  ]

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

emptyPragma :: LEpaComment -> Idea
emptyPragma = emptyComment (\s -> "{-" ++ s ++ "-}") $ "Empty pragma "

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
emptyComment f msg o@(L pos _) = traceShow ("EMPTY-COMMENT", show pos) $
  let !s1 = commentText o
      !loc = RealSrcSpan (anchor pos) GHC.Data.Strict.Nothing
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

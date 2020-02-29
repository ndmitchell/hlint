
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


module Hint.Comment(commentHint) where

import Hint.Type
import Data.Char
import Data.List.Extra
import Refact.Types(Refactoring(ModifyComment))
import SrcLoc
import ApiAnnotation
import GHC.Util

directives :: [String]
directives = words $
    "LANGUAGE OPTIONS_GHC INCLUDE WARNING DEPRECATED MINIMAL INLINE NOINLINE INLINABLE " ++
    "CONLIKE LINE SPECIALIZE SPECIALISE UNPACK NOUNPACK SOURCE"


commentHint :: ModuHint
commentHint _ m = concatMap chk (ghcComments m)
    where
        chk :: Located AnnotationComment -> [Idea]
        chk comm
          | isMultiline, "#" `isSuffixOf` s && not ("#" `isPrefixOf` s) = [grab "Fix pragma markup" comm $ '#':s]
          | isMultiline, name `elem` directives = [grab "Use pragma syntax" comm $ "# " ++ trim s ++ " #"]
               where
                 isMultiline = isCommentMultiline comm
                 s = commentText comm
                 name = takeWhile (\x -> isAlphaNum x || x == '_') $ trimStart s
        chk _ = []

        grab :: String -> Located AnnotationComment -> String -> Idea
        grab msg o@(L pos _) s2 =
          let s1 = commentText o in
          rawIdea' Suggestion msg pos (f s1) (Just $ f s2) [] refact
            where f s = if isCommentMultiline o then "{-" ++ s ++ "-}" else "--" ++ s
                  refact = [ModifyComment (toRefactSrcSpan' pos) (f s2)]

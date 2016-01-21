
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


pragmas = words $
    "LANGUAGE OPTIONS_GHC INCLUDE WARNING DEPRECATED MINIMAL INLINE NOINLINE INLINABLE " ++
    "CONLIKE LINE SPECIALIZE SPECIALISE UNPACK NOUNPACK SOURCE"


commentHint :: Comment -> [Idea]
commentHint c@(Comment True span s)
    | "#" `isSuffixOf` s && not ("#" `isPrefixOf` s) = [grab "Fix pragma markup" c $ '#':s]
    | name `elem` pragmas = [grab "Use pragma syntax" c $ "# " ++ trim s ++ " #"]
        where name = takeWhile (\x -> isAlphaNum x || x == '_') $ dropWhile isSpace s
commentHint _ = []

grab :: String -> Comment -> String -> Idea
grab msg (Comment typ pos s1) s2 = rawIdea Suggestion msg pos (f s1) (Just $ f s2) [] refact
    where f s = if typ then "{-" ++ s ++ "-}" else "--" ++ s
          refact = [ModifyComment (toRefactSrcSpan pos) (f s2)]

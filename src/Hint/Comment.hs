
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
import Data.List
import Util


pragmas = words $
    "LANGUAGE OPTIONS_GHC INCLUDE WARNING DEPRECATED MINIMAL INLINE NOINLINE INLINABLE " ++
    "CONLIKE LINE SPECIALIZE SPECIALISE UNPACK NOUNPACK SOURCE"


commentHint :: Comment -> [Idea]
commentHint c@(Comment True span s)
    | "#" `isSuffixOf` s && not ("#" `isPrefixOf` s) = [suggest "Fix pragma markup" c $ '#':s]
    | name `elem` pragmas = [suggest "Use pragma syntax" c $ "# " ++ trim s ++ " #"]
        where name = takeWhile (\x -> isAlphaNum x || x == '_') $ dropWhile isSpace s
commentHint _ = []

suggest :: String -> Comment -> String -> Idea
suggest msg (Comment typ pos s1) s2 = rawIdea Warning msg pos (f s1) (Just $ f s2) []
    where f s = if typ then "{-" ++ s ++ "-}" else "--" ++ s

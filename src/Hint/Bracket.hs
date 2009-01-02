{-# LANGUAGE PatternGuards #-}

{-
<TEST>
yes = (f x) x where res = f x x
no = f (x x)
yes = (f x) ||| y where res = f x ||| y
yes = if (f x) then y else z where res = if f x then y else z
yes = if x then (f y) else z where res = if x then f y else z
</TEST>
-}


module Hint.Bracket where

import Control.Arrow
import Data.Maybe
import Type
import HSE.All


bracketHint :: Hint
bracketHint = concatMap bracketExp . children0Exp nullSrcLoc

bracketExp :: (SrcLoc,Exp) -> [Idea]
bracketExp (loc,x) =
    [idea "Redundant brackets" loc x (fromParen x) | isParen x] ++ f loc x
    where
        f loc x = [idea "Redundant brackets" loc x y | let y = descendBracket (isParen &&& fromParen) x, x /= y] ++
                  concatMap (uncurry f) (children1Exp loc x)

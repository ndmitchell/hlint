{-# LANGUAGE PatternGuards #-}

{-
<TEST>
yes = (f x) x where res = f x x
no = f (x x)
yes = (f x) ||| y where res = f x ||| y
yes = if (f x) then y else z where res = if f x then y else z
yes = if x then (f y) else z where res = if x then f y else z

no = groupFsts . sortFst $ mr
yes = split "to" $ names where res = split "to" names
yes = white $ keysymbol where res = white keysymbol
yes = operator foo $ operator where res = operator foo operator
no = operator foo $ operator bar
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
    [g loc x (fromParen x) | isParen x] ++ f loc x
    where
        f loc x = testDirect loc x ++ testDollar loc x ++ concatMap (uncurry f) (children1Exp loc x)
        g = warn "Redundant brackets"

        testDirect loc x = [g loc x y | let y = descendBracket (isParen &&& fromParen) x, x /= y]

        testDollar loc x =
            [idea Error "Redundant $" loc x y | InfixApp a d b <- [x], opExp d ~= "$"
            ,let y = App a b, not $ needBracket 0 y a, not $ needBracket 1 y b]

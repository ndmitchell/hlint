{-# LANGUAGE PatternGuards #-}

{-
<TEST>
yes = (f x) x where res = f x x
no = f (x x)
yes = (f x) ||| y where res = f x ||| y
yes = if (f x) then y else z where res = if f x then y else z
yes = if x then (f y) else z where res = if x then f y else z
yes = (a foo) :: Int where res = a foo :: Int

no = groupFsts . sortFst $ mr
yes = split "to" $ names where res = split "to" names
yes = white $ keysymbol where res = white keysymbol
yes = operator foo $ operator where res = operator foo operator
no = operator foo $ operator bar
yes = (b $ c d) ++ e where res = b (c d) ++ e
</TEST>
-}


module Hint.Bracket where

import Control.Arrow
import Data.Maybe
import Type
import HSE.All


bracketHint :: Hint
bracketHint _ = concatMap bracketExp . children0Exp nullSrcLoc

bracketExp :: (SrcLoc,Exp) -> [Idea]
bracketExp (loc,x) =
    [g loc x (fromParen x) | isParen x] ++ f loc x
    where
        f loc x = testDirect loc x ++ testDollar loc x ++ concatMap (uncurry f) (children1Exp loc x)
        g = warn "Redundant brackets"

        testDirect loc x = [g loc x y | let y = descendBracket (isParen &&& fromParen) x, x /= y]

        testDollar loc x =
            [idea Error "Redundant $" loc x y | InfixApp a d b <- [x], opExp d ~= "$"
            ,let y = App a b, not $ needBracket 0 y a, not $ needBracket 1 y b] ++
            [idea Error "Redundant $" loc x y | InfixApp a op b <- [x]
            ,(first,Paren (InfixApp c1 d c2)) <- [(True,a),(False,b)], opExp d ~= "$"
            ,let c = App c1 (Paren c2),let (a2,b2) = if first then (c,b) else (a,c)
            ,let y = InfixApp a2 op b2]

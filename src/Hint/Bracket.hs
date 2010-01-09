{-
<TEST>
yes = (f x) x -- f x x
no = f (x x)
yes = (foo) -- foo
yes = (f x) ||| y -- f x ||| y
yes = if (f x) then y else z -- if f x then y else z
yes = if x then (f y) else z -- if x then f y else z
yes = (a foo) :: Int -- a foo :: Int

no = groupFsts . sortFst $ mr
yes = split "to" $ names -- split "to" names
yes = white $ keysymbol -- white keysymbol
yes = operator foo $ operator -- operator foo operator
no = operator foo $ operator bar
yes = (b $ c d) ++ e -- b (c d) ++ e
yes = (a b $ c d) ++ e -- a b (c d) ++ e
no = (f . g $ a) ++ e
yes = [(foo bar)] -- [foo bar]
</TEST>
-}


module Hint.Bracket where

import Control.Arrow
import Type
import Hint
import HSE.All


bracketHint :: DeclHint
bracketHint _ _ = concatMap bracketExp . childrenBi

bracketExp :: Exp_ -> [Idea]
bracketExp x =
    [g x (fromParen x) | isParen x] ++ f x
    where
        f x = testDirect x ++ testDollar x ++ concatMap f (children x)
        g = warn "Redundant brackets"

        testDirect x = [g x y | let y = descendBracket (isParen &&& fromParen) x, x /=~= y]

        testDollar x =
            [idea Error "Redundant $" x y | InfixApp _ a d b <- [x], opExp d ~= "$"
            ,let y = App an a b, not $ needBracket 0 y a, not $ needBracket 1 y b]
            ++
            [idea Error "Redundant $" x (t y)
            |(t, Paren _ (InfixApp _ a1 op1 a2)) <- infixes x
            ,opExp op1 ~= "$", isVar a1 || isApp a1 || isParen a1, not $ isAtom a2
            ,let y = App an a1 (Paren an a2)]


-- return both sides, and a way to put them together again
infixes :: Exp_ -> [(Exp_ -> Exp_, Exp_)]
infixes (InfixApp s a b c) = [(InfixApp s a b, c), (\a -> InfixApp s a b c, a)]
infixes _ = []

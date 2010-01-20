{-
Raise an error if you are bracketing an atom, or are enclosed be a list bracket

<TEST>
-- bracket reduction
yes = (f x) x -- @Warning f x x
no = f (x x)
yes = (foo) -- @Error foo
yes = (foo bar) -- @Warning foo bar
yes = foo (bar) -- @Error bar
yes = foo ((x x)) -- @Error (x x)
yes = (f x) ||| y -- @Warning f x ||| y
yes = if (f x) then y else z -- @Warning if f x then y else z
yes = if x then (f y) else z -- @Warning if x then f y else z
yes = (a foo) :: Int -- @Warning a foo :: Int
yes = [(foo bar)] -- @Warning [foo bar]

-- dollar reduction tests
no = groupFsts . sortFst $ mr
yes = split "to" $ names -- split "to" names
yes = white $ keysymbol -- white keysymbol
yes = operator foo $ operator -- operator foo operator
no = operator foo $ operator bar

-- $/bracket rotation tests
yes = (b $ c d) ++ e -- b (c d) ++ e
yes = (a b $ c d) ++ e -- a b (c d) ++ e
no = (f . g $ a) ++ e
</TEST>
-}


module Hint.Bracket where

import Type
import Hint
import HSE.All


bracketHint :: DeclHint
bracketHint _ _ = concatMap bracketExp . childrenBi




bracketExp :: Exp_ -> [Idea]
bracketExp x = bracket True x ++ dollar x


bracket :: Bool -> Exp_ -> [Idea]
bracket bad = f Nothing
    where
        msg = "Redundant bracket"

        -- f (Maybe (index, parent, gen)) child
        f _ o@(Paren _ x) | isAtom x = err msg o x : g x
        f Nothing o@(Paren _ x) | bad = warn msg o x : g x
        f (Just (i,o,gen)) (Paren _ x) | not $ needBracket i o x = warn msg o (gen x) : g x
        f _ x = g x

        g o = concat [f (Just (i,o,gen)) x | (i,(x,gen)) <- zip [0..] $ holes o]


dollar :: Exp_ -> [Idea]
dollar = concatMap f . universe
    where
        msg = warn "Redundant $"
        f x = [msg x y | InfixApp _ a d b <- [x], opExp d ~= "$"
              ,let y = App an a b, not $ needBracket 0 y a, not $ needBracket 1 y b]
              ++
              [msg x (t y) |(t, Paren _ (InfixApp _ a1 op1 a2)) <- infixes x
              ,opExp op1 ~= "$", isVar a1 || isApp a1 || isParen a1, not $ isAtom a2
              ,let y = App an a1 (Paren an a2)]


-- return both sides, and a way to put them together again
infixes :: Exp_ -> [(Exp_ -> Exp_, Exp_)]
infixes (InfixApp s a b c) = [(InfixApp s a b, c), (\a -> InfixApp s a b c, a)]
infixes _ = []

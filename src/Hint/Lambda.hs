{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Find and match:

    foo a = \x -> y (provided no where's outside the lambda)
    \x -> y (use const, if the variable x does not occur in y, and it doesn't need bracketing)
    foo x = y x (eta reduce)
    foo x = f $ g x (eta reduce, convert to .)
    foo x y = f (g x) (g y) ==> f `on` g
    -- Never offer to eta reduce if the variable is named mr and is the only one (Monomorphism Restriction)
    -- don't eta reduce func a b c = .... (g b) (g c) to (g b) . g, looks ugly

<TEST>
f a = \x -> x + x -- f a x = x + x
h a = f (g a ==) -- h = f . (==) . g
test a = foo (\x -> True) -- const True
test = foo (\x -> map f [])
test = 0 where f x = y x -- f = y
test mr = y mr
test = 0 where f x = g $ f $ map head x -- f = g . f . map head
test z x y = f (g x) (g y)
f x y = f (g x) (g y)
f x y = g x == g y -- f = (==) `on` g
a + b = foo a b -- (+) = foo
h a = f ((++) a) a -- (a ++)
h a = flip f x (y z) -- f (y z) x
h a = flip f x $ y z
yes = foo (\x -> sum x) -- sum
yes = foo (\x l -> sum x x l) -- \x -> sum x x
test = foo (\x -> y == x) -- (y ==)
test = foo (\x -> x == g y) -- (== g y)
test = foo (\x -> g x == x)
cp i = show (i - 1)
cp i = show (i + 1)
test c = 1 # 2 # c
</TEST>
-}


module Hint.Lambda where

import HSE.All
import Type
import Hint


lambdaHint :: DeclHint
lambdaHint _ _ x = concatMap lambdaExp (universeBi x) ++ concatMap lambdaDecl (universe x)


lambdaExp :: Exp_ -> [Idea]
lambdaExp o@(Lambda _ [v] y) | isAtom y, Just x <- f v, x `notElem` vars y =
        [warn "Use const" o res]
    where
        f (view -> PVar_ x) = Just x
        f PWildCard{} = Just "_"
        f _ = Nothing
        res = App an (toNamed "const") y
lambdaExp o@(Lambda _ vs x) | length vs /= length vs2 =
        [warn "Eta reduce" o $ if null vs2 then x2 else Lambda an vs2 x2]
    where (vs2,x2) = etaReduces vs x
lambdaExp o@(Paren _ (App _ (Var _ x@(UnQual _ Symbol{})) y)) | isAtom y =
        [warn "Operator rotate" o $ LeftSection an y (QVarOp an x)]
lambdaExp o@(App _ (App _ (App _ flp x) y) z) | flp ~= "flip" =
        [err "Redundant flip" o $ App an (App an x z) y]
lambdaExp _ = []


lambdaDecl :: Decl_ -> [Idea]
lambdaDecl (PatBind _ (PVar _ x) typ rhs bind) = lambdaDef $ Match an x [] rhs bind
lambdaDecl (FunBind _ [x]) = lambdaDef x --only apply to 1-def, because arities must be the same
lambdaDecl _ = []


lambdaDef :: Match S -> [Idea]
lambdaDef o@(Match _ name pats (UnGuardedRhs _ bod) Nothing)
    | Lambda loc vs y <- bod = [warn "Redundant lambda" o $ reform (pats++vs) y]
    | [PVar _ x, PVar _ y] <- pats, Just (f,g) <- useOn x y bod =
              [warn "Use on" o $ reform [] (ensureBracket1 $ InfixApp an f (toNamed "on") g)]
    | (p2,y) <- etaReduces pats bod, length p2 /= length pats = [warn "Eta reduce" o $ reform p2 y]
    | otherwise = []
        where reform pats2 bod2 = Match an name pats2 (UnGuardedRhs an bod2) Nothing
lambdaDef (InfixMatch an p name ps rhs binds) = lambdaDef $ Match an name (p:ps) rhs binds
lambdaDef _ = []


-- given x y, f (g x) (g y) = Just (f, g)
useOn :: Name S -> Name S -> Exp_ -> Maybe (Exp_, Exp_)
useOn x1 y1 (view -> App2 f (view -> App1 g1 x2) (view -> App1 g2 y2))
    | fromNamed f `elem` ["==",">=",">","!=","<","<="] && g1 =~= g2, map (Var an . UnQual an) [x1,y1] `eqList` [x2,y2]
    = Just (f,g1)
useOn _ _ _ = Nothing


etaReduces :: [Pat S] -> Exp_ -> ([Pat S], Exp_)
etaReduces ps x | ps /= [], PVar_ p <- view $ last ps, p /= "mr", Just y <- etaReduce p x = etaReduces (init ps) y
                | otherwise = (ps,x)


etaReduce :: String -> Exp_ -> Maybe Exp_
etaReduce x (App _ y (view -> Var_ z)) | x == z && x `notElem` vars y = Just y
etaReduce x (InfixApp _ y op z) | f y z = Just $ RightSection an op z
                                | f z y = Just $ LeftSection an y op
    where f y z = prettyPrint op `notElem` ["+","-"] &&
                  all (not . isInfixApp) [y,z] && view y == Var_ x && x `notElem` vars z
etaReduce x (App _ y z) | not (uglyEta y z) && x `notElem` vars y = do
    z2 <- etaReduce x z
    return $ InfixApp an y (toNamed ".") z2
etaReduce x (view -> App2 dollar y z) | dollar ~= "$" = etaReduce x (App an y z)
etaReduce x (LeftSection _ y op) = etaReduce x $ App an (opExp op) y
etaReduce x y | isParen y = etaReduce x (fromParen y)
etaReduce x y = Nothing


-- (f (g x)) (h y), ugly if g == h
uglyEta :: Exp_ -> Exp_ -> Bool
uglyEta (fromParen -> App _ f (fromParen -> App _ g x)) (fromParen -> App _ h y) = g =~= h
uglyEta _ _ = False

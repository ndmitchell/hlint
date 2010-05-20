{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Concept:
    Remove all the lambdas you can be inserting only sections
    Never create a right section with +-# as the operator (they are misparsed)

    Rules:
    fun a = \x -> y  -- promote lambdas, provided no where's outside the lambda
    fun x = y x  -- eta reduce, x /= mr and foo /= symbol
    \x -> y x  -- eta reduce
    ((#) x) ==> (x #)  -- rotate operators
    (flip op x) ==> (`op` x)  -- rotate operators
    \x y -> x + y ==> (+)  -- insert operator
    \x y -> op y x ==> flip op
    \x -> x + y ==> (+ y)  -- insert section, 
    \x -> op x y ==> (`op` y)  -- insert section 
    \x -> y + x ==> (y +)  -- insert section
    \x -> \y -> ... ==> \x y -- lambda compression

<TEST>
f a = \x -> x + x -- f a x = x + x
f a = \x -> x + x where _ = test
fun x y z = f x y z -- fun = f
fun x y z = f x x y z -- fun x = f x x
fun x y z = f g z -- fun x y = f g
fun mr = y mr
f = foo ((*) x) -- (x *)
f = (*) x
f = foo (flip op x) -- (`op` x)
f = flip op x
f = foo (flip (*) x) -- (* x)
f = foo (flip (-) x)
f = foo (\x y -> fun x y) -- fun
f = foo (\x y -> x + y) -- (+)
f = foo (\x -> x * y) -- (* y)
f = foo (\x -> x # y)
f = foo (\x -> \y -> x x y y) -- \x y -> x x y y
f = foo (\x -> \x -> foo x x)
x ! y = fromJust $ lookup x y
f = foo (\i -> writeIdea (getClass i) i)
f = bar (flip Foo.bar x) -- (`Foo.bar` x)
f = a b (\x -> c x d)  -- (`c` d)
yes = \x -> f x (g y) -- (`f` g y)
yes = \x -> a x -- a
yes = \x y -> op y x -- flip op
</TEST>
-}


module Hint.Lambda where

import Hint.Util
import Hint.Type
import Util


lambdaHint :: DeclHint
lambdaHint _ _ x = concatMap lambdaExp (universeBi x) ++ concatMap lambdaDecl (universe x)


lambdaDecl :: Decl_ -> [Idea]
lambdaDecl o@(FunBind _ [Match _ name pats (UnGuardedRhs _ bod) Nothing])
    | Lambda _ vs y <- bod = [err "Redundant lambda" o $ reform (pats++vs) y]
    | (pats2,bod2) <- etaReduce pats bod, length pats2 < length pats = [err "Eta reduce" o $ reform pats2 bod2]
        where reform p b = FunBind an [Match an name p (UnGuardedRhs an b) Nothing]
lambdaDecl _ = []


etaReduce :: [Pat_] -> Exp_ -> ([Pat_], Exp_)
etaReduce ps (App _ x (Var _ (UnQual _ (Ident _ y))))
    | ps /= [], PVar _ (Ident _ p) <- last ps, p == y, p /= "mr", y `notElem` vars x
    = etaReduce (init ps) x
etaReduce ps x = (ps,x)


lambdaExp :: Exp_ -> [Idea]
lambdaExp o@(Paren _ (App _ (Var _ (UnQual _ (Symbol _ x))) y)) | isAtom y, allowLeftSection x =
    [warn "Use section" o $ LeftSection an y (toNamed x)]
lambdaExp o@(Paren _ (App _ (App _ (view -> Var_ "flip") (Var _ x)) y)) | allowRightSection $ fromNamed x =
    [warn "Use section" o $ RightSection an (QVarOp an x) y]
lambdaExp o@Lambda{} | res <- niceLambda [] o, not $ isLambda res =
    [warn "Avoid lambda" o res]
lambdaExp o@(Lambda _ ps1 (fromParen -> Lambda _ ps2 bod)) | pvars ps1 `disjoint` pvars ps2 =
    [warn "Collapse lambdas" o $ Lambda an (ps1++ps2) bod]
lambdaExp _ = []

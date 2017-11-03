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
    \x -> (x +) ==> (+) -- operator reduction

<TEST>
f a = \x -> x + x -- f a x = x + x
f a = \a -> a + a -- f _ a = a + a
f a = \x -> x + x where _ = test
f (test -> a) = \x -> x + x
f = \x -> x + x -- f x = x + x
fun x y z = f x y z -- fun = f
fun x y z = f x x y z -- fun x = f x x
fun x y z = f g z -- fun x y = f g
fun mr = y mr
fun x = f . g $ x -- fun = f . g
f = foo (\y -> g x . h $ y) -- g x . h
f = foo ((*) x) -- (x *)
f = (*) x
f = foo (flip op x) -- (`op` x)
f = flip op x
f = foo (flip (*) x) -- (* x)
f = foo (flip (-) x)
f = foo (\x y -> fun x y) -- @Warning fun
f = foo (\x y -> x + y) -- (+)
f = foo (\x -> x * y) -- @Suggestion (* y)
f = foo (\x -> x # y)
f = foo (\x -> \y -> x x y y) -- \x y -> x x y y
f = foo (\x -> \x -> foo x x) -- \_ x -> foo x x
f = foo (\(foo -> x) -> \y -> x x y y)
f = foo (\(x:xs) -> \x -> foo x x) -- \(_:xs) x -> foo x x
f = foo (\x -> \y -> \z -> x x y y z z) -- \x y z -> x x y y z z
x ! y = fromJust $ lookup x y
f = foo (\i -> writeIdea (getClass i) i)
f = bar (flip Foo.bar x) -- (`Foo.bar` x)
f = a b (\x -> c x d)  -- (`c` d)
yes = \x -> a x where -- a
yes = \x y -> op y x where -- flip op
f = \y -> nub $ reverse y where -- nub . reverse
f = \z -> foo $ bar $ baz z where -- foo . bar . baz
f = \z -> foo $ bar x $ baz z where -- foo . bar x . baz
f = \z -> foo $ z $ baz z where
f = \x -> bar map (filter x) where -- bar map . filter
f = bar &+& \x -> f (g x)
foo = [\column -> set column [treeViewColumnTitle := printf "%s (match %d)" name (length candidnates)]]
foo = [\x -> x]
foo = [\m x -> insert x x m]
foo a b c = bar (flux ++ quux) c where flux = a -- foo a b = bar (flux ++ quux)
foo a b c = bar (flux ++ quux) c where flux = c
yes = foo (\x -> Just x) -- @Warning Just
foo = bar (\x -> (x `f`)) -- f
baz = bar (\x -> (x +)) -- (+)
yes = blah (\ x -> case x of A -> a; B -> b) -- \ case A -> a; B -> b
no = blah (\ x -> case x of A -> a x; B -> b x)
</TEST>
-}


module Hint.Lambda(lambdaHint) where

import Hint.Util
import Hint.Type
import Util
import Data.List.Extra
import Data.Maybe
import Refact.Types hiding (RType(Match))


lambdaHint :: DeclHint
lambdaHint _ _ x = concatMap (uncurry lambdaExp) (universeParentBi x) ++ concatMap lambdaDecl (universe x)


lambdaDecl :: Decl_ -> [Idea]
lambdaDecl (toFunBind -> o@(FunBind loc1 [Match _ name pats (UnGuardedRhs loc2 bod) bind]))
    | isNothing bind, isLambda $ fromParen bod, null (universeBi pats :: [Exp_]) =
      [warn "Redundant lambda" o (gen pats bod) [Replace Decl (toSS o) s1 t1]]
    | length pats2 < length pats, pvars (drop (length pats2) pats) `disjoint` varss bind
        = [warn "Eta reduce" (reform pats bod) (reform pats2 bod2)
            [ -- Disabled, see apply-refact #3
              -- Replace Decl (toSS $ reform pats bod) s2 t2]]
            ]]
        where reform p b = FunBind loc [Match an name p (UnGuardedRhs an b) Nothing]
              loc = setSpanInfoEnd loc1 $ srcSpanEnd $ srcInfoSpan loc2
              gen ps = uncurry reform . fromLambda . Lambda an ps
              (finalpats, body) = fromLambda . Lambda an pats $ bod
              (pats2, bod2) = etaReduce pats bod
              template fps b = prettyPrint $ reform (zipWith munge ['a'..'z'] fps) (toNamed "body")
              munge :: Char -> Pat_ -> Pat_
              munge ident p@(PWildCard _) = p
              munge ident p = PVar (ann p) (Ident (ann p) [ident])
              subts fps b = ("body", toSS b) : zipWith (\x y -> ([x],y)) ['a'..'z'] (map toSS fps)
              s1 = subts finalpats body
              --s2 = subts pats2 bod2
              t1 = template finalpats body
              --t2 = template pats2 bod2

lambdaDecl _ = []

setSpanInfoEnd ssi (line, col) = ssi{srcInfoSpan = (srcInfoSpan ssi){srcSpanEndLine=line, srcSpanEndColumn=col}}


etaReduce :: [Pat_] -> Exp_ -> ([Pat_], Exp_)
etaReduce ps (App _ x (Var _ (UnQual _ (Ident _ y))))
    | ps /= [], PVar _ (Ident _ p) <- last ps, p == y, p /= "mr", y `notElem` vars x
    = etaReduce (init ps) x
etaReduce ps (InfixApp a x (isDol -> True) y) = etaReduce ps (App a x y)
etaReduce ps x = (ps,x)


--Section refactoring is not currently implemented.
lambdaExp :: Maybe Exp_ -> Exp_ -> [Idea]
lambdaExp p o@(Paren _ (App _ v@(Var l (UnQual _ (Symbol _ x))) y)) | isAtom y, allowLeftSection x =
    [suggestN "Use section" o (exp y x)] -- [Replace Expr (toSS o) subts template]]
    where
      exp op rhs = LeftSection an op (toNamed rhs)
--      template = prettyPrint (exp (toNamed "a") "*")
--      subts = [("a", toSS y), ("*", toSS v)]
lambdaExp p o@(Paren _ (App _ (App _ (view -> Var_ "flip") (Var _ x)) y)) | allowRightSection $ fromNamed x =
    [suggestN "Use section" o $ RightSection an (QVarOp an x) y]
lambdaExp p o@Lambda{} | maybe True (not . isInfixApp) p, (res, refact) <- niceLambdaR [] o, not $ isLambda res =
    [(if isVar res || isCon res then warn else suggest) "Avoid lambda" o res (refact $ toSS o)]
lambdaExp p o@(Lambda _ pats x) | isLambda (fromParen x), null (universeBi pats :: [Exp_]), maybe True (not . isLambda) p =
    [suggest "Collapse lambdas" o (Lambda an pats body) [Replace Expr (toSS o) subts template]]
    where
      (pats, body) = fromLambda o
      template = prettyPrint $  Lambda an (zipWith munge ['a'..'z'] pats) (toNamed "body")
      munge :: Char -> Pat_ -> Pat_
      munge ident p@(PWildCard _) = p
      munge ident p = PVar (ann p) (Ident (ann p) [ident])
      subts = ("body", toSS body) : zipWith (\x y -> ([x],y)) ['a'..'z'] (map toSS pats)
lambdaExp p o@(Lambda _ [view -> PVar_ u] (Case _ (view -> Var_ v) alts))
    | u == v, u `notElem` vars alts = [suggestN "Use lambda-case" o $ LCase an alts]
lambdaExp _ _ = []


-- replace any repeated pattern variable with _
fromLambda :: Exp_ -> ([Pat_], Exp_)
fromLambda (Lambda _ ps1 (fromLambda . fromParen -> (ps2,x))) = (transformBi (f $ pvars ps2) ps1 ++ ps2, x)
    where f bad x@PVar{} | prettyPrint x `elem` bad = PWildCard an
          f bad x = x
fromLambda x = ([], x)

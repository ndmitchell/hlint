{-# LANGUAGE PatternGuards, ViewPatterns #-}

{-
The matching does a fairly simple unification between the two terms, treating
any single letter variable on the left as a free variable. After the matching
we substitute, transform and check the side conditions. We also "see through"
both ($) and (.) functions on the right.

TRANSFORM PATTERNS
_eval_ - perform deep evaluation, must be used at the top of a RHS
_noParen_ - don't bracket this particular item

SIDE CONDITIONS
(&&), (||), not - boolean connectives
isAtom x - does x never need brackets
isFoo x - is the root constructor of x a "Foo"
notEq x y - are x and y not equal
notIn xs ys - are all x variables not in ys expressions
notTypeSafe - no semantics, a hint for testing only

($) AND (.)
We see through ($) simply by expanding it if nothing else matches.
We see through (.) by translating rules that have (.) equivalents
to separate rules. For example:

concat (map f x) ==> concatMap f x
-- we spot both these rules can eta reduce with respect to x
concat . map f ==> concatMap f
-- we use the associativity of (.) to add
concat . map f . x ==> concatMap f . x
-- currently 36 of 169 rules have (.) equivalents
-}

module Hint.Match(readMatch) where

import Data.List
import Data.Maybe
import Type
import Hint
import HSE.All
import Control.Monad
import Control.Arrow
import Data.Function
import Util


fmapAn = fmap (const an)


---------------------------------------------------------------------
-- READ THE RULE

readMatch :: [Setting] -> DeclHint
readMatch settings = findIdeas (concatMap readRule settings)


readRule :: Setting -> [Setting]
readRule m@MatchExp{lhs=(fmapAn -> lhs), rhs=(fmapAn -> rhs), side=(fmap fmapAn -> side)} =
    (:) m{lhs=lhs,side=side,rhs=rhs} $ fromMaybe [] $ do
        (l,v1) <- dotVersion lhs
        (r,v2) <- dotVersion rhs
        guard $ v1 == v2 && l /= [] && r /= [] && v1 `notElem` vars side
        return [m{lhs=dotApps l, rhs=dotApps r, side=side}
               ,m{lhs=dotApps (l++[toNamed v1]), rhs=dotApps (r++[toNamed v1]), side=side}]
readRule _ = []


-- find a dot version of this rule, return the sequence of app prefixes, and the var
dotVersion :: Exp_ -> Maybe ([Exp_], String)
dotVersion (view -> Var_ v) | isUnifyVar v = Just ([], v)
dotVersion (fromApps -> xs) | length xs > 1 = fmap (first (apps (init xs) :)) $ dotVersion (fromParen $ last xs)
dotVersion _ = Nothing


---------------------------------------------------------------------
-- PERFORM THE MATCHING

findIdeas :: [Setting] -> NameMatch -> Module S -> Decl_ -> [Idea]
findIdeas matches nm _ decl =
  [ idea (rankS m) (hintS m) x y
  | (parent,x) <- universeParentExp decl, not $ isParen x, let x2 = fmapAn x
  , m <- matches, Just y <- [matchIdea nm decl m parent x2]]


matchIdea :: NameMatch -> Decl_ -> Setting -> Maybe (Int, Exp_) -> Exp_ -> Maybe Exp_
matchIdea nm decl MatchExp{lhs=lhs,rhs=rhs,side=side} parent x = do
    u <- unify nm lhs x
    u <- check u
    let res = addBracket parent $ unqualify nm $ performEval $ subst u rhs
    guard $ checkSide side $ ("original",x) : ("result",res) : u
    guard $ checkDefine decl parent res
    return res


---------------------------------------------------------------------
-- UNIFICATION

-- unify a b = c, a[c] = b
-- note: App is unrolled because it's really common (performance reasons)
unify :: NameMatch -> Exp_ -> Exp_ -> Maybe [(String,Exp_)]
unify nm (Do _ xs) (Do _ ys) | length xs == length ys = concatZipWithM (unifyStmt nm) xs ys
unify nm (Lambda _ xs x) (Lambda _ ys y) | length xs == length ys = liftM2 (++) (unify nm x y) (concatZipWithM unifyPat xs ys)
unify nm x y | isParen x || isParen y = unify nm (fromParen x) (fromParen y)
unify nm (Var _ (fromNamed -> v)) y | isUnifyVar v = Just [(v,y)]
unify nm (Var _ x) (Var _ y) | nm x y = Just []
unify nm (App _ x1 x2) (App _ y1 y2) = liftM2 (++) (unify nm x1 y1) (unify nm x2 y2)
unify nm x y | isOther x && isOther y && eqExpShell x y = concatZipWithM (unify nm) (children x) (children y)
unify nm x (InfixApp _ lhs (opExp -> op) rhs)
    | op ~= "$" = unify nm x $ App an lhs rhs
    | otherwise = unify nm x $ App an (App an op lhs) rhs
unify nm _ _ = Nothing

-- types that are not already handled in unify
{-# INLINE isOther #-}
isOther Do{} = False
isOther Lambda{} = False
isOther Var{} = False
isOther App{} = False
isOther _ = True


unifyStmt :: NameMatch -> Stmt S -> Stmt S -> Maybe [(String,Exp_)]
unifyStmt nm (Generator _ p1 x1) (Generator _ p2 x2) = liftM2 (++) (unifyPat p1 p2) (unify nm x1 x2)
unifyStmt nm x y | ((==) `on` descendBi (const (toNamed "_" :: Exp_))) x y = concatZipWithM (unify nm) (childrenBi x) (childrenBi y)
unifyStmt nm _ _ = Nothing


unifyPat :: Pat_ -> Pat_ -> Maybe [(String,Exp_)]
unifyPat (PVar _ x) (PVar _ y) = Just [(fromNamed x, toNamed $ fromNamed y)]
unifyPat PWildCard{} PVar{} = Just []
unifyPat x y | ((==) `on` descend (const $ PWildCard an)) x y = concatZipWithM unifyPat (children x) (children y)
unifyPat _ _ = Nothing


---------------------------------------------------------------------
-- SUBSTITUTION UTILITIES

-- check the unification is valid
check :: [(String,Exp_)] -> Maybe [(String,Exp_)]
check = mapM f . groupSortFst
    where f (x,ys) = if length (nub ys) == 1 then Just (x,head ys) else Nothing


-- perform a substitution
subst :: [(String,Exp_)] -> Exp_ -> Exp_
subst bind = transform g . transformBracket f
    where
        f (Var _ (fromNamed -> x)) | isUnifyVar x = lookup x bind
        f _ = Nothing

        g (App _ np (Paren _ x)) | np ~= "_noParen_" = x
        g x = x


---------------------------------------------------------------------
-- SIDE CONDITIONS

checkSide :: Maybe Exp_ -> [(String,Exp_)] -> Bool
checkSide x bind = maybe True f x
    where
        f (InfixApp _ x op y)
            | opExp op ~= "&&" = f x && f y
            | opExp op ~= "||" = f x || f y
        f (App _ x y) | x ~= "not" = not $ f y
        f (Paren _ x) = f x

        f (App _ cond (sub -> y))
            | 'i':'s':typ <- fromNamed cond
            = if typ == "Atom" then isAtom y else head (words $ show y) == typ
        f (App _ (App _ cond (sub -> x)) (sub -> y))
            | cond ~= "notIn" = and [x `notElem` universe y | x <- list x, y <- list y]
            | cond ~= "notEq" = x /= y
        f x | x ~= "notTypeSafe" = True
        f x = error $ "Hint.Match.checkSide, unknown side condition: " ++ prettyPrint x

        list :: Exp_ -> [Exp_]
        list (List _ xs) = xs
        list x = [x]

        sub :: Exp_ -> Exp_
        sub = transform f
            where f (view -> Var_ x) | Just y <- lookup x bind = y
                  f x = x


-- does the result look very much like the declaration
checkDefine :: Decl_ -> Maybe (Int, Exp_) -> Exp_ -> Bool
checkDefine x Nothing y = fromNamed x /= fromNamed (transformBi unqual $ head $ fromApps y)
checkDefine _ _ _ = True


---------------------------------------------------------------------
-- TRANSFORMATION

-- if it has _eval_ do evaluation on it
performEval :: Exp_ -> Exp_
performEval (App _ e x) | e ~= "_eval_" = evaluate x
performEval x = x


-- contract Data.List.foo ==> foo, if Data.List is loaded
unqualify :: NameMatch -> Exp_ -> Exp_
unqualify nm = transformBi f
    where
        f (Qual _ mod x) | nm (Qual an mod x) (UnQual an x) = UnQual an x
        f x = x


addBracket :: Maybe (Int,Exp_) -> Exp_ -> Exp_
addBracket (Just (i,p)) c | needBracket i p c = Paren an c
addBracket _ x = x

{-# LANGUAGE PatternGuards, ViewPatterns, RelaxedPolyRec #-}

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
import Data.Data
import Unsafe.Coerce
import Type
import Hint
import HSE.All
import Control.Monad
import Control.Arrow
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
    u <- unifyExp nm lhs x
    u <- check u
    let res = addBracket parent $ unqualify nm $ performEval $ subst u rhs
    guard $ checkSide side $ ("original",x) : ("result",res) : u
    guard $ checkDefine decl parent res
    return res


---------------------------------------------------------------------
-- UNIFICATION

-- unify a b = c, a[c] = b
unify :: Data a => NameMatch -> a -> a -> Maybe [(String,Exp_)]
unify nm x y | Just x <- cast x = unifyExp nm x (unsafeCoerce y)
             | Just x <- cast x = unifyPat nm x (unsafeCoerce y)
             | otherwise = unifyDef nm x y


unifyDef :: Data a => NameMatch -> a -> a -> Maybe [(String,Exp_)]
unifyDef nm x y = fmap concat . sequence =<< gzip (unify nm) x y


-- App/InfixApp are analysed specially for performance reasons
unifyExp :: NameMatch -> Exp_ -> Exp_ -> Maybe [(String,Exp_)]
unifyExp nm x y | isParen x || isParen y = unifyExp nm (fromParen x) (fromParen y)
unifyExp nm (Var _ (fromNamed -> v)) y | isUnifyVar v = Just [(v,y)]
unifyExp nm (Var _ x) (Var _ y) | nm x y = Just []
unifyExp nm (App _ x1 x2) (App _ y1 y2) = liftM2 (++) (unifyExp nm x1 y1) (unifyExp nm x2 y2)
unifyExp nm x (InfixApp _ lhs2 op2 rhs2)
    | InfixApp _ lhs1 op1 rhs1 <- x = guard (op1 == op2) >> liftM2 (++) (unifyExp nm lhs1 lhs2) (unifyExp nm rhs1 rhs2)
    | isDol op2 = unifyExp nm x $ App an lhs2 rhs2
    | otherwise = unifyExp nm x $ App an (App an (opExp op2) lhs2) rhs2
unifyExp nm x y | isOther x, isOther y = unifyDef nm x y
unifyExp nm _ _ = Nothing


unifyPat :: NameMatch -> Pat_ -> Pat_ -> Maybe [(String,Exp_)]
unifyPat nm (PVar _ x) (PVar _ y) = Just [(fromNamed x, toNamed $ fromNamed y)]
unifyPat nm PWildCard{} PVar{} = Just []
unifyPat nm x y = unifyDef nm x y 


-- types that are not already handled in unify
{-# INLINE isOther #-}
isOther Var{} = False
isOther App{} = False
isOther InfixApp{} = False
isOther _ = True


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

        g (App _ np x) | np ~= "_noParen_" = fromParen x
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

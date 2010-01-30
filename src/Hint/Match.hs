{-# LANGUAGE PatternGuards, ViewPatterns #-}

{-
Supported meta-hints:

_eval_ - perform deep evaluation, must be used at the top of a RHS
_noParen_ - don't bracket this particular item
-}

module Hint.Match(readMatch) where

import Data.List
import Data.Maybe
import Type
import Hint
import HSE.All
import Control.Monad
import Data.Function
import Util


---------------------------------------------------------------------
-- PERFORM MATCHING

fmapAn = fmap (const an)

readMatch :: [Setting] -> DeclHint
readMatch settings = findIdeas [m{lhs = fmapAn $ lhs m, side = fmap fmapAn $ side m} | m@MatchExp{} <- settings]


findIdeas :: [Setting] -> NameMatch -> Module S -> Decl_ -> [Idea]
findIdeas matches nm _ decl =
  [ idea (rankS m) (hintS m) x y
  | (parent,x) <- universeParentExp decl, not $ isParen x, let x2 = fmapAn x
  , m <- matches, Just y <- [matchIdea nm decl m parent x2]]


matchIdea :: NameMatch -> Decl_ -> Setting -> Maybe (Int, Exp_) -> Exp_ -> Maybe Exp_
matchIdea nm decl MatchExp{lhs=lhs,rhs=rhs,side=side} parent x = do
    u <- unify nm lhs x
    u <- check u
    let sub = subst u rhs
    guard $ checkDot lhs sub
    let res = addBracket parent $ unqualify nm $ dotContract $ performEval sub
    guard $ checkSide side $ ("original",x) : ("result",res) : u
    guard $ checkDefine decl parent res
    return res


-- unify a b = c, a[c] = b
-- note: App is unrolled because it's really common
unify :: NameMatch -> Exp_ -> Exp_ -> Maybe [(String,Exp_)]
unify nm (Do _ xs) (Do _ ys) | length xs == length ys = concatZipWithM (unifyStmt nm) xs ys
unify nm (Lambda _ xs x) (Lambda _ ys y) | length xs == length ys = liftM2 (++) (unify nm x y) (concatZipWithM unifyPat xs ys)
unify nm x y | isParen x || isParen y = unify nm (fromParen x) (fromParen y)
unify nm (Var _ (fromNamed -> v)) y | isUnifyVar v = Just [(v,y)]
unify nm (Var _ x) (Var _ y) | nm x y = Just []
unify nm (App _ x1 x2) (App _ y1 y2) = liftM2 (++) (unify nm x1 y1) (unify nm x2 y2)
unify nm x y | isOther x && isOther y && eqExpShell x y = concatZipWithM (unify nm) (children x) (children y)
unify nm x o@(view -> App2 op y1 y2)
  | op ~= "$" = unify nm x $ App an y1 y2
  | op ~= "." = unify nm x $ dotExpand o
unify nm x (InfixApp _ lhs op rhs) = unify nm x $ App an (App an (opExp op) lhs) rhs
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


-- check the unification is valid
check :: [(String,Exp_)] -> Maybe [(String,Exp_)]
check = mapM f . groupSortFst
    where f (x,ys) = if length (nub ys) == 1 then Just (x,head ys) else Nothing


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


-- If they have have a lambda in the pattern
-- don't allow dot contraction to happen, as it's usually wrong
checkDot :: Exp_ -> Exp_ -> Bool
checkDot lhs rhs2 = not $ any isLambda (universeS lhs) && toNamed "?" `elem` universe rhs2


-- does the result look very much like the declaration
checkDefine :: Decl_ -> Maybe (Int, Exp_) -> Exp_ -> Bool
checkDefine x Nothing y = fromNamed x /= fromNamed (transformBi unqual $ head $ fromApps y)
checkDefine _ _ _ = True


-- perform a substitution
subst :: [(String,Exp_)] -> Exp_ -> Exp_
subst bind = transform g . transformBracket f
    where
        f (Var _ (fromNamed -> x)) | isUnifyVar x = lookup x bind
        f _ = Nothing

        g (App _ np (Paren _ x)) | np ~= "_noParen_" = x
        g x = x


dotExpand :: Exp_ -> Exp_
dotExpand (view -> App2 op x1 x2) | op ~= "." = ensureBracket1 $ App an x1 (dotExpand x2)
dotExpand x = ensureBracket1 $ App an x (toNamed "?")


-- simplify, removing any introduced ? vars, from expanding (.)
dotContract :: Exp_ -> Exp_
dotContract x = fromMaybe x (f x)
    where
        f x | isParen x = f $ fromParen x
        f (App _ x y) | "?" <- fromNamed y = Just x
                      | Just z <- f y = Just $ InfixApp an x (toNamed ".") z
        f _ = Nothing

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

{-# LANGUAGE PatternGuards, ViewPatterns #-}

{-
Supported meta-hints:

_eval_ - perform deep evaluation, must be used at the top of a RHS
_noParen_ - don't bracket this particular item
-}

module Hint.Match(readMatch) where

import Data.Char
import Data.List
import Data.Maybe
import Type
import Hint
import HSE.All
import Control.Monad
import Data.Function
import Util
import HSE.Evaluate(evaluate)


---------------------------------------------------------------------
-- PERFORM MATCHING

readMatch :: [Setting] -> DeclHint
readMatch = findIdeas . filter isMatchExp


findIdeas :: [Setting] -> NameMatch -> Module S -> Decl_ -> [Idea]
findIdeas matches nm _ decl =
  [ idea (rankS m) (hintS m) x y
  | x <- universeBi decl, not $ isParen x
  , m <- matches, Just y <- [matchIdea nm m x]]


matchIdea :: NameMatch -> Setting -> Exp_ -> Maybe Exp_
matchIdea nm MatchExp{lhs=lhs,rhs=rhs,side=side} x = do
    u <- unify nm (fmap (const an) lhs) (fmap (const an) x)
    u <- check u
    guard $ checkSide side u
    let rhs2 = subst u rhs
    guard $ checkDot lhs rhs2
    return $ unqualify nm $ dotContract $ performEval rhs2


-- unify a b = c, a[c] = b
unify :: NameMatch -> Exp_ -> Exp_ -> Maybe [(String,Exp_)]
unify nm (Do _ xs) (Do _ ys) | length xs == length ys = concatZipWithM (unifyStmt nm) xs ys
unify nm (Lambda _ xs x) (Lambda _ ys y) | length xs == length ys = liftM2 (++) (unify nm x y) (concatZipWithM unifyPat xs ys)
unify nm x y | isParen x || isParen y = unify nm (fromParen x) (fromParen y)
unify nm (Var _ (fromNamed -> v)) y | isUnifyVar v = Just [(v,y)]
unify nm (Var _ x) (Var _ y) | nm x y = Just []
unify nm x y | ((==) `on` descend (const $ toNamed "_")) x y = concatZipWithM (unify nm) (children x) (children y)
unify nm x o@(view -> App2 op y1 y2)
  | op ~= "$" = unify nm x $ App an y1 y2
  | op ~= "." = unify nm x $ dotExpand o
unify nm x (InfixApp _ lhs op rhs) = unify nm x $ App an (App an (opExp op) lhs) rhs
unify nm _ _ = Nothing


unifyStmt :: NameMatch -> Stmt S -> Stmt S -> Maybe [(String,Exp_)]
unifyStmt nm (Generator _ p1 x1) (Generator _ p2 x2) = liftM2 (++) (unifyPat p1 p2) (unify nm x1 x2)
unifyStmt nm x y | ((==) `on` descendBi (const (toNamed "_" :: Exp_))) x y = concatZipWithM (unify nm) (childrenBi x) (childrenBi y)
unifyStmt nm _ _ = Nothing


unifyPat :: Pat_ -> Pat_ -> Maybe [(String,Exp_)]
unifyPat (PVar _ x) (PVar _ y) = Just [(fromNamed x, toNamed $ fromNamed y)]
unifyPat PWildCard{} PVar{} = Just []
unifyPat x y | ((==) `on` descend (const $ PWildCard an)) x y = concatZipWithM unifyPat (children x) (children y)
unifyPat _ _ = Nothing


concatZipWithM f xs = liftM concat . zipWithM f xs


-- check the unification is valid
check :: [(String,Exp_)] -> Maybe [(String,Exp_)]
check = mapM f . groupSortFst
    where f (x,ys) = if length (nub ys) == 1 then Just (x,head ys) else Nothing


checkSide :: Maybe Exp_ -> [(String,Exp_)] -> Bool
checkSide Nothing  bind = True
checkSide (Just x) bind = f x
    where
        f (InfixApp _ x op y)
            | opExp op ~= "&&" = f x && f y
            | opExp op ~= "||" = f x || f y
        f (Paren _ x) = f x
        f (App _ x (Var _ y))
            | 'i':'s':typ <- fromNamed x, Just e <- lookup (fromNamed y) bind
            = if typ == "Atom" then isAtom e
              else head (words $ show e) == typ
        f (App _ (App _ nin xs) ys) | nin ~= "notIn" = and [notIn x y | x <- g xs, y <- g ys]
        f x = error $ "Hint.Match.checkSide, unknown side condition: " ++ prettyPrint x

        g :: Exp_ -> [Exp_]
        g (List _ xs) = xs
        g x = [x]

        notIn x y = fromMaybe False $ do
            x2 <- lookup (fromNamed x) bind
            y2 <- lookup (fromNamed y) bind
            return $ x2 `notElem` universe y2


-- If they have have a lambda in the pattern
-- don't allow dot contraction to happen, as it's usually wrong
checkDot :: Exp_ -> Exp_ -> Bool
checkDot lhs rhs2 = not $ any isLambda (universeS lhs) && toNamed "?" `elem` universe rhs2


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


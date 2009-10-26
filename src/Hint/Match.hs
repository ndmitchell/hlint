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
import HSE.All
import Control.Monad
import Data.Function
import HSE.Evaluate(evaluate)


---------------------------------------------------------------------
-- PERFORM MATCHING

readMatch :: [Setting] -> DeclHint
readMatch = findIdeas . filter isMatchExp


findIdeas :: [Setting] -> NameMatch -> Decl -> [Idea]
findIdeas matches nm decl =
  [ idea (rankS m) (hintS m) loc x y
  | (loc, x) <- universeExp nullSrcLoc decl, not $ isParen x
  , m <- matches, Just y <- [matchIdea nm m x]]


matchIdea :: NameMatch -> Setting -> Exp -> Maybe Exp
matchIdea nm MatchExp{lhs=lhs,rhs=rhs,side=side} x = do
    u <- unify nm lhs x
    u <- check u
    guard $ checkSide side u
    return $ unqualify nm $ dotContract $ performEval $ subst u rhs


-- unify a b = c, a[c] = b
unify :: NameMatch -> Exp -> Exp -> Maybe [(String,Exp)]
unify nm (Do xs) (Do ys) | length xs == length ys = concatZipWithM (unifyStmt nm) xs ys
unify nm (Lambda _ xs x) (Lambda _ ys y) | length xs == length ys = liftM2 (++) (unify nm x y) (concatZipWithM unifyPat xs ys)
unify nm x y | isParen x || isParen y = unify nm (fromParen x) (fromParen y)
unify nm (Var (fromNamed -> v)) y | isUnifyVar v = Just [(v,y)]
unify nm (Var x) (Var y) | nm x y = Just []
unify nm x y | ((==) `on` descend (const $ toNamed "_")) x y = concatZipWithM (unify nm) (children x) (children y)
unify nm x o@(view -> App2 op y1 y2)
  | op ~= "$" = unify nm x $ y1 `App` y2
  | op ~= "." = unify nm x $ dotExpand o
unify nm x (InfixApp lhs op rhs) = unify nm x (opExp op `App` lhs `App` rhs)
unify nm _ _ = Nothing


unifyStmt :: NameMatch -> Stmt -> Stmt -> Maybe [(String,Exp)]
unifyStmt nm (Generator _ p1 x1) (Generator _ p2 x2) = liftM2 (++) (unifyPat p1 p2) (unify nm x1 x2)
unifyStmt nm x y | ((==) `on` descendBi (const (toNamed "_" :: Exp))) x y = concatZipWithM (unify nm) (childrenBi x) (childrenBi y)
unifyStmt nm _ _ = Nothing


unifyPat :: Pat -> Pat -> Maybe [(String,Exp)]
unifyPat (PVar x) (PVar y) = Just [(fromNamed x, toNamed $ fromNamed y)]
unifyPat PWildCard (PVar _) = Just []
unifyPat x y | ((==) `on` descend (const PWildCard)) x y = concatZipWithM unifyPat (children x) (children y)
unifyPat _ _ = Nothing


concatZipWithM f xs = liftM concat . zipWithM f xs


-- check the unification is valid
check :: [(String,Exp)] -> Maybe [(String,Exp)]
check = mapM f . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    where f xs = if length (nub xs) == 1 then Just (head xs) else Nothing


checkSide :: Maybe Exp -> [(String,Exp)] -> Bool
checkSide Nothing  bind = True
checkSide (Just x) bind = f x
    where
        f (InfixApp x op y)
            | opExp op ~= "&&" = f x && f y
            | opExp op ~= "||" = f x || f y
        f (Paren x) = f x
        f (App x (Var y))
            | 'i':'s':typ <- fromNamed x, Just e <- lookup (fromNamed y) bind
            = if typ == "Atom" then isAtom e
              else head (words $ show e) == typ
        f (App (App nin xs) ys) | nin ~= "notIn" = and [notIn x y | x <- g xs, y <- g ys]
        f x = error $ "Hint.Match.checkSide, unknown side condition: " ++ prettyPrint x

        g :: Exp -> [Exp]
        g (List xs) = xs
        g x = [x]

        notIn x y = fromMaybe False $ do
            x2 <- lookup (fromNamed x) bind
            y2 <- lookup (fromNamed y) bind
            return $ x2 `notElem` universe y2


-- perform a substitution
subst :: [(String,Exp)] -> Exp -> Exp
subst bind = transform g . transformBracket f
    where
        f (Var (fromNamed -> x)) | isUnifyVar x = lookup x bind
        f _ = Nothing

        g (App np (Paren x)) | np ~= "_noParen_" = x
        g x = x


dotExpand :: Exp -> Exp
dotExpand (view -> App2 op x1 x2) | op ~= "." = ensureBracket1 $ App x1 (dotExpand x2)
dotExpand x = ensureBracket1 $ App x (toNamed "?")


-- simplify, removing any introduced ? vars, from expanding (.)
dotContract :: Exp -> Exp
dotContract x = fromMaybe x (f x)
    where
        f x | isParen x = f $ fromParen x
        f (App x y) | "?" <- fromNamed y = Just x
                    | Just z <- f y = Just $ InfixApp x (QVarOp $ UnQual $ Symbol ".") z
        f _ = Nothing

-- if it has _eval_ do evaluation on it
performEval :: Exp -> Exp
performEval (App e x) | e ~= "_eval_" = evaluate x
performEval x = x


-- contract Data.List.foo ==> foo, if Data.List is loaded
unqualify :: NameMatch -> Exp -> Exp
unqualify nm = transformBi f
    where
        f (Qual mod x) | nm (Qual mod x) (UnQual x) = UnQual x
        f x = x


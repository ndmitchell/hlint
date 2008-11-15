{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Hint.Match(readMatch) where

import Language.Haskell.Exts
import Data.Char
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Type
import Util
import Control.Monad
import Data.Function
import Debug.Trace


data Mat = Mat {message :: String, lhs :: Exp, rhs :: Exp}

instance Show Mat where
    show (Mat x y z) = "Match " ++ show x ++ "\n  " ++ prettyPrint y ++ "\n  " ++ prettyPrint z ++ "\n"
    showList = showString . concat . map show

-- Any 1-letter variable names are assumed to be unification variables
isFreeVar :: String -> Bool
isFreeVar [x] = x == '?' || isAlpha x
isFreeVar _ = False


---------------------------------------------------------------------
-- READ THE MATCHES

readMatch :: Module -> Hint
readMatch modu = findIdeas (concatMap readOne $ childrenBi modu)



readOne :: Decl -> [Mat]
readOne (FunBind [Match src (Ident "hint") [PLit (String msg)]
           (UnGuardedRhs (InfixApp lhs (QVarOp (UnQual (Symbol "==>"))) rhs)) (BDecls [])]) =
        [Mat (if null msg then pickName lhs rhs else msg) lhs rhs]

readOne (PatBind src (PVar name) bod bind) = readOne $ FunBind [Match src name [PLit (String "")] bod bind]

readOne (FunBind xs) = concatMap (readOne . FunBind . (:[])) xs

readOne x = error $ "Failed to read hint " ++ maybe "" showSrcLoc (getSrcLoc x) ++ "\n" ++ prettyPrint x


pickName :: Exp -> Exp -> String
pickName lhs rhs | null names = "Unnamed suggestion"
                 | otherwise = "Use " ++ head names
    where
        names = filter (not . isFreeVar) $ map f (childrenBi rhs) \\ map f (childrenBi lhs) 
        f (Ident x) = x
        f (Symbol x) = x


---------------------------------------------------------------------
-- PERFORM MATCHING

findIdeas :: [Mat] -> Decl -> [Idea]
findIdeas matches decl =
  [ idea (message m) loc x y
  | (loc, x) <- universeExp nullSrcLoc decl, not $ isParen x
  , m <- matches, Just y <- [matchIdea m x]]


matchIdea :: Mat -> Exp -> Maybe Exp
matchIdea Mat{lhs=lhs,rhs=rhs} x = do
    u <- unify lhs x
    u <- check u
    return $ simp $ remParen $ subst u rhs


-- unify a b = c, a[c] = b
unify :: Exp -> Exp -> Maybe [(String,Exp)]
unify x y | Just v <- fromVar x, isFreeVar v = Just [(v,addParen y)]
unify x y | ((==) `on` descend (const $ toVar "_")) x y = liftM concat $ zipWithM unify (children x) (children y)
unify x y | isParen x || isParen y = unify (fromParen x) (fromParen y)
unify x (view -> App2 op y1 y2)
  | op ~= "$" = unify x $ addParen y1 `App` addParen y2
  | op ~= "." = unify x $ App (addParen y1) (addParen y2 `App` toVar "?")
unify _ _ = Nothing


-- check the unification is valid
check :: [(String,Exp)] -> Maybe [(String,Exp)]
check = mapM f . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    where f xs = if length (nub xs) == 1 then Just (head xs) else Nothing


-- perform a substitution
subst :: [(String,Exp)] -> Exp -> Exp
subst bind x | Just v <- fromVar x, isFreeVar v, Just y <- lookup v bind = y
             | otherwise = descend (subst bind) x


-- simplify, removing any introduced ? vars (from expanding .)
simp :: Exp -> Exp
simp (App x y) | Just "?" <- fromVar y = x
simp x = x

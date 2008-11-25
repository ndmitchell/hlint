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


data Mat = Mat {message :: String, lhs :: Exp, rhs :: Exp, side :: Maybe Exp}

instance Show Mat where
    show (Mat x y z q) = unlines $ ("Match " ++ show x) :
        map (\x -> "  " ++ prettyPrint x) ([y,z] ++ maybeToList q)

    showList = showString . concatMap show

-- Any 1-letter variable names are assumed to be unification variables
isFreeVar :: String -> Bool
isFreeVar [x] = x == '?' || isAlpha x
isFreeVar _ = False


---------------------------------------------------------------------
-- READ THE MATCHES

readMatch :: Module -> Hint
readMatch = findIdeas . concatMap readOne . childrenBi



readOne :: Decl -> [Mat]
readOne (FunBind [Match src (Ident "hint") [PLit (String msg)]
           (UnGuardedRhs (InfixApp lhs (QVarOp (UnQual (Symbol "==>"))) rhs)) (BDecls bind)]) =
        [Mat (if null msg then pickName lhs rhs else msg) (fromParen lhs) (fromParen rhs) (readSide bind)]

readOne (PatBind src (PVar name) bod bind) = readOne $ FunBind [Match src name [PLit (String "")] bod bind]

readOne (FunBind xs) | length xs /= 1 = concatMap (readOne . FunBind . (:[])) xs

readOne x = error $ "Failed to read hint " ++ maybe "" showSrcLoc (getSrcLoc x) ++ "\n" ++ prettyPrint x


readSide :: [Decl] -> Maybe Exp
readSide [] = Nothing
readSide [PatBind src PWildCard (UnGuardedRhs bod) (BDecls [])] = Just bod
readSide (x:_) = error $ "Failed to read side condition " ++ maybe "" showSrcLoc (getSrcLoc x) ++ "\n" ++ prettyPrint x


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
    return $ remParen $ dotContract $ subst u rhs


-- unify a b = c, a[c] = b
unify :: Exp -> Exp -> Maybe [(String,Exp)]
unify x y | isParen x || isParen y = unify (fromParen x) (fromParen y)
unify x y | Just v <- fromVar x, isFreeVar v = Just [(v,addParen y)]
unify x y | ((==) `on` descend (const $ toVar "_")) x y = liftM concat $ zipWithM unify (children x) (children y)
unify x o@(view -> App2 op y1 y2)
  | op ~= "$" = unify x $ addParen y1 `App` addParen y2
  | op ~= "." = unify x $ dotExpand o
unify _ _ = Nothing


-- check the unification is valid
check :: [(String,Exp)] -> Maybe [(String,Exp)]
check = mapM f . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    where f xs = if length (nub xs) == 1 then Just (head xs) else Nothing


-- perform a substitution
subst :: [(String,Exp)] -> Exp -> Exp
subst bind x | Just v <- fromVar x, isFreeVar v, Just y <- lookup v bind = y
             | otherwise = descend (subst bind) x


dotExpand :: Exp -> Exp
dotExpand (view -> App2 op x1 x2) | op ~= "." = App (addParen x1) (addParen $ dotExpand x2)
dotExpand x = addParen x `App` toVar "?"


-- simplify, removing any introduced ? vars, from expanding (.)
dotContract :: Exp -> Exp
dotContract x = fromMaybe x (f x)
    where
        f x | isParen x = f $ fromParen x
        f (App x y) | Just "?" <- fromVar y = Just x
                    | Just z <- f y = Just $ InfixApp x (QVarOp $ UnQual $ Symbol ".") z
        f _ = Nothing

{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Hint.Match(readMatch) where

import Language.Haskell.Exts
import Data.Char
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Hint.Type
import Hint.Util
import Control.Monad
import Data.Function
import Debug.Trace


data Match = Match {message :: String, lhs :: HsExp, rhs :: HsExp}

instance Show Match where
    show (Match x y z) = "Match " ++ show x ++ "\n  " ++ prettyPrint y ++ "\n  " ++ prettyPrint z ++ "\n"
    showList = showString . concat . map show

-- Any 1-letter variable names are assumed to be unification variables
isFreeVar :: String -> Bool
isFreeVar [x] = x == '?' || isAlpha x
isFreeVar _ = False


---------------------------------------------------------------------
-- READ THE MATCHES

readMatch :: HsModule -> Hint
readMatch modu = findIdeas (concatMap readOne $ childrenBi modu)



readOne :: HsDecl -> [Match]
readOne (HsFunBind [HsMatch src (HsIdent "hint") [HsPLit (HsString msg)]
           (HsUnGuardedRhs (HsInfixApp lhs (HsQVarOp (UnQual (HsSymbol "==>"))) rhs)) (HsBDecls [])]) =
        [Match (ifNull msg (pickName lhs rhs)) lhs rhs]

readOne (HsPatBind src (HsPVar name) bod bind) = readOne $ HsFunBind [HsMatch src name [HsPLit (HsString "")] bod bind]

readOne (HsFunBind xs) = concatMap (readOne . HsFunBind . (:[])) xs

readOne x = error $ "Failed to read hint " ++ maybe "" showSrcLoc (getSrcLoc x) ++ "\n" ++ prettyPrint x


pickName :: HsExp -> HsExp -> String
pickName lhs rhs | null names = "Unnamed suggestion"
                 | otherwise = "Use " ++ head names
    where
        names = filter (not . isFreeVar) $ map f (childrenBi rhs) \\ map f (childrenBi lhs) 
        f (HsIdent x) = x
        f (HsSymbol x) = x


---------------------------------------------------------------------
-- PERFORM MATCHING

findIdeas :: [Match] -> HsDecl -> [Idea]
findIdeas matches decl =
  [ idea (message m) loc x y
  | (loc, x) <- universeExp nullSrcLoc decl, not $ isParen x
  , m <- matches, Just y <- [matchIdea m x]]


matchIdea :: Match -> HsExp -> Maybe HsExp
matchIdea Match{lhs=lhs,rhs=rhs} x = do
    u <- unify lhs x
    u <- check u
    return $ simp $ remParen $ subst u rhs


-- unify a b = c, a[c] = b
unify :: HsExp -> HsExp -> Maybe [(String,HsExp)]
unify x y | Just v <- fromVar x, isFreeVar v = Just [(v,addParen y)]
unify x y | ((==) `on` descend (const HsWildCard)) x y = liftM concat $ zipWithM unify (children x) (children y)
unify (HsParen x) y = unify x y
unify x (HsParen y) = unify x y
unify x (view -> App2 op y1 y2)
  | op ~= "$" = unify x $ addParen y1 `HsApp` addParen y2
  | op ~= "." = unify x $ HsApp (addParen y1) (addParen y2 `HsApp` toVar "?")
unify _ _ = Nothing


-- check the unification is valid
check :: [(String,HsExp)] -> Maybe [(String,HsExp)]
check = mapM f . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    where f xs = if length (nub xs) == 1 then Just (head xs) else Nothing


-- perform a substitution
subst :: [(String,HsExp)] -> HsExp -> HsExp
subst bind x | Just v <- fromVar x, isFreeVar v, Just y <- lookup v bind = y
             | otherwise = descend (subst bind) x


-- simplify, removing any introduced ? vars (from expanding .)
simp :: HsExp -> HsExp
simp (HsApp x y) | Just "?" <- fromVar y = x
simp x = x

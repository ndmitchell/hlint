{-# LANGUAGE PatternGuards, ViewPatterns #-}

{-
map f [] = []
map f (x:xs) = f x : map f xs

filter f [] = []
filter f (x:xs) = if f x then x : filter f xs else filter f xs

foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
-}


module Hint.ListRec(listRecHint) where

import Type
import Util
import HSE.All
import Data.Maybe
import Control.Monad
import Data.Generics.PlateData


listRecHint :: Decl -> [Idea]
listRecHint = concatMap f . universe
    where
        f o = maybeToList $ do
            x <- findAnalysis o
            x <- matchRecDecl x
            let loc = headDef nullSrcLoc $ universeBi o
            return $ idea Error "Use recursion" loc o x


---------------------------------------------------------------------
-- MATCH THE RECURSION

matchRecDecl :: (Name, [Maybe Name], Exp, (Name,Name,Exp)) -> Maybe Decl
matchRecDecl (a,b,c,d@(_,xs,_)) = do
    bod <- matchRecExp a b c d
    return $ FunBind [Match nullSrcLoc a [PVar xs] (UnGuardedRhs bod) (BDecls [])]


matchRecExp :: Name -> [Maybe Name] -> Exp -> (Name,Name,Exp) -> Maybe Exp
matchRecExp name ps nil (x,xs,cons)

    -- try for map
    | [Nothing] <- ps, nil ~= "[]", InfixApp lhs c rhs <- cons, opExp c ~= ":"
    , rhs == App (Var $ UnQual name) (Var $ UnQual xs), Var (UnQual xs) `notElem` universe lhs
    = Just $ apps [toVar "map", extract [x] lhs, Var $ UnQual xs]

    | otherwise = Nothing


---------------------------------------------------------------------
-- UTILITY FUNCTIONS

-- a list of application, with any necessary brackets
apps :: [Exp] -> Exp
apps = foldl1 (\x y -> ensureBracket1 $ App x y)


-- generate a lambda, but prettier (if possible)
extract :: [Name] -> Exp -> Exp
extract [x] (InfixApp a op b)
    | a == Var (UnQual x) = RightSection op b
    | b == Var (UnQual x) = LeftSection a op
extract ps x = Lambda nullSrcLoc (map PVar ps) x



---------------------------------------------------------------------
-- FIND THE CASE ANALYSIS

-- Return: function name, arguments, nil-case, (x,xs,cons-case)
-- argument = Nothing, means it is the recursion variable
findAnalysis :: Decl -> Maybe (Name, [Maybe Name], Exp, (Name,Name,Exp))
findAnalysis (FunBind
    [Match _ name1 (readPat -> Just (p1,ps1)) (UnGuardedRhs bod1) (BDecls [])
    ,Match _ name2 (readPat -> Just (p2,ps2)) (UnGuardedRhs bod2) (BDecls [])])
    | name1 == name2 && ps1 == ps2 && isJust p1 /= isJust p2
    = Just (name1, ps1, nil, cons)
    where
        (nil,cons) | Just (x,xs) <- p1 = (bod2, (x,xs,bod1))
                   | Just (x,xs) <- p2 = (bod1, (x,xs,bod2))

findAnalysis _ = Nothing


-- Read a list of patterns, on success return Just (cons, pats)
-- where pat = Nothing, means it is either [] or (x:xs)
-- and the first is Nothing = [], Just (x,xs) = (x:xs)
readPat :: [Pat] -> Maybe (Maybe (Name,Name), [Maybe Name])
readPat = f Nothing
    where
        f (Just list) [] = Just (list, [])
        
        f list (PVar x:xs) = do
            (list,ps) <- f list xs
            return (list, Just x:ps)

        f Nothing (x:xs) | Just list <- readPatList x = do
            (list,ps) <- f (Just list) xs
            return (list, Nothing:ps)

        f _ _ = Nothing


-- [] = Just Nothing, (x:xs) = Just (Just (x,xs))
readPatList :: Pat -> Maybe (Maybe (Name,Name))
readPatList (PParen (PInfixApp (PVar x) (Special Cons) (PVar xs))) = Just $ Just (x,xs)
readPatList (PList []) = Just Nothing
readPatList _ = Nothing

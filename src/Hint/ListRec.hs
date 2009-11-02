{-# LANGUAGE PatternGuards, ViewPatterns #-}

{-
map f [] = []
map f (x:xs) = f x : map f xs

foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
-}

{-
<TEST>
f (x:xs) = negate x + f xs ; f [] = 0 -- f xs = foldr ((+) . negate) 0 xs
f (x:xs) = x + 1 : f xs ; f [] = [] -- f xs = map (+ 1) xs
f z (x:xs) = f (z*x) xs ; f z [] = z -- f z xs = foldl (*) z xs
f a (x:xs) b = x + a + b : f a xs b ; f a [] b = [] -- f a xs b = map (\ x -> x + a + b) xs
f [] a = return a ; f (x:xs) a = a + x >>= \fax -> f xs fax -- f xs a = foldM (+) a xs
</TEST>
-}


module Hint.ListRec(listRecHint) where

import Type
import Util
import HSE.All
import Data.List
import Data.Maybe
import Data.Ord
import Data.Either
import Control.Monad
import Data.Generics.PlateData


listRecHint :: DeclHint
listRecHint _ = concatMap f . universe
    where
        f o = maybeToList $ do
            let x = o
            (x, addCase) <- findCase x
            (use,rank,x) <- matchListRec x
            let res = addCase x
                loc = headDef nullSrcLoc $ universeBi o
            return $ idea rank ("Use " ++ use) loc o res


recursive = toNamed "_recursive_"

-- recursion parameters, nil-case, (x,xs,cons-case)
-- for cons-case delete any recursive calls with xs from them
-- any recursive calls are marked "_recursive_"
data ListCase = ListCase [Name] Exp (Name,Name,Exp)
                deriving Show


data BList = BNil | BCons Name Name
             deriving (Eq,Ord,Show)

-- function name, parameters, list-position, list-type, body (unmodified)
data Branch = Branch Name [Name] Int BList Exp
              deriving Show



---------------------------------------------------------------------
-- MATCH THE RECURSION


matchListRec :: ListCase -> Maybe (String,Rank,Exp)
matchListRec o@(ListCase vars nil (x,xs,cons))
    
    | [] <- vars, nil ~= "[]", InfixApp lhs c rhs <- cons, opExp c ~= ":"
    , fromParen rhs == recursive, Var (UnQual xs) `notElem` universe lhs
    = Just $ (,,) "map" Error $ appsBracket
        [toNamed "map", lambda [x] lhs, Var $ UnQual xs]

    | [] <- vars, App2 op lhs rhs <- view cons
    , null $ universe op `intersect` [Var (UnQual x), Var (UnQual xs)]
    , fromParen rhs == recursive, Var (UnQual xs) `notElem` universe lhs
    = Just $ (,,) "foldr" Warning $ appsBracket
        [toNamed "foldr", lambda [x] $ appsBracket [op,lhs], nil, Var $ UnQual xs]

    | [v] <- vars, Var (UnQual v) == nil, App r lhs <- cons, r == recursive
    , Var (UnQual xs) `notElem` universe lhs
    = Just $ (,,) "foldl" Warning $ appsBracket
        [toNamed "foldl", lambda [v,x] lhs, Var $ UnQual v, Var $ UnQual xs]

    | [v] <- vars, App ret res <- nil, ret ~= "return", res ~= "()" || res == Var (UnQual v)
    , [Generator _ (PVar b1) e, Qualifier (fromParen -> App r (Var (UnQual b2)))] <- asDo cons
    , b1 == b2, r == recursive, Var (UnQual xs) `notElem` universe e
    , name <- "foldM" ++ ['_'|res ~= "()"]
    = Just $ (,,) name Warning $ appsBracket
        [toNamed name, lambda [v,x] e, Var $ UnQual v, Var $ UnQual xs]

    | otherwise = Nothing


-- Very limited attempt to convert >>= to do, only useful for foldM/foldM_
asDo :: Exp -> [Stmt]
asDo (view -> App2 bind lhs (Lambda src [v] rhs)) = [Generator src v lhs, Qualifier rhs]
asDo (Do x) = x
asDo x = [Qualifier x]

---------------------------------------------------------------------
-- FIND THE CASE ANALYSIS

findCase :: Decl -> Maybe (ListCase, Exp -> Decl)
findCase x = do
    FunBind [x1,x2] <- return x
    Branch name1 ps1 p1 c1 b1 <- findBranch x1
    Branch name2 ps2 p2 c2 b2 <- findBranch x2
    guard (name1 == name2 && ps1 == ps2 && p1 == p2)
    [(BNil, b1), (BCons x xs, b2)] <- return $ sortBy (comparing fst) [(c1,b1), (c2,b2)]
    b2 <- transformAppsM (delCons name1 p1 xs) b2
    (ps,b2) <- return $ eliminateArgs ps1 b2

    let ps12 = let (a,b) = splitAt p1 ps1 in map PVar $ a ++ xs : b
    return (ListCase ps b1 (x,xs,b2)
           ,\e -> FunBind [Match nullSrcLoc name1 ps12 Nothing (UnGuardedRhs e) (BDecls [])])


delCons :: Name -> Int -> Name -> Exp -> Maybe Exp
delCons func pos var (fromApps -> Var (UnQual x):xs) | func == x = do
    (pre,Var (UnQual v):post) <- return $ splitAt pos xs
    guard $ v == var
    return $ apps $ recursive : pre ++ post
delCons _ _ _ x = return x


eliminateArgs :: [Name] -> Exp -> ([Name], Exp)
eliminateArgs ps cons = (remove ps, transform f cons)
    where
        args = [zs | z:zs <- map fromApps $ universeApps cons, z == recursive]
        elim = [all (\xs -> length xs > i && xs !! i == Var (UnQual p)) args | (i,p) <- zip [0..] ps] ++ repeat False
        remove = concat . zipWith (\b x -> [x | not b]) elim

        f (fromApps -> x:xs) | x == recursive = apps $ x : remove xs
        f x = x


---------------------------------------------------------------------
-- FIND A BRANCH

findBranch :: Match -> Maybe Branch
findBranch x = do
    Match _ name ps Nothing (UnGuardedRhs bod) (BDecls []) <- return x
    (a,b,c) <- findPat ps
    return $ Branch name a b c bod


findPat :: [Pat] -> Maybe ([Name], Int, BList)
findPat ps = do
    ps <- mapM readPat ps
    [i] <- return $ findIndices isRight ps
    let (left,[right]) = partitionEithers ps
    return (left, i, right)


readPat :: Pat -> Maybe (Either Name BList)
readPat (PVar x) = Just $ Left x
readPat (PParen (PInfixApp (PVar x) (Special Cons) (PVar xs))) = Just $ Right $ BCons x xs
readPat (PList []) = Just $ Right BNil
readPat _ = Nothing


---------------------------------------------------------------------
-- UTILITY FUNCTIONS

-- a list of application, with any necessary brackets
appsBracket :: [Exp] -> Exp
appsBracket = foldl1 (\x -> ensureBracket1 . App x)


-- generate a lambda, but prettier (if possible)
lambda :: [Name] -> Exp -> Exp
lambda xs (Paren x) = lambda xs x
lambda xs (Lambda s (PVar v:vs) x) = lambda (xs++[v]) (Lambda s vs x)
lambda xs (Lambda _ [] x) = lambda xs x
lambda [x] (App a b) | Var (UnQual x) == b = a
lambda [x] (App a (Paren (App b c)))
    | isAtom a && isAtom b && Var (UnQual x) == c = InfixApp a (toNamed ".") b
lambda [x] (InfixApp a op b)
    | a == Var (UnQual x) = RightSection op b
    | b == Var (UnQual x) = LeftSection a op
lambda [x,y] (view -> App2 op x1 y1)
    | x1 == Var (UnQual x) && y1 == Var (UnQual y) = op
    | x1 == Var (UnQual y) && y1 == Var (UnQual x) = App (toNamed "flip") op
lambda ps x = Lambda nullSrcLoc (map PVar ps) x

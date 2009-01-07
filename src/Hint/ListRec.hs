{-# LANGUAGE PatternGuards, ViewPatterns #-}

{-
map f [] = []
map f (x:xs) = f x : map f xs

foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
-}


module Hint.ListRec(listRecHint) where

import Type
import Util
import HSE.All
import Data.List
import Data.Maybe
import Data.Ord
import Control.Monad
import Data.Generics.PlateData


listRecHint :: Decl -> [Idea]
listRecHint = concatMap f . universe
    where
        f o = maybeToList $ do
            let x = o
            (x, addArgs) <- return $ removeInvariantArgs x
            (x, addCase) <- findCase x
            x <- matchListRec x
            let res = addArgs $ addCase x
                loc = headDef nullSrcLoc $ universeBi o
            return $ idea Error "Use recursion" loc o res


recursive = toVar "_recursive_"

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


matchListRec :: ListCase -> Maybe Exp
matchListRec o@(ListCase vars nil (x,xs,cons))
    
    | [] <- vars, nil ~= "[]", InfixApp lhs c rhs <- cons, opExp c ~= ":"
    , rhs == recursive, Var (UnQual xs) `notElem` universe lhs
    = Just $ appsBracket [toVar "map", lambda [x] lhs, Var $ UnQual xs]

    | [] <- vars, App2 op lhs rhs <- view cons
    , null $ universe op `intersect` [Var (UnQual x), Var (UnQual xs)]
    , rhs == recursive, Var (UnQual xs) `notElem` universe lhs
    = Just $ appsBracket [toVar "foldr", lambda [x] (App op lhs), nil, Var $ UnQual xs]

    | [v] <- vars, Var (UnQual v) == nil, App r lhs <- cons, r == recursive
    , Var (UnQual xs) `notElem` universe lhs
    = Just $ appsBracket [toVar "foldl", lambda [v,x] lhs, Var $ UnQual v, Var $ UnQual xs]

    | otherwise = Nothing


---------------------------------------------------------------------
-- FIND THE CASE ANALYSIS

findCase :: Decl -> Maybe (ListCase, Exp -> Decl)
findCase x = do
    FunBind [x1,x2] <- return x
    Branch name1 ps1 p1 c1 b1 <- findBranch x1
    Branch name2 ps2 p2 c2 b2 <- findBranch x2
    guard (name1 == name2 && ps1 == ps2 && p1 == p2)
    [(BNil, b1), (BCons x xs, b2)] <- return $ sortBy (comparing fst) [(c1,b1), (c2,b2)]
    b2 <- delCons name1 p1 xs b2

    let ps = let (a,b) = splitAt p1 ps1 in map PVar $ a ++ xs : b
    return (ListCase ps1 b1 (x,xs,b2)
           ,\e -> FunBind [Match nullSrcLoc name1 ps (UnGuardedRhs e) (BDecls [])])


-- delete the variable x from position n in each call to name
-- if a call exists, but doesn't have that variable in that position, then Nothing
delCons :: Name -> Int -> Name -> Exp -> Maybe Exp
delCons func pos var (fromApps -> (rec@(Var (UnQual func2)):xs)) | func == func2 = do
    (pre,Var (UnQual x):post) <- return $ splitAt pos xs
    guard $ x == var
    res <- mapM (descendM $ delCons func pos var) (pre++post)
    return $ apps $ recursive : res
delCons func pos var x = descendM (delCons func pos var) x


---------------------------------------------------------------------
-- FIND A BRANCH

findBranch :: Match -> Maybe Branch
findBranch x = do
    Match _ name ps (UnGuardedRhs bod) (BDecls []) <- return x
    (a,b,c) <- findPat ps
    return $ Branch name a b c bod


findPat :: [Pat] -> Maybe ([Name], Int, BList)
findPat ps = do
    ps <- mapM readPat ps
    [i] <- return $ findIndices isRight ps
    let (left,[right]) = unzipEither ps
    return (left, i, right)


readPat :: Pat -> Maybe (Either Name BList)
readPat (PVar x) = Just $ Left x
readPat (PParen (PInfixApp (PVar x) (Special Cons) (PVar xs))) = Just $ Right $ BCons x xs
readPat (PList []) = Just $ Right BNil
readPat _ = Nothing


---------------------------------------------------------------------
-- REMOVE INVARIANT ARGUMENTS

-- An invariant argument is passed in the same location
-- in every recursive call, provide a function for putting them
-- back in. This is similar to reverse lambda lifting.
removeInvariantArgs :: Decl -> (Decl, Decl -> Decl)
removeInvariantArgs x = (x, id)


---------------------------------------------------------------------
-- UTILITY FUNCTIONS (most should move out over time)

-- a list of application, with any necessary brackets
appsBracket :: [Exp] -> Exp
appsBracket = foldl1 (\x y -> ensureBracket1 $ App x y)


apps :: [Exp] -> Exp
apps = foldl1 App


fromApps :: Exp -> [Exp]
fromApps (App x y) = fromApps x ++ [y]
fromApps x = [x]


-- generate a lambda, but prettier (if possible)
lambda :: [Name] -> Exp -> Exp
lambda xs (Paren x) = lambda xs x
lambda [x] (InfixApp a op b)
    | a == Var (UnQual x) = RightSection op b
    | b == Var (UnQual x) = LeftSection a op
lambda [x,y] (view -> App2 op x1 y1)
    | x1 == Var (UnQual x) && y1 == Var (UnQual y) = op
lambda ps x = Lambda nullSrcLoc (map PVar ps) x


isLeft Left{} = True; isLeft _ = False
isRight = not . isLeft

unzipEither :: [Either a b] -> ([a],[b])
unzipEither (Left  x:xs) = let (a,b) = unzipEither xs in (x:a,b)
unzipEither (Right x:xs) = let (a,b) = unzipEither xs in (a,x:b)
unzipEither [] = ([], [])

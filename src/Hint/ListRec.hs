{-# LANGUAGE PatternGuards, ViewPatterns #-}
{-# LANGUAGE PackageImports #-}

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
f (x:xs) a = a + x >>= \fax -> f xs fax ; f [] a = pure a -- f xs a = foldM (+) a xs
foos [] x = x; foos (y:ys) x = foo y $ foos ys x -- foos ys x = foldr foo x ys
f [] y = y; f (x:xs) y = f xs $ g x y -- f xs y = foldl (flip g) y xs
f [] y = y; f (x : xs) y = let z = g x y in f xs z -- f xs y = foldl (flip g) y xs
f [] y = y; f (x:xs) y = f xs (f xs z)
</TEST>
-}


module Hint.ListRec(listRecHint) where

import Hint.Type
import Hint.Util
import Data.List.Extra
import Data.Maybe
import Data.Either.Extra
import Control.Monad
import Refact.Types hiding (RType(Match))

import qualified "ghc-lib-parser" SrcLoc as GHC
import "ghc-lib-parser" HsExtension
import "ghc-lib-parser" HsPat
import "ghc-lib-parser" HsTypes
import "ghc-lib-parser" TysWiredIn
import "ghc-lib-parser" FastString
import "ghc-lib-parser" RdrName
import qualified "ghc-lib-parser" HsBinds as GHC
import qualified "ghc-lib-parser" HsExpr as GHC
import GHC.Util

listRecHint :: DeclHint
listRecHint _ _ = concatMap f . universe
    where
        f o = maybeToList $ do
            let x = o
            (x, addCase) <- findCase x
            (use,severity,x) <- matchListRec x
            let y = addCase x
            guard $ recursiveStr `notElem` varss y
            -- Maybe we can do better here maintaining source formatting?
            return $ idea severity ("Use " ++ use) o y [Replace Decl (toSS o) [] (prettyPrint y)]


recursiveStr :: String
recursiveStr = "_recursive_"
recursive = toNamed recursiveStr
recursiveExpr = noloc $ GHC.HsVar noext (noloc $ mkVarUnqual (fsLit recursiveStr))

-- recursion parameters, nil-case, (x,xs,cons-case)
-- for cons-case delete any recursive calls with xs from them
-- any recursive calls are marked "_recursive_"
data ListCase = ListCase [String] Exp_ (String,String,Exp_)
                deriving Show


data BList = BNil | BCons String String
             deriving (Eq,Ord,Show)

-- function name, parameters, list-position, list-type, body (unmodified)
data Branch = Branch String [String] Int BList Exp_
              deriving Show

data Branch' = Branch' String [String] Int BList (GHC.LHsExpr GhcPs)
              -- deriving Show



---------------------------------------------------------------------
-- MATCH THE RECURSION


matchListRec :: ListCase -> Maybe (String,Severity,Exp_)
matchListRec o@(ListCase vs nil (x,xs,cons))

    | [] <- vs, nil ~= "[]", InfixApp _ lhs c rhs <- cons, opExp c ~= ":"
    , fromParen rhs =~= recursive, xs `notElem` vars lhs
    = Just $ (,,) "map" Warning $ appsBracket
        [toNamed "map", niceLambda [x] lhs, toNamed xs]

    | [] <- vs, App2 op lhs rhs <- view cons
    , vars op `disjoint` [x,xs]
    , fromParen rhs == recursive, xs `notElem` vars lhs
    = Just $ (,,) "foldr" Suggestion $ appsBracket
        [toNamed "foldr", niceLambda [x] $ appsBracket [op,lhs], nil, toNamed xs]

    | [v] <- vs, view nil == Var_ v, App _ r lhs <- cons, r =~= recursive
    , xs `notElem` vars lhs
    = Just $ (,,) "foldl" Suggestion $ appsBracket
        [toNamed "foldl", niceLambda [v,x] lhs, toNamed v, toNamed xs]

    | [v] <- vs, App _ ret res <- nil, isReturn ret, res ~= "()" || view res == Var_ v
    , [Generator _ (view -> PVar_ b1) e, Qualifier _ (fromParen -> App _ r (view -> Var_ b2))] <- asDo cons
    , b1 == b2, r == recursive, xs `notElem` vars e
    , name <- "foldM" ++ ['_' | res ~= "()"]
    = Just $ (,,) name Suggestion $ appsBracket
        [toNamed name, niceLambda [v,x] e, toNamed v, toNamed xs]

    | otherwise = Nothing


-- Very limited attempt to convert >>= to do, only useful for foldM/foldM_
asDo :: Exp_ -> [Stmt S]
asDo (view -> App2 bind lhs (Lambda _ [v] rhs)) = [Generator an v lhs, Qualifier an rhs]
asDo (Do _ x) = x
asDo x = [Qualifier an x]

---------------------------------------------------------------------
-- FIND THE CASE ANALYSIS

findCase :: Decl_ -> Maybe (ListCase, Exp_ -> Decl_)
findCase x = do
    FunBind _ [x1,x2] <- return x
    Branch name1 ps1 p1 c1 b1 <- findBranch x1
    Branch name2 ps2 p2 c2 b2 <- findBranch x2
    guard (name1 == name2 && ps1 == ps2 && p1 == p2)
    [(BNil, b1), (BCons x xs, b2)] <- return $ sortOn fst [(c1,b1), (c2,b2)]
    b2 <- transformAppsM (delCons name1 p1 xs) b2
    (ps,b2) <- return $ eliminateArgs ps1 b2

    let ps12 = let (a,b) = splitAt p1 ps1 in map toNamed $ a ++ xs : b
    return (ListCase ps b1 (x,xs,b2)
           ,\e -> FunBind an [Match an (toNamed name1) ps12 (UnGuardedRhs an e) Nothing])


delCons :: String -> Int -> String -> Exp_ -> Maybe Exp_
delCons func pos var (fromApps -> (view -> Var_ x):xs) | func == x = do
    (pre, (view -> Var_ v):post) <- return $ splitAt pos xs
    guard $ v == var
    return $ apps $ recursive : pre ++ post
delCons _ _ _ x = return x

delCons' :: String -> Int -> String -> GHC.LHsExpr GhcPs -> Maybe (GHC.LHsExpr GhcPs)
delCons' func pos var (fromApps' -> (view' -> Var_' x) : xs) | func == x = do
    (pre, (view' -> Var_' v) : post) <- return $ splitAt pos xs
    guard $ v == var
    return $ apps' $ recursiveExpr : pre ++ post
delCons' _ _ _ x = return x

eliminateArgs :: [String] -> Exp_ -> ([String], Exp_)
eliminateArgs ps cons = (remove ps, transform f cons)
    where
        args = [zs | z:zs <- map fromApps $ universeApps cons, z =~= recursive]
        elim = [all (\xs -> length xs > i && view (xs !! i) == Var_ p) args | (i,p) <- zip [0..] ps] ++ repeat False
        remove = concat . zipWith (\b x -> [x | not b]) elim

        f (fromApps -> x:xs) | x == recursive = apps $ x : remove xs
        f x = x

eliminateArgs' :: [String] -> GHC.LHsExpr GhcPs -> ([String], GHC.LHsExpr GhcPs)
eliminateArgs' ps cons = (remove ps, transform f cons)
  where
    args = [zs | z : zs <- map fromApps' $ universeApps' cons
             , W (transformBi (const GHC.noSrcSpan) z) == W recursiveExpr]
    elim = [all (\xs -> length xs > i && view' (xs !! i) == Var_' p) args | (i, p) <- zip [0..] ps] ++ repeat False
    remove = concat . zipWith (\b x -> [x | not b]) elim

    f (fromApps' -> x : xs) | W x == W recursiveExpr = apps' $ x : remove xs
    f x = x

---------------------------------------------------------------------
-- FIND A BRANCH

findBranch :: Match S -> Maybe Branch
findBranch x = do
    Match _ name ps (UnGuardedRhs _ bod) Nothing <- return x
    (a,b,c) <- findPat ps
    return $ Branch (fromNamed name) a b c $ simplifyExp bod

findBranch' :: GHC.LMatch GhcPs (GHC.HsExpr GhcPs) -> Maybe Branch'
findBranch' (GHC.L _ x) = do
  -- A right hand side of a pattern or function binding that is
  -- unguarded and has no local bindings.
  GHC.Match { GHC.m_ctxt = GHC.FunRhs {GHC.mc_fun=(GHC.L _ name)}
            , GHC.m_pats = ps
            , GHC.m_grhss =
              GHC.GRHSs {GHC.grhssGRHSs=[GHC.L l (GHC.GRHS _ [] body)]
                        , GHC.grhssLocalBinds=GHC.L _ (GHC.EmptyLocalBinds _)
                        }
            } <- return x
  (a, b, c) <- findPat' ps
  return $ Branch' (rdrNameName name) a b c $ simplifyExp' (GHC.L l body)

findPat :: [Pat_] -> Maybe ([String], Int, BList)
findPat ps = do
    ps <- mapM readPat ps
    [i] <- return $ findIndices isRight ps
    let (left,[right]) = partitionEithers ps
    return (left, i, right)

-- If all of the patterns in the input list can be interpreted as
-- variables ('x') or list constructions ('x : xs'), then return a
-- triple : the variables, the indices in the input list that
-- correspond to list constructions and lastly the lists. If one or
-- more of the patterns in the input list can't be classified in
-- either of those two ways, return 'Nothing'.
findPat' :: [LPat GhcPs] -> Maybe ([String], Int, BList)
findPat' ps = do
  ps <- mapM readPat' ps
  [i] <- return $ findIndices isRight ps
  let (left, [right]) = partitionEithers ps
  return (left, i, right)

readPat :: Pat_ -> Maybe (Either String BList)
readPat (view -> PVar_ x) = Just $ Left x
readPat (PParen _ (PInfixApp _ (view -> PVar_ x) (Special _ Cons{}) (view -> PVar_ xs))) = Just $ Right $ BCons x xs
readPat (PList _ []) = Just $ Right BNil
readPat _ = Nothing

-- Interpret a pattern as a either a variable 'x', a list of form 'x :
-- xs' (or 'Nothing' if neither).
readPat' :: LPat GhcPs -> Maybe (Either String BList)
readPat' (view' -> PVar_' x) = Just $ Left x
readPat' (ParPat _ (ConPatIn (GHC.L _ n) (InfixCon (view' -> PVar_' x) (view' -> PVar_' xs))))
  | n == consDataCon_RDR = Just $ Right $ BCons x xs
readPat' (ListPat _ []) = Just $ Right BNil
readPat' _ = Nothing

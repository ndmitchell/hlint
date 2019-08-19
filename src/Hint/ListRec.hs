{-# LANGUAGE PatternGuards, ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

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

import Hint.Type (DeclHint', Severity(Suggestion, Warning), idea', toSS')

import Data.Generics.Uniplate.Operations
import Data.List.Extra
import Data.Maybe
import Data.Either.Extra
import Control.Monad
import Refact.Types hiding (RType(Match))

import "ghc-lib-parser" SrcLoc
import "ghc-lib-parser" HsExtension
import "ghc-lib-parser" HsPat
import "ghc-lib-parser" HsTypes
import "ghc-lib-parser" TysWiredIn
import "ghc-lib-parser" RdrName
import "ghc-lib-parser" HsBinds
import "ghc-lib-parser" HsExpr
import "ghc-lib-parser" HsDecls
import "ghc-lib-parser" OccName
import "ghc-lib-parser" BasicTypes

import GHC.Util

listRecHint :: DeclHint'
listRecHint _ _ = concatMap f . universe
    where
        f o = maybeToList $ do
            let x = o
            (x, addCase) <- findCase x
            (use,severity,x) <- matchListRec x
            let y = addCase x
            guard $ recursiveStr `notElem` varss' y
            -- Maybe we can do better here maintaining source
            -- formatting?
            return $ idea' severity ("Use " ++ use) o y [Replace Decl (toSS' o) [] (unsafePrettyPrint y)]

recursiveStr :: String
recursiveStr = "_recursive_"
recursive = strToVar' recursiveStr

data ListCase =
  ListCase
    [String] -- recursion parameters
    (LHsExpr GhcPs)  -- nil case
    (String, String, LHsExpr GhcPs) -- cons case
-- For cons-case delete any recursive calls with 'xs' in them. Any
-- recursive calls are marked "_recursive_".

data BList = BNil | BCons String String
             deriving (Eq, Ord, Show)

data Branch =
  Branch
    String  -- function name
    [String]  -- parameters
    Int -- list position
    BList (LHsExpr GhcPs) -- list type/body


---------------------------------------------------------------------
-- MATCH THE RECURSION


matchListRec :: ListCase -> Maybe (String, Severity, LHsExpr GhcPs)
matchListRec o@(ListCase vs nil (x, xs, cons))
    -- Suggest 'map'?
    | [] <- vs, varToStr' nil == "[]", (LL _ (OpApp _ lhs c rhs)) <- cons, varToStr' c == ":"
    , eqNoLoc' (fromParen' rhs) recursive, xs `notElem` vars' lhs
    = Just $ (,,) "map" Hint.Type.Warning $
      appsBracket' [ strToVar' "map", niceLambda' [x] lhs, strToVar' xs]
    -- Suggest 'foldr'?
    | [] <- vs, App2' op lhs rhs <- view' cons, vars' op `disjoint` [x, xs]
    , eqNoLoc' (fromParen' rhs) recursive
    = Just $ (,,) "foldr" Suggestion $
      appsBracket' [ strToVar' "foldr", niceLambda' [x] $ appsBracket' [op,lhs], nil, strToVar' xs]
    -- Suggest 'foldl'?
    | [v] <- vs, view' nil == Var_' v, (LL _ (HsApp _ r lhs)) <- cons
    , eqNoLoc' (fromParen' r) recursive
    , xs `notElem` vars' lhs
    = Just $ (,,) "foldl" Suggestion $
      appsBracket' [ strToVar' "foldl", niceLambda' [v,x] lhs, strToVar' v, strToVar' xs]
    -- Suggest 'foldM'?
    | [v] <- vs, (LL _ (HsApp _ ret res)) <- nil, isReturn' ret, varToStr' res == "()" || view' res == Var_' v
    , [LL _ (BindStmt _ (view' -> PVar_' b1) e _ _), LL _ (BodyStmt _ (fromParen' -> (LL _ (HsApp _ r (view' -> Var_' b2)))) _ _)] <- asDo cons
    , b1 == b2, eqNoLoc' r recursive, xs `notElem` vars' e
    , name <- "foldM" ++ ['_' | varToStr' res == "()"]
    = Just $ (,,) name Suggestion $
      appsBracket' [strToVar' name, niceLambda' [v,x] e, strToVar' v, strToVar' xs]
    -- Nope, I got nothing ¯\_(ツ)_/¯.
    | otherwise = Nothing

-- Very limited attempt to convert >>= to do, only useful for
-- 'foldM' / 'foldM_'.
asDo :: LHsExpr GhcPs -> [LStmt GhcPs (LHsExpr GhcPs)]
asDo (view' ->
       App2' bind lhs
         (LL _ (HsLam _ MG {
             mg_alts=LL _ [
                 LL _ Match {  m_ctxt=LambdaExpr
                            , m_pats=[LL _ v@VarPat{}]
                            , m_grhss=GRHSs _
                                        [LL _ (GRHS _ [] rhs)]
                                        (LL _ (EmptyLocalBinds _))}]}))
      ) =
  [ noLoc $ BindStmt noExt v lhs noSyntaxExpr' noSyntaxExpr'
  , noLoc $ BodyStmt noExt rhs noSyntaxExpr' noSyntaxExpr'     ]
asDo (LL _ (HsDo _ DoExpr (LL _ stmts))) = stmts
asDo x = [noLoc $ BodyStmt noExt x noSyntaxExpr' noSyntaxExpr']


---------------------------------------------------------------------
-- FIND THE CASE ANALYSIS


findCase :: LHsDecl GhcPs -> Maybe (ListCase, LHsExpr GhcPs -> LHsDecl GhcPs)
findCase x = do
  -- Match a function binding with two alternatives.
  (LL _ (ValD _ FunBind {fun_matches=
              MG{mg_alts=
                     (LL _
                            [ x1@(LL _ Match{..}) -- Match fields.
                            , x2]), ..} -- Match group fields.
          , ..} -- Fun. bind fields.
      )) <- return x

  Branch name1 ps1 p1 c1 b1 <- findBranch x1
  Branch name2 ps2 p2 c2 b2 <- findBranch x2
  guard (name1 == name2 && ps1 == ps2 && p1 == p2)
  [(BNil, b1), (BCons x xs, b2)] <- return $ sortOn fst [(c1, b1), (c2, b2)]
  b2 <- transformAppsM' (delCons name1 p1 xs) b2
  (ps, b2) <- return $ eliminateArgs ps1 b2

  let ps12 = let (a, b) = splitAt p1 ps1 in map strToPat (a ++ xs : b) -- Function arguments.
      emptyLocalBinds = noLoc $ EmptyLocalBinds noExt -- Empty where clause.
      gRHS e = noLoc $ GRHS noExt [] e :: LGRHS GhcPs (LHsExpr GhcPs) -- Guarded rhs.
      gRHSSs e = GRHSs noExt [gRHS e] emptyLocalBinds -- Guarded rhs set.
      match e = Match{m_ext=noExt,m_pats=ps12, m_grhss=gRHSSs e, ..} -- Match.
      matchGroup e = MG{mg_alts=noLoc [noLoc $ match e], mg_origin=Generated, ..} -- Match group.
      funBind e = FunBind {fun_matches=matchGroup e, ..} :: HsBindLR GhcPs GhcPs -- Fun bind.

  return (ListCase ps b1 (x, xs, b2), noLoc . ValD noExt . funBind)

delCons :: String -> Int -> String -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs)
delCons func pos var (fromApps' -> (view' -> Var_' x) : xs) | func == x = do
    (pre, (view' -> Var_' v) : post) <- return $ splitAt pos xs
    guard $ v == var
    return $ apps' $ recursive : pre ++ post
delCons _ _ _ x = return x

eliminateArgs :: [String] -> LHsExpr GhcPs -> ([String], LHsExpr GhcPs)
eliminateArgs ps cons = (remove ps, transform f cons)
  where
    args = [zs | z : zs <- map fromApps' $ universeApps' cons, eqNoLoc' z recursive]
    elim = [all (\xs -> length xs > i && view' (xs !! i) == Var_' p) args | (i, p) <- zip [0..] ps] ++ repeat False
    remove = concat . zipWith (\b x -> [x | not b]) elim

    f (fromApps' -> x : xs) | eqNoLoc' x recursive = apps' $ x : remove xs
    f x = x


---------------------------------------------------------------------
-- FIND A BRANCH


findBranch :: LMatch GhcPs (LHsExpr GhcPs) -> Maybe Branch
findBranch (L _ x) = do
  Match { m_ctxt = FunRhs {mc_fun=(L _ name)}
            , m_pats = ps
            , m_grhss =
              GRHSs {grhssGRHSs=[L l (GRHS _ [] body)]
                        , grhssLocalBinds=L _ (EmptyLocalBinds _)
                        }
            } <- return x
  (a, b, c) <- findPat ps
  return $ Branch (occNameString $rdrNameOcc name) a b c $ simplifyExp' body

findPat :: [LPat GhcPs] -> Maybe ([String], Int, BList)
findPat ps = do
  ps <- mapM readPat ps
  [i] <- return $ findIndices isRight ps
  let (left, [right]) = partitionEithers ps

  return (left, i, right)

readPat :: Pat GhcPs -> Maybe (Either String BList)
readPat (view' -> PVar_' x) = Just $ Left x
readPat (LL _ (ParPat _ (LL _ (ConPatIn (L _ n) (InfixCon (view' -> PVar_' x) (view' -> PVar_' xs))))))
 | n == consDataCon_RDR = Just $ Right $ BCons x xs
readPat (LL _ (ConPatIn (L _ n) (PrefixCon [])))
  | n == nameRdrName nilDataConName = Just $ Right BNil
readPat _ = Nothing

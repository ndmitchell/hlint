{-# LANGUAGE PatternGuards, ViewPatterns #-}
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
fun [] = []; fun (x:xs) = f x xs ++ fun xs
</TEST>
-}


module Hint.ListRec(listRecHint) where

import Hint.Type (DeclHint, Severity(Suggestion, Warning), idea, toSSA)

import Data.Generics.Uniplate.DataOnly
import Data.List.Extra
import Data.Maybe
import Data.Either.Extra
import Control.Monad
import Refact.Types hiding (RType(Match))

import GHC.Types.SrcLoc
import GHC.Hs.Extension
import GHC.Hs.Pat
import GHC.Builtin.Types
import GHC.Hs.Type
import GHC.Types.Name.Reader
import GHC.Hs.Binds
import GHC.Hs.Expr
import GHC.Hs.Decls
import GHC.Types.Basic

import GHC.Parser.Annotation
import Language.Haskell.Syntax.Extension

import GHC.Util
import Language.Haskell.GhclibParserEx.GHC.Hs.Pat
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader

listRecHint :: DeclHint
listRecHint _ _ = concatMap f . universe
    where
        f o = maybeToList $ do
            let x = o
            (x, addCase) <- findCase x
            (use,severity,x) <- matchListRec x
            let y = addCase x
            guard $ recursiveStr `notElem` varss y
            -- Maybe we can do better here maintaining source
            -- formatting?
            pure $ idea severity ("Use " ++ use) (reLoc o) (reLoc y) [Replace Decl (toSSA o) [] (unsafePrettyPrint y)]

recursiveStr :: String
recursiveStr = "_recursive_"
recursive = strToVar recursiveStr

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
    | [] <- vs, varToStr nil == "[]", (L _ (OpApp _ lhs c rhs)) <- cons, varToStr c == ":"
    , astEq (fromParen rhs) recursive, xs `notElem` vars lhs
    = Just $ (,,) "map" Hint.Type.Warning $
      appsBracket [ strToVar "map", niceLambda [x] lhs, strToVar xs]
    -- Suggest 'foldr'?
    | [] <- vs, App2 op lhs rhs <- view cons
    , xs `notElem` (vars op ++ vars lhs) -- the meaning of xs changes, see #793
    , astEq (fromParen rhs) recursive
    = Just $ (,,) "foldr" Suggestion $
      appsBracket [ strToVar "foldr", niceLambda [x] $ appsBracket [op,lhs], nil, strToVar xs]
    -- Suggest 'foldl'?
    | [v] <- vs, view nil == Var_ v, (L _ (HsApp _ r lhs)) <- cons
    , astEq (fromParen r) recursive
    , xs `notElem` vars lhs
    = Just $ (,,) "foldl" Suggestion $
      appsBracket [ strToVar "foldl", niceLambda [v,x] lhs, strToVar v, strToVar xs]
    -- Suggest 'foldM'?
    | [v] <- vs, (L _ (HsApp _ ret res)) <- nil, isReturn ret, varToStr res == "()" || view res == Var_ v
    , [L _ (BindStmt _ (view -> PVar_ b1) e), L _ (BodyStmt _ (fromParen -> (L _ (HsApp _ r (view -> Var_ b2)))) _ _)] <- asDo cons
    , b1 == b2, astEq r recursive, xs `notElem` vars e
    , name <- "foldM" ++ ['_' | varToStr res == "()"]
    = Just $ (,,) name Suggestion $
      appsBracket [strToVar name, niceLambda [v,x] e, strToVar v, strToVar xs]
    -- Nope, I got nothing ¯\_(ツ)_/¯.
    | otherwise = Nothing

-- Very limited attempt to convert >>= to do, only useful for
-- 'foldM' / 'foldM_'.
asDo :: LHsExpr GhcPs -> [LStmt GhcPs (LHsExpr GhcPs)]
asDo (view ->
       App2 bind lhs
         (L _ (HsLam _ MG {
              mg_origin=FromSource
            , mg_alts=L _ [
                 L _ Match {  m_ctxt=LambdaExpr
                            , m_pats=[v@(L _ VarPat{})]
                            , m_grhss=GRHSs _
                                        [L _ (GRHS _ [] rhs)]
                                        (EmptyLocalBinds _)}]}))
      ) =
  [ noLocA $ BindStmt EpAnnNotUsed v lhs
  , noLocA $ BodyStmt noExtField rhs noSyntaxExpr noSyntaxExpr ]
asDo (L _ (HsDo _ (DoExpr _) (L _ stmts))) = stmts
asDo x = [noLocA $ BodyStmt noExtField x noSyntaxExpr noSyntaxExpr]


---------------------------------------------------------------------
-- FIND THE CASE ANALYSIS


findCase :: LHsDecl GhcPs -> Maybe (ListCase, LHsExpr GhcPs -> LHsDecl GhcPs)
findCase x = do
  -- Match a function binding with two alternatives.
  (L _ (ValD _ FunBind {fun_matches=
              MG{mg_origin=FromSource, mg_alts=
                     (L _
                            [ x1@(L _ Match{..}) -- Match fields.
                            , x2]), ..} -- Match group fields.
          , ..} -- Fun. bind fields.
      )) <- pure x

  Branch name1 ps1 p1 c1 b1 <- findBranch x1
  Branch name2 ps2 p2 c2 b2 <- findBranch x2
  guard (name1 == name2 && ps1 == ps2 && p1 == p2)
  [(BNil, b1), (BCons x xs, b2)] <- pure $ sortOn fst [(c1, b1), (c2, b2)]
  b2 <- transformAppsM (delCons name1 p1 xs) b2
  (ps, b2) <- pure $ eliminateArgs ps1 b2

  let ps12 = let (a, b) = splitAt p1 ps1 in map strToPat (a ++ xs : b) -- Function arguments.
      emptyLocalBinds = EmptyLocalBinds noExtField :: HsLocalBindsLR GhcPs GhcPs -- Empty where clause.
      gRHS e = noLocA $ GRHS EpAnnNotUsed [] e :: LGRHS GhcPs (LHsExpr GhcPs) -- Guarded rhs.
      gRHSSs e = GRHSs emptyComments [gRHS e] emptyLocalBinds -- Guarded rhs set.
      match e = Match{m_ext=EpAnnNotUsed,m_pats=ps12, m_grhss=gRHSSs e, ..} -- Match.
      matchGroup e = MG{mg_alts=noLocA [noLocA $ match e], mg_origin=Generated, ..} -- Match group.
      funBind e = FunBind {fun_matches=matchGroup e, ..} :: HsBindLR GhcPs GhcPs -- Fun bind.

  pure (ListCase ps b1 (x, xs, b2), noLocA . ValD noExtField . funBind)

delCons :: String -> Int -> String -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs)
delCons func pos var (fromApps -> (view -> Var_ x) : xs) | func == x = do
    (pre, (view -> Var_ v) : post) <- pure $ splitAt pos xs
    guard $ v == var
    pure $ apps $ recursive : pre ++ post
delCons _ _ _ x = pure x

eliminateArgs :: [String] -> LHsExpr GhcPs -> ([String], LHsExpr GhcPs)
eliminateArgs ps cons = (remove ps, transform f cons)
  where
    args = [zs | z : zs <- map fromApps $ universeApps cons, astEq z recursive]
    elim = [all (\xs -> length xs > i && view (xs !! i) == Var_ p) args | (i, p) <- zipFrom 0 ps] ++ repeat False
    remove = concat . zipWith (\b x -> [x | not b]) elim

    f (fromApps -> x : xs) | astEq x recursive = apps $ x : remove xs
    f x = x


---------------------------------------------------------------------
-- FIND A BRANCH


findBranch :: LMatch GhcPs (LHsExpr GhcPs) -> Maybe Branch
findBranch (L _ x) = do
  Match { m_ctxt = FunRhs {mc_fun=(L _ name)}
            , m_pats = ps
            , m_grhss =
              GRHSs {grhssGRHSs=[L l (GRHS _ [] body)]
                        , grhssLocalBinds=EmptyLocalBinds _
                        }
            } <- pure x
  (a, b, c) <- findPat ps
  pure $ Branch (occNameStr name) a b c $ simplifyExp body

findPat :: [LPat GhcPs] -> Maybe ([String], Int, BList)
findPat ps = do
  ps <- mapM readPat ps
  [i] <- pure $ findIndices isRight ps
  let (left, [right]) = partitionEithers ps

  pure (left, i, right)

readPat :: LPat GhcPs -> Maybe (Either String BList)
readPat (view -> PVar_ x) = Just $ Left x
readPat (L _ (ParPat _ _ (L _ (ConPat _ (L _ n) (InfixCon (view -> PVar_ x) (view -> PVar_ xs)))) _))
 | n == consDataCon_RDR = Just $ Right $ BCons x xs
readPat (L _ (ConPat _ (L _ n) (PrefixCon [] [])))
  | n == nameRdrName nilDataConName = Just $ Right BNil
readPat _ = Nothing

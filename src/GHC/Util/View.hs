{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, FlexibleInstances, PatternSynonyms #-}

module GHC.Util.View (
   fromParen
  , View(..)
  , RdrName_(RdrName_), Var_(Var_), PVar_(PVar_), PApp_(PApp_), App2(App2),LamConst1(LamConst1)
  , pattern SimpleLambda
) where

import GHC.Hs
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.Basic
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
import GHC.Util.Brackets

fromParen :: LocatedA (HsExpr GhcPs) -> LocatedA (HsExpr GhcPs)
fromParen x = maybe x fromParen $ remParen x

fromPParen :: LocatedA (Pat GhcPs) -> LocatedA (Pat GhcPs)
fromPParen (L _ (ParPat _ x)) = fromPParen x
fromPParen x = x

class View a b where
  view :: a -> b

data RdrName_ = NoRdrName_ | RdrName_ (LocatedN RdrName)
data Var_  = NoVar_ | Var_ String deriving Eq
data PVar_ = NoPVar_ | PVar_ String
data PApp_ = NoPApp_ | PApp_ String [LocatedA (Pat GhcPs)]
data App2  = NoApp2  | App2 (LocatedA (HsExpr GhcPs)) (LocatedA (HsExpr GhcPs)) (LocatedA (HsExpr GhcPs))
data LamConst1 = NoLamConst1 | LamConst1 (LocatedA (HsExpr GhcPs))

instance View (LocatedA (HsExpr GhcPs)) LamConst1 where
  view (fromParen -> (L _ (HsLam _ (MG _ (L _ [L _ (Match _ LambdaExpr [L _ WildPat {}]
    (GRHSs _ [L _ (GRHS _ [] x)] ((EmptyLocalBinds _))))]) FromSource)))) = LamConst1 x
  view _ = NoLamConst1

instance View (LocatedA (HsExpr GhcPs)) RdrName_ where
    view (fromParen -> (L _ (HsVar _ name))) = RdrName_ name
    view _ = NoRdrName_

instance View (LocatedA (HsExpr GhcPs)) Var_ where
    view (view -> RdrName_ name) = Var_ (rdrNameStr name)
    view _ = NoVar_

instance View (LocatedA (HsExpr GhcPs)) App2 where
  view (fromParen -> L _ (OpApp _ lhs op rhs)) = App2 op lhs rhs
  view (fromParen -> L _ (HsApp _ (L _ (HsApp _ f x)) y)) = App2 f x y
  view _ = NoApp2

instance View (LocatedA (Pat GhcPs)) PVar_ where
  view (fromPParen -> L _ (VarPat _ (L _ x))) = PVar_ $ occNameStr x
  view _ = NoPVar_

instance View (LocatedA (Pat GhcPs)) PApp_ where
  view (fromPParen -> L _ (ConPat _ (L _ x) (PrefixCon _ args))) =
    PApp_ (occNameStr x) args
  view (fromPParen -> L _ (ConPat _ (L _ x) (InfixCon lhs rhs))) =
    PApp_ (occNameStr x) [lhs, rhs]
  view _ = NoPApp_

-- A lambda with no guards and no where clauses
pattern SimpleLambda :: [LocatedA (Pat GhcPs)] -> LocatedA (HsExpr GhcPs) -> LocatedA (HsExpr GhcPs)
pattern SimpleLambda vs body <- L _ (HsLam _ (MG _ (L _ [L _ (Match _ _ vs (GRHSs _ [L _ (GRHS _ [] body)] ((EmptyLocalBinds _))))]) _))

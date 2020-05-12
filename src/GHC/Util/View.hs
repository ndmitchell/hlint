{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, FlexibleInstances, PatternSynonyms #-}

module GHC.Util.View (
   fromParen
  , View(..)
  , Var_(Var_), PVar_(PVar_), PApp_(PApp_), App2(App2),LamConst1(LamConst1)
  , pattern SimpleLambda
) where

import GHC.Hs
import SrcLoc
import RdrName
import OccName
import BasicTypes
import GHC.Util.RdrName (rdrNameStr')

fromParen :: LHsExpr GhcPs -> LHsExpr GhcPs
fromParen (L _ (HsPar _ x)) = fromParen x
fromParen x = x

fromPParen :: LPat GhcPs -> LPat GhcPs
fromPParen (L _ (ParPat _ x)) = fromPParen x
fromPParen x = x

class View a b where
  view :: a -> b

data Var_  = NoVar_ | Var_ String deriving Eq
data PVar_ = NoPVar_ | PVar_ String
data PApp_ = NoPApp_ | PApp_ String [LPat GhcPs]
data App2  = NoApp2  | App2 (LHsExpr GhcPs) (LHsExpr GhcPs) (LHsExpr GhcPs)
data LamConst1 = NoLamConst1 | LamConst1 (LHsExpr GhcPs)

instance View (LHsExpr GhcPs) LamConst1 where
  view (fromParen -> (L _ (HsLam _ (MG _ (L _ [L _ (Match _ LambdaExpr [L _ WildPat {}]
    (GRHSs _ [L _ (GRHS _ [] x)] (L _ (EmptyLocalBinds _))))]) FromSource)))) = LamConst1 x
  view _ = NoLamConst1

instance View (LHsExpr GhcPs) Var_ where
    view (fromParen -> (L _ (HsVar _ (rdrNameStr' -> x)))) = Var_ x
    view _ = NoVar_

instance View (LHsExpr GhcPs) App2 where
  view (fromParen -> L _ (OpApp _ lhs op rhs)) = App2 op lhs rhs
  view (fromParen -> L _ (HsApp _ (L _ (HsApp _ f x)) y)) = App2 f x y
  view _ = NoApp2

instance View (Located (Pat GhcPs)) PVar_ where
  view (fromPParen -> L _ (VarPat _ (L _ x))) = PVar_ $ occNameString (rdrNameOcc x)
  view _ = NoPVar_

instance View (Located (Pat GhcPs)) PApp_ where
  view (fromPParen -> L _ (ConPatIn (L _ x) (PrefixCon args))) =
    PApp_ (occNameString . rdrNameOcc $ x) args
  view (fromPParen -> L _ (ConPatIn (L _ x) (InfixCon lhs rhs))) =
    PApp_ (occNameString . rdrNameOcc $ x) [lhs, rhs]
  view _ = NoPApp_

-- A lambda with no guards and no where clauses
pattern SimpleLambda :: [LPat GhcPs] -> LHsExpr GhcPs -> LHsExpr GhcPs
pattern SimpleLambda vs body <- L _ (HsLam _ (MG _ (L _ [L _ (Match _ _ vs (GRHSs _ [L _ (GRHS _ [] body)] (L _ (EmptyLocalBinds _))))]) _))

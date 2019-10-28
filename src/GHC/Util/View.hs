{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}

module GHC.Util.View (
   fromParen', fromPParen'
  , View'(..)
  , Var_'(Var_'), PVar_'(PVar_'), PApp_'(PApp_'), App2'(App2'),LamConst1'(LamConst1')
) where

import HsSyn
import SrcLoc
import RdrName
import OccName
import BasicTypes

fromParen' :: LHsExpr GhcPs -> LHsExpr GhcPs
fromParen' (LL _ (HsPar _ x)) = fromParen' x
fromParen' x = x

fromPParen' :: Pat GhcPs -> Pat GhcPs
fromPParen' (LL _ (ParPat _ x)) = fromPParen' x
fromPParen' x = x

class View' a b where
  view' :: a -> b

data Var_'  = NoVar_' | Var_' String deriving Eq
data PVar_' = NoPVar_' | PVar_' String
data PApp_' = NoPApp_' | PApp_' String [Pat GhcPs]
data App2'  = NoApp2'  | App2' (LHsExpr GhcPs) (LHsExpr GhcPs) (LHsExpr GhcPs)
data LamConst1' = NoLamConst1' | LamConst1' (LHsExpr GhcPs)

instance View' (LHsExpr GhcPs) LamConst1' where
  view' (fromParen' -> (LL _ (HsLam _ (MG _ (L _ [LL _ (Match _ LambdaExpr [LL _ WildPat {}]
    (GRHSs _ [LL _ (GRHS _ [] x)] (LL _ (EmptyLocalBinds _))))]) FromSource)))) = LamConst1' x
  view' _ = NoLamConst1'

instance View' (LHsExpr GhcPs) Var_' where
    view' (fromParen' -> (LL _ (HsVar _ (LL _ (Unqual x))))) = Var_' $ occNameString x
    view' _ = NoVar_'

instance View' (LHsExpr GhcPs) App2' where
  view' (fromParen' -> LL _ (OpApp _ lhs op rhs)) = App2' op lhs rhs
  view' (fromParen' -> LL _ (HsApp _ (LL _ (HsApp _ f x)) y)) = App2' f x y
  view' _ = NoApp2'

instance View' (Pat GhcPs) PVar_' where
  view' (fromPParen' -> LL _ (VarPat _ (L _ x))) = PVar_' $ occNameString (rdrNameOcc x)
  view' _ = NoPVar_'

instance View' (Pat GhcPs) PApp_' where
  view' (fromPParen' -> LL _ (ConPatIn (L _ x) (PrefixCon args))) =
    PApp_' (occNameString . rdrNameOcc $ x) args
  view' (fromPParen' -> LL _ (ConPatIn (L _ x) (InfixCon lhs rhs))) =
    PApp_' (occNameString . rdrNameOcc $ x) [lhs, rhs]
  view' _ = NoPApp_'

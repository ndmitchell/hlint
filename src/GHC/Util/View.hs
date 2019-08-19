{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}

module GHC.Util.View (
   fromParen', fromPParen'
  , View'(..)
  , Var_'(Var_'), PVar_'(PVar_'), App2'(App2')
) where

import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" SrcLoc
import "ghc-lib-parser" RdrName
import "ghc-lib-parser" OccName

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
data App2'  = NoApp2'  | App2' (LHsExpr GhcPs) (LHsExpr GhcPs) (LHsExpr GhcPs)

instance View' (LHsExpr GhcPs) Var_' where
    view' (fromParen' -> (LL _ (HsVar _ (LL _ (Unqual x))))) = Var_' $ occNameString x
    view' _ = NoVar_'

instance View' (LHsExpr GhcPs) App2' where
  view' (fromParen' -> LL _ (OpApp _ lhs op rhs)) = App2' op lhs rhs
  view' (fromParen' -> LL _ (HsApp _ (LL _ (HsApp _ f x)) y)) = App2' f x y
  view' _ = NoApp2'

instance View' (Pat GhcPs) PVar_' where
  view' (LL _ (fromPParen' -> VarPat _ (L _ x))) = PVar_' $ occNameString (rdrNameOcc x)
  view' _ = NoPVar_'

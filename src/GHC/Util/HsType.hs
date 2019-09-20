{-# LANGUAGE PackageImports #-}

module GHC.Util.HsType (
    Brackets'(..)
  , fromTyParen'
  ) where

import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" SrcLoc

import GHC.Util.Brackets

fromTyParen' :: LHsType GhcPs -> LHsType GhcPs
fromTyParen' (LL _ (HsParTy _ x)) = x
fromTyParen' x = x

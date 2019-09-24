
module GHC.Util.HsType (
    Brackets'(..)
  , fromTyParen'
  ) where

import HsSyn
import SrcLoc

import GHC.Util.Brackets

fromTyParen' :: LHsType GhcPs -> LHsType GhcPs
fromTyParen' (LL _ (HsParTy _ x)) = x
fromTyParen' x = x

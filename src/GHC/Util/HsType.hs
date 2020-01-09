
module GHC.Util.HsType (
    Brackets'(..)
  , fromTyParen'
  , isTyQuasiQuote'
  , isUnboxedTuple'
  ) where

import HsSyn
import SrcLoc

import GHC.Util.Brackets

fromTyParen' :: LHsType GhcPs -> LHsType GhcPs
fromTyParen' (LL _ (HsParTy _ x)) = x
fromTyParen' x = x

isTyQuasiQuote' :: LHsType GhcPs -> Bool
isTyQuasiQuote' (LL _ (HsSpliceTy _ HsQuasiQuote{})) = True; isTyQuasiQuote' _ = False

isUnboxedTuple' :: HsTupleSort -> Bool
isUnboxedTuple' HsUnboxedTuple = True
isUnboxedTuple' _ = False

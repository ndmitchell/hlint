{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses , FlexibleInstances, FlexibleContexts #-}

module GHC.Util.Pat (
    strToPat
  , Brackets'(..)
  ) where

import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" SrcLoc
import "ghc-lib-parser" TysWiredIn
import "ghc-lib-parser" FastString
import "ghc-lib-parser" RdrName

import GHC.Util.Brackets

-- Make a pattern from a string.
strToPat :: String -> Pat GhcPs
strToPat z
  | z == "True"  = ConPatIn (noLoc true_RDR) (PrefixCon [])
  | z == "False" = ConPatIn (noLoc false_RDR) (PrefixCon [])
  | z == "[]"    = ConPatIn (noLoc $ nameRdrName nilDataConName) (PrefixCon [])
  | otherwise    = VarPat noExt (noLoc $ mkVarUnqual (fsLit z))

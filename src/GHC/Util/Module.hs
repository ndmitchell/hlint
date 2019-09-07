{-# LANGUAGE PackageImports #-}

module GHC.Util.Module (modName) where

import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" Module
import "ghc-lib-parser" SrcLoc

modName :: Located (HsModule GhcPs) -> String
modName (LL _ HsModule {hsmodName=Nothing}) = "Main"
modName (LL _ HsModule {hsmodName=Just (L _ n)}) = moduleNameString n
modName _ = "" -- {-# COMPLETE LL #-}

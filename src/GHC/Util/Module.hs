{-# LANGUAGE PackageImports #-}

module GHC.Util.Module (modName) where

import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" Module
import "ghc-lib-parser" SrcLoc

modName :: HsModule GhcPs -> String
modName HsModule {hsmodName=Nothing} = "Main"
modName HsModule {hsmodName=Just (L _ n)} = moduleNameString n

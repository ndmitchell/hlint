
module GHC.Util.Module (modName, fromModuleName') where

import HsSyn
import Module
import SrcLoc

modName :: Located (HsModule GhcPs) -> String
modName (LL _ HsModule {hsmodName=Nothing}) = "Main"
modName (LL _ HsModule {hsmodName=Just (L _ n)}) = moduleNameString n
modName _ = "" -- {-# COMPLETE LL #-}

fromModuleName' :: Located ModuleName -> String
fromModuleName' (LL _ n) = moduleNameString n
fromModuleName' _ = "" -- {# COMPLETE LL #}

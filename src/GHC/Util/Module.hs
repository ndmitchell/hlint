
module GHC.Util.Module (modName, fromModuleName') where

import HsSyn
import Module
import SrcLoc

modName :: Located (HsModule GhcPs) -> String
modName (L _ HsModule {hsmodName=Nothing}) = "Main"
modName (L _ HsModule {hsmodName=Just (L _ n)}) = moduleNameString n

fromModuleName' :: Located ModuleName -> String
fromModuleName' (L _ n) = moduleNameString n

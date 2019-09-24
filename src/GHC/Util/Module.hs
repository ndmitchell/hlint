
module GHC.Util.Module (modName) where

import HsSyn
import Module
import SrcLoc

modName :: Located (HsModule GhcPs) -> String
modName (LL _ HsModule {hsmodName=Nothing}) = "Main"
modName (LL _ HsModule {hsmodName=Just (L _ n)}) = moduleNameString n
modName _ = "" -- {-# COMPLETE LL #-}

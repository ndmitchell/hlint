{-
    Suggest using better export declarations

<TEST>
main = 1
module Foo where foo = 1 -- module Foo(module Foo) where
module Foo(foo) where foo = 1
module Foo(module Foo) where foo = 1 -- @Ignore module Foo(...) where
module Foo(module Foo, foo) where foo = 1 -- module Foo(..., foo) where
</TEST>
-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Hint.Export(exportHint) where

import Hint.Type
import "ghc-lib-parser" HsSyn
import qualified "ghc-lib-parser" Module as GHC
import "ghc-lib-parser" SrcLoc as GHC
import "ghc-lib-parser" OccName
import "ghc-lib-parser" RdrName
import GHC.Util

exportHint :: ModuHint
exportHint _ (ModuleEx _ _ (dL -> L s m@HsModule {hsmodName = Just name, hsmodExports = exports}) _)
  | Nothing <- exports =
      let r = o{ hsmodExports = Just (noloc [noloc (IEModuleContents noext name)] )} in
      [(ignore' "Use module export list" (cL s o) (noloc r) []){ideaNote = [Note "an explicit list is usally better"]}]
  | Just (dL -> L _ xs) <- exports
  , mods <- [x | x <- xs, isMod x]
  , modName <- GHC.moduleNameString (unloc name)
  , names <- [GHC.moduleNameString (unloc n) | (dL -> L _ (IEModuleContents _ n)) <- mods]
  , exports' <- [x | x <- xs, not (matchesModName modName x)]
  , modName `elem` names =
      let dots = mkRdrUnqual (mkVarOcc " ... ")
          r = o{ hsmodExports = Just (noloc (noloc (IEVar noext (noloc (IEName (noloc dots)))) : exports') )}
      in
        [ignore' "Use explicit module export list" (cL s o) (noloc r) []]
      where
          o = m{hsmodImports=[], hsmodDecls=[], hsmodDeprecMessage=Nothing, hsmodHaddockModHeader=Nothing }
          isMod (dL -> L _ (IEModuleContents _ _)) = True
          isMod _ = False

          matchesModName m (dL -> L _ (IEModuleContents _ (dL -> L _ n))) = GHC.moduleNameString n == m
          matchesModName _ _ = False

exportHint _ _ = []

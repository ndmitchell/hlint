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

module Hint.Export(exportHint) where

import Hint.Type
import "ghc-lib-parser" HsSyn
import qualified "ghc-lib-parser" Module as GHC
import "ghc-lib-parser" SrcLoc as GHC
import "ghc-lib-parser" OccName
import "ghc-lib-parser" RdrName

exportHint :: ModuHint
exportHint _ (ModuleEx _ _ (L s m@HsModule {hsmodName = Just name, hsmodExports = exports}) _)
  | Nothing <- exports =
      let r = o{ hsmodExports = Just (GHC.noLoc [GHC.noLoc (IEModuleContents noExt name)] )} in
      [(ignore' "Use module export list" (L s o) (GHC.noLoc r) []){ideaNote = [Note "an explicit list is usally better"]}]
  | Just (L _ xs) <- exports
  , mods <- [x | x <- xs, isMod x]
  , modName <- GHC.moduleNameString (GHC.unLoc name)
  , names <- [GHC.moduleNameString (GHC.unLoc n) | (L _ (IEModuleContents _ n)) <- mods]
  , exports' <- [x | x <- xs, not (matchesModName modName x)]
  , modName `elem` names =
      let dots = mkRdrUnqual (mkVarOcc " ... ")
          r = o{ hsmodExports = Just (GHC.noLoc (GHC.noLoc (IEVar noExt (GHC.noLoc (IEName (GHC.noLoc dots)))) : exports') )}
      in
        [ignore' "Use explicit module export list" (L s o) (GHC.noLoc r) []]
      where
          o = m{hsmodImports=[], hsmodDecls=[], hsmodDeprecMessage=Nothing, hsmodHaddockModHeader=Nothing }
          isMod (L _ (IEModuleContents _ _)) = True
          isMod _ = False

          matchesModName m (L _ (IEModuleContents _ (L _ n))) = GHC.moduleNameString n == m
          matchesModName _ _ = False

exportHint _ _ = []

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
{-# LANGUAGE TypeFamilies #-}

module Hint.Export(exportHint) where

import Hint.Type(ModuHint, ModuleEx(..),ideaNote,ignore',Note(..))

import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" Module
import "ghc-lib-parser" SrcLoc
import "ghc-lib-parser" OccName
import "ghc-lib-parser" RdrName

exportHint :: ModuHint
exportHint _ (ModuleEx _ _ (LL s m@HsModule {hsmodName = Just name, hsmodExports = exports}) _)
  | Nothing <- exports =
      let r = o{ hsmodExports = Just (noLoc [noLoc (IEModuleContents noExt name)] )} in
      [(ignore' "Use module export list" (L s o) (noLoc r) []){ideaNote = [Note "an explicit list is usally better"]}]
  | Just (L _ xs) <- exports
  , mods <- [x | x <- xs, isMod x]
  , modName <- moduleNameString (unLoc name)
  , names <- [ moduleNameString (unLoc n) | (LL _ (IEModuleContents _ n)) <- mods]
  , exports' <- [x | x <- xs, not (matchesModName modName x)]
  , modName `elem` names =
      let dots = mkRdrUnqual (mkVarOcc " ... ")
          r = o{ hsmodExports = Just (noLoc (noLoc (IEVar noExt (noLoc (IEName (noLoc dots)))) : exports') )}
      in
        [ignore' "Use explicit module export list" (L s o) (noLoc r) []]
      where
          o = m{hsmodImports=[], hsmodDecls=[], hsmodDeprecMessage=Nothing, hsmodHaddockModHeader=Nothing }
          isMod (LL _ (IEModuleContents _ _)) = True
          isMod _ = False

          matchesModName m (LL _ (IEModuleContents _ (L _ n))) = moduleNameString n == m
          matchesModName _ _ = False

exportHint _ _ = []

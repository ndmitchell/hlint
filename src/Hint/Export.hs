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
{-# LANGUAGE TypeFamilies #-}

module Hint.Export(exportHint) where

import Hint.Type(ModuHint, ModuleEx(..),ideaNote,ignore,Note(..))

import GHC.Hs
import GHC.Unit.Module
import GHC.Types.SrcLoc
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader

exportHint :: ModuHint
exportHint _ (ModuleEx (L s m@HsModule {hsmodName = Just name, hsmodExports = exports}) )
  | Nothing <- exports =
      let r = o{ hsmodExports = Just (noLocA [noLocA (IEModuleContents EpAnnNotUsed name)] )} in
      [(ignore "Use module export list" (L s o) (noLoc r) []){ideaNote = [Note "an explicit list is usually better"]}]
  | Just (L _ xs) <- exports
  , mods <- [x | x <- xs, isMod x]
  , modName <- moduleNameString (unLoc name)
  , names <- [ moduleNameString (unLoc n) | (L _ (IEModuleContents _ n)) <- mods]
  , exports' <- [x | x <- xs, not (matchesModName modName x)]
  , modName `elem` names =
      let dots = mkRdrUnqual (mkVarOcc " ... ")
          r = o{ hsmodExports = Just (noLocA (noLocA (IEVar noExtField (noLocA (IEName (noLocA dots)))) : exports') )}
      in
        [ignore "Use explicit module export list" (L s o) (noLoc r) []]
      where
          o = m{hsmodImports=[], hsmodDecls=[], hsmodDeprecMessage=Nothing, hsmodHaddockModHeader=Nothing }
          isMod (L _ (IEModuleContents _ _)) = True
          isMod _ = False

          matchesModName m (L _ (IEModuleContents _ (L _ n))) = moduleNameString n == m
          matchesModName _ _ = False

exportHint _ _ = []

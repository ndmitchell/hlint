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

module Hint.Export(exportHint) where

import Hint.Type


exportHint :: ModuHint
exportHint _ (Module _ (Just o@(ModuleHead a name warning exports)) _ _ _)
    | Nothing <- exports =
        let o2 = ModuleHead a name warning $ Just $ ExportSpecList a [EModuleContents a name]
        in [(ignore "Use module export list" o o2 []){ideaNote = [Note "An explicit list is usually better"]}]
    | Just (ExportSpecList _ xs) <- exports, EModuleContents a name `elem_` xs =
        let o2 = ModuleHead a name warning $ Just $ ExportSpecList a $ EVar a ellipses : delete_ (EModuleContents a name) xs
        in [ignore "Use explicit module export list" o o2 []]
exportHint _ _ = []

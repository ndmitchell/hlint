
module Hint.Type(module Hint.Type, module Idea, module HSE.All) where

import HSE.All
import Idea


type DeclHint = Scope -> Module_ -> Decl_ -> [Idea]
type ModuHint = Scope -> Module_          -> [Idea]
type CrossHint = [(Scope, Module_)] -> [Idea]

data Hint = DeclHint {declHint :: DeclHint}
          | ModuHint {moduHint :: ModuHint}
          | CrossHint {crossHint :: CrossHint}

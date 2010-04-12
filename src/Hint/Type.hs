
module Hint.Type(module Hint.Type, module Idea, module HSE.All) where

import HSE.All
import Idea


type DeclHint = NameMatch -> Module_ -> Decl_ -> [Idea]
type ModuHint = NameMatch -> Module_          -> [Idea]

data Hint = DeclHint {declHint :: DeclHint} | ModuHint {moduHint :: ModuHint}

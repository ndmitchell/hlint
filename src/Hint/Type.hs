
module Hint.Type where

import HSE.All
import Type


type DeclHint = NameMatch -> Module_ -> Decl_ -> [Idea]
type ModuHint = NameMatch -> Module_          -> [Idea]

data Hint = DeclHint {declHint :: DeclHint} | ModuHint {moduHint :: ModuHint}

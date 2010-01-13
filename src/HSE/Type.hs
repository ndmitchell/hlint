
module HSE.Type(
    module HSE.Type, module Language.Haskell.Exts.Annotated,
    module Data.Generics, module Data.Generics.Uniplate.Data
    ) where

import Language.Haskell.Exts.Annotated hiding (parse, loc, parseFile, paren)
import Data.Generics hiding (Infix, Fixity)
import Data.Generics.Uniplate.Data

type S = SrcSpanInfo
type Module_ = Module S
type Decl_ = Decl S
type Exp_ = Exp S
type Pat_ = Pat S
type Type_ = Type S


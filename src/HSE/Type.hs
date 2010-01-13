
module HSE.Type(module HSE.Type, module Export) where

import Language.Haskell.Exts.Annotated as Export hiding (parse, loc, parseFile, paren)
import Data.Generics.Uniplate.Data as Export

type S = SrcSpanInfo
type Module_ = Module S
type Decl_ = Decl S
type Exp_ = Exp S
type Pat_ = Pat S
type Type_ = Type S

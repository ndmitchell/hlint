
module HSE.Type(module HSE.Type, module Export) where

-- Almost all from the Annotated module, but the fixity resolution from Annotated
-- uses the unannotated Assoc enumeration, so export that instead
import Language.Haskell.Exts as Export hiding (parse, loc, paren)
import Data.Generics.Uniplate.Data as Export

type S = SrcSpanInfo
type Module_ = Module S
type Decl_ = Decl S
type Exp_ = Exp S
type Pat_ = Pat S
type Type_ = Type S

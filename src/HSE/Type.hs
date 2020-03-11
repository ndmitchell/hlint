
module HSE.Type(
    S,
    module HSE,
    module Uniplate
    ) where

-- Almost all from the Annotated module, but the fixity resolution from Annotated
-- uses the unannotated Assoc enumeration, so export that instead
import Language.Haskell.Exts as HSE hiding (parse, loc, paren)
import Data.Generics.Uniplate.Data as Uniplate

type S = SrcSpanInfo

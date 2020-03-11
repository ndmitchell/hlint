
module HSE.Type(
    module HSE,
    ) where

-- Almost all from the Annotated module, but the fixity resolution from Annotated
-- uses the unannotated Assoc enumeration, so export that instead
import Language.Haskell.Exts as HSE hiding (parse, loc, paren)


import HLint.HLint

-- Hints that apply only to the HLint source code

error = QVarOp a (UnQual b x) ==> toNamed x
error = QConOp a (UnQual b x) ==> toNamed x
error = UnQual a (Symbol b x) ==> toNamed x
error = UnQual a (Ident  b x) ==> toNamed x

-- deliberately creates an Ident with "...", which should be a symbol
ignore "Use toNamed" = Hint.Naming.shorten

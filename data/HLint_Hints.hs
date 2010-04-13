
import HLint.HLint

-- Hints that apply only to the HLint source code

error = QVarOp a (UnQual b x) ==> toNamed x
error = QConOp a (UnQual b x) ==> toNamed x
error = UnQual a (Symbol b x) ==> toNamed x
error = UnQual a (Ident  b x) ==> toNamed x
error = idea Error ==> err
error = idea Warning ==> warn


-- deliberately creates an Ident with "...", which should be a symbol
ignore "Use toNamed" = Hint.Naming.shorten
-- would require module cycles
ignore "Use toNamed" = HSE.Util

-- good for beginners, but I sometimes use it to increase symmetry
ignore "Use let"

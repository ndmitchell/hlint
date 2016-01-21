
import "hint" HLint.HLint

-- Hints that apply only to the HLint source code

error = QVarOp a (UnQual b x) ==> toNamed x
error = QConOp a (UnQual b x) ==> toNamed x
error = UnQual a (Symbol b x) ==> toNamed x
error = UnQual a (Ident  b x) ==> toNamed x
error = idea Warning ==> warn
error = idea Suggestion ==> suggest


-- deliberately creates an Ident with "...", which should be a symbol
ignore "Use toNamed" = Hint.Naming.shorten
-- would require module cycles
ignore "Use toNamed" = HSE.Util

-- good for beginners, but I sometimes use it to increase symmetry
ignore "Use let"

-- I do this anyway where it makes sense
ignore "Eta reduce"

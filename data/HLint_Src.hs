-- Hints that apply only to the HLint source code

error = QVarOp (UnQual x) ==> toNamed x
error = QConOp (UnQual x) ==> toNamed x
error = UnQual (Symbol x) ==> toNamed x
error = UnQual (Ident x) ==> toNamed x

-- deliberately creates an Ident with "...", which should be a symbol
ignore "Use toNamed" = Hint.Naming.shorten

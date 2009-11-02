-- Hints that apply only to the HLint source code

error = QVarOp (UnQual x) ==> toNamed x
error = UnQual (Symbol x) ==> toNamed x

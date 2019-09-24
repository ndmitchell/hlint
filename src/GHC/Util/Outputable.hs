
module GHC.Util.Outputable (unsafePrettyPrint) where

import Outputable

-- \"Unsafe\" in this case means that it uses the following
-- 'DynFlags' for printing -
-- <http://hackage.haskell.org/package/ghc-lib-parser-8.8.0.20190424/docs/src/DynFlags.html#v_unsafeGlobalDynFlags
-- unsafeGlobalDynFlags> This could lead to the issues documented
-- there, but it also might not be a problem for our use case.  TODO:
-- Decide whether this really is unsafe, and if it is, what needs to
-- be done to make it safe.
unsafePrettyPrint :: (Outputable.Outputable a) => a -> String
unsafePrettyPrint = Outputable.showSDocUnsafe . Outputable.ppr

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HSE.Scope(
    Scope, scopeCreate, scopeImports
    ) where

import Data.Semigroup
import HSE.Type
import HSE.Util
import Data.List
import Data.Maybe
import Prelude

{-
the hint file can do:

import Prelude (filter)
import Data.List (filter)
import List (filter)

then filter on it's own will get expanded to all of them

import Data.List
import List as Data.List


if Data.List.head x ==> x, then that might match List too
-}


-- | Data type representing the modules in scope within a module.
--   Created with 'scopeCreate' and queried with 'scopeMatch' and 'scopeMove'.
--   Note that the 'mempty' 'Scope' is not equivalent to 'scopeCreate' on an empty module,
--   due to the implicit import of 'Prelude'.
newtype Scope = Scope [ImportDecl S]
             deriving (Show, Monoid, Semigroup)

-- | Create a 'Scope' value from a module, based on the modules imports.
scopeCreate :: Module SrcSpanInfo -> Scope
scopeCreate xs = Scope $ [prelude | not $ any isPrelude res] ++ res
    where
        res = [x | x <- moduleImports xs, importPkg x /= Just "hint"]
        prelude = ImportDecl an (ModuleName an "Prelude") False False False Nothing Nothing Nothing
        isPrelude x = fromModuleName (importModule x) == "Prelude"


scopeImports :: Scope -> [ImportDecl S]
scopeImports (Scope x) = x

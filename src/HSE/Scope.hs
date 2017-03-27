{-# LANGUAGE ViewPatterns #-}

module HSE.Scope(
    Scope, scopeCreate, scopeImports,
    scopeMatch, scopeMove
    ) where

import Data.Monoid
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
             deriving Show

instance Monoid Scope where
    mempty = Scope []
    mappend (Scope xs) (Scope ys) = Scope $ xs ++ ys

-- | Create a 'Scope' value from a module, based on the modules imports.
scopeCreate :: Module_ -> Scope
scopeCreate xs = Scope $ [prelude | not $ any isPrelude res] ++ res
    where
        res = [x | x <- moduleImports xs, importPkg x /= Just "hint"]
        prelude = ImportDecl an (ModuleName an "Prelude") False False False Nothing Nothing Nothing
        isPrelude x = fromModuleName (importModule x) == "Prelude"


scopeImports :: Scope -> [ImportDecl S]
scopeImports (Scope x) = x



-- | Given a two names in scopes, could they possibly refer to the same thing.
--   This property is reflexive.
scopeMatch :: (Scope, QName S) -> (Scope, QName S) -> Bool
scopeMatch (a, x@Special{}) (b, y@Special{}) = x =~= y
scopeMatch (a, x) (b, y) | isSpecial x || isSpecial y = False
scopeMatch (a, x) (b, y) = unqual x =~= unqual y && not (null $ possModules a x `intersect` possModules b y)


-- | Given a name in a scope, and a new scope, create a name for the new scope that will refer
--   to the same thing. If the resulting name is ambiguous, it picks a plausible candidate.
scopeMove :: (Scope, QName S) -> Scope -> QName S
scopeMove (a, x@(fromQual -> Just name)) (Scope b)
    | null imps = head $ real ++ [x]
    | any (not . importQualified) imps = unqual x
    | otherwise = Qual an (head $ mapMaybe importAs imps ++ map importModule imps) name
    where
        real = [Qual an (ModuleName an m) name | m <- possModules a x]
        imps = [i | r <- real, i <- b, possImport i r]
scopeMove (_, x) _ = x


-- which modules could a name possibly lie in
-- if it's qualified but not matching any import, assume the user
-- just lacks an import
possModules :: Scope -> QName S -> [String]
possModules (Scope is) x = f x
    where
        res = [fromModuleName $ importModule i | i <- is, possImport i x]

        f Special{} = [""]
        f x@(Qual _ mod _) = [fromModuleName mod | null res] ++ res
        f _ = res


possImport :: ImportDecl S -> QName S -> Bool
possImport i Special{} = False
possImport i (Qual _ mod x) = fromModuleName mod `elem` map fromModuleName ms && possImport i{importQualified=False} (UnQual an x)
    where ms = importModule i : maybeToList (importAs i)
possImport i (UnQual _ x) = not (importQualified i) && maybe True f (importSpecs i)
    where
        f (ImportSpecList _ hide xs) = if hide then Just True `notElem` ms else Nothing `elem` ms || Just True `elem` ms
            where ms = map g xs

        g :: ImportSpec S -> Maybe Bool -- does this import cover the name x
        g (IVar _ y) = Just $ x =~= y
        g (IAbs _ _ y) = Just $ x =~= y
        g (IThingAll _ y) = if x =~= y then Just True else Nothing
        g (IThingWith _ y ys) = Just $ x `elem_` (y : map fromCName ys)

        fromCName :: CName S -> Name S
        fromCName (VarName _ x) = x
        fromCName (ConName _ x) = x

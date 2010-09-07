
module HSE.NameMatch(
    Scope, emptyScope, moduleScope, scopeImports,
    NameMatch, nameMatch, nameQualify
    ) where

import HSE.Type
import HSE.Util
import Data.List
import Data.Maybe

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

type NameMatch = QName S -> QName S -> Bool


data Scope = Scope [ImportDecl S]
             deriving Show

moduleScope :: Module S -> Scope
moduleScope xs = Scope $ [prelude | not $ any isPrelude res] ++ res
    where
        res = [x | x <- moduleImports xs, importPkg x /= Just "hint"]
        prelude = ImportDecl an (ModuleName an "Prelude") False False Nothing Nothing Nothing
        isPrelude x = fromModuleName (importModule x) == "Prelude"


emptyScope :: Scope
emptyScope = Scope []


scopeImports :: Scope -> [ImportDecl S]
scopeImports (Scope x) = x



-- given A B x y, does A{x} possibly refer to the same name as B{y}
-- this property is reflexive
nameMatch :: Scope -> Scope -> NameMatch
nameMatch a b x@Special{} y@Special{} = x =~= y
nameMatch a b x y | isSpecial x || isSpecial y = False
nameMatch a b x y = unqual x =~= unqual y && not (null $ possModules a x `intersect` possModules b y)


-- given A B x, return y such that A{x} == B{y}, if you can
nameQualify :: Scope -> Scope -> QName S -> QName S
nameQualify a (Scope b) x
    | isSpecial x = x
    | null imps = head $ real ++ [x]
    | any (not . importQualified) imps = unqual x
    | otherwise = Qual an (head $ mapMaybe importAs imps ++ map importModule imps) $ fromQual x
    where
        real = [Qual an (ModuleName an m) $ fromQual x | m <- possModules a x]
        imps = [i | r <- real, i <- b, possImport i r]


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
        g (IAbs _ y) = Just $ x =~= y
        g (IThingAll _ y) = if x =~= y then Just True else Nothing
        g (IThingWith _ y ys) = Just $ x `elem_` (y : map fromCName ys)
        
        fromCName :: CName S -> Name S
        fromCName (VarName _ x) = x
        fromCName (ConName _ x) = x


module HSE.NameMatch(
    Scope, emptyScope, moduleScope, scopeImports,
    NameMatch, nameMatch, nameQualify
    ) where

import HSE.Type
import HSE.Util
import HSE.Match
import qualified Data.Map as Map
import Data.Maybe
import Util

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
nameMatch2 :: Scope -> Scope -> QName S -> QName S -> Bool
nameMatch2 a b x y = undefined -- unqual x == unqual y && not (null $ possModules a x `intersect` possModules b y)


-- given A B x, return y such that A{x} == B{y}, if you can
nameQualify :: Scope -> Scope -> QName S -> QName S
nameQualify a b x = f x
    where
        f (Qual _ mod x) | nameMatch a b (Qual an mod x) (UnQual an x) = UnQual an x
        f x = x
{-
    all those where the import allows it,
    then what qualification would be required (if none pick that)
    else go for the full name of the module
    possModules a x -}


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
possImport i (Qual _ mod x) = fromModuleName mod `elem` map fromModuleName ms && possImport i (UnQual an x)
    where ms = [importModule i | not $ importQualified i] ++ maybeToList (importAs i)
possImport i (UnQual _ x) = maybe True f $ importSpecs i
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


--------------------------------------------------------
-- OLD STUFF


type NameMatch = QName S -> QName S -> Bool

-- given A B x y, does B{y} perhaps refer to x
--
-- Given a list of import statements, are the names equal
-- The import statements are only in scope on the second name
--
-- If the left is unqualified, then the right is dequalified and checked for match
-- If the left is qualified, then the right is wrapped and name resolved
nameMatch :: Scope -> Scope -> NameMatch
nameMatch _ (Scope imps) = f
    where
        -- deal with "as" imports
        resolve :: ModuleName S -> ModuleName S
        resolve = \x -> Map.findWithDefault x (fromNamed x) mp
            where mp = Map.fromList [(fromNamed as, importModule i) | i <- imps, Just as <- [importAs i]]
        
        -- return True if x is potentially imported by B
        importedFrom :: ModuleName S -> Name S -> Bool
        importedFrom = \modu x ->
                any (g x) $ Map.findWithDefault [(True,[]) | fromNamed modu == "Prelude"] (fromNamed modu) mp
            where mp = Map.fromList $ groupSortFst
                       [(fromNamed $ importModule i, importSpecNames i) | i <- imps, not $ importQualified i]

                  g x (hide,y) = hide /= (x `elem_` y)

        f (Qual _ xm x) (Qual _ ym y) = x =~= y && xm =~= resolve ym
        f (Qual _ xm x) (UnQual _ y) = x =~= y && importedFrom xm x
        f (UnQual _ x) (Qual _ ym y) = x =~= y
        f x y = x =~= y


importSpecNames :: ImportDecl S -> (Bool, [Name S])
importSpecNames x = case importSpecs x of
    Nothing -> (True, [])
    Just (ImportSpecList _ b x) -> (b, concatMap f x)
    where
        f (IVar _ x) = [x]
        f (IAbs _ x) = [x]
        f (IThingAll _ x) = [x]
        f (IThingWith _ x ys) = x : map g ys
        g (VarName _ x) = x
        g (ConName _ x) = x


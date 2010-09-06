
module HSE.NameMatch(Scope, moduleScope, emptyScope, NameMatch, nameMatch) where

import HSE.Type
import HSE.Util
import HSE.Match
import qualified Data.Map as Map
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
moduleScope = Scope . moduleImports

emptyScope :: Scope
emptyScope = Scope []



-- given A B x y, does A{x} possibly refer to the same name as B{y}
-- this property is reflexive
nameMatch2 :: Scope -> Scope -> QName S -> QName S -> Bool
nameMatch2 a b x y = undefined -- unqual x == unqual y && not (null $ possModules a x `intersect` possModules b y)


-- given A B x, pick y such that A{x} == B{y}, if you can
nameQualify :: Scope -> Scope -> QName S -> QName S
nameQualify a x b = undefined {- 
    all those where the import allows it,
    then what qualification would be required (if none pick that)
    else go for the full name of the module
    possModules a x -}


-- which modules could a name possibly lie in
-- if it's qualified but not matching any import, assume the user
-- just lacks an import
possModules :: Scope -> QName S -> [String]
possModules = undefined



--------------------------------------------------------
-- OLD STUFF


type NameMatch = QName S -> QName S -> Bool


-- Given a list of import statements, are the names equal
-- The import statements are only in scope on the second name
--
-- If the left is unqualified, then the right is dequalified and checked for match
-- If the left is qualified, then the right is wrapped and name resolved
nameMatch :: [ImportDecl S] -> NameMatch
nameMatch imps = f
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


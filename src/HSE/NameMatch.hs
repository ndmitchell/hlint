
module HSE.NameMatch(Scope, moduleScope, emptyScope, NameMatch, nameMatch) where

import HSE.Type
import HSE.Util
import HSE.Match
import qualified Data.Map as Map
import Util


data Scope = Scope [ImportDecl S]
             deriving Show

moduleScope :: Module S -> Scope
moduleScope = Scope . moduleImports

emptyScope :: Scope
emptyScope = Scope []


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


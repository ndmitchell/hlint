
module HSE.NameMatch(NameMatch, nameMatch) where

import Language.Haskell.Exts
import qualified Data.Map as Map
import Control.Arrow
import Data.List
import Data.Function
import Data.Ord


type NameMatch = QName -> QName -> Bool


-- Given a list of import statements, are the names equal
-- The import statements are only in scope on the second name
--
-- If the left is unqualified, then the right is dequalified and checked for match
-- If the left is qualified, then the right is wrapped and name resolved
nameMatch :: [ImportDecl] -> NameMatch
nameMatch imps = f
    where
        -- deal with "as" imports
        resolve :: ModuleName -> ModuleName
        resolve = \x -> Map.findWithDefault x x mp
            where mp = Map.fromList [(as, importModule i) | i <- imps, Just as <- [importAs i]]
        
        -- return True if x is potentially imported by B
        importedFrom :: ModuleName -> Name -> Bool
        importedFrom = \modu x -> any (g x) $ Map.findWithDefault [(True,[]) | modu == ModuleName "Prelude"] modu mp
            where mp = Map.fromList $ map (fst . head &&& map snd) $
                            groupBy ((==) `on` fst) $ sortBy (comparing fst)
                            [(importModule i, importSpecNames i) | i <- imps, not $ importQualified i]

                  g x (hide,y) = hide /= (x `elem` y)
        

        f (Qual xm x) (Qual ym y) = x == y && xm == resolve ym
        f (Qual xm x) (UnQual y) = x == y && importedFrom xm x
        f x y = x == y


importSpecNames :: ImportDecl -> (Bool, [Name])
importSpecNames x = case importSpecs x of
    Nothing -> (True, [])
    Just (b, x) -> (b, concatMap f x)
    where
        f (IVar x) = [x]
        f (IAbs x) = [x]
        f (IThingAll x) = [x]
        f (IThingWith x ys) = x : map g ys
        g (VarName x) = x
        g (ConName x) = x


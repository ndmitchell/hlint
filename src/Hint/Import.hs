{-# LANGUAGE PatternGuards #-}
{-
    Reduce the number of import declarations.
    Two import declarations can be combined if:
      (note, A[] is A with whatever import list, or none)
    
    import A[]; import A[] = import A[]
    import A(B); import A(C) = import A(B,C)
    import A; import A(C) = import A
    import A; import A hiding (C) = import A
    import A[]; import A[] as Y = import A[] as Y

<TEST>
import A; import A -- import A
import A; import A; import A -- import A
import A(Foo) ; import A -- import A
import A(Bar(..)); import {-# SOURCE #-} A
import A; import B
import A(B) ; import A(C) -- import A(B,C)
import A; import A hiding (C) -- import A
import A; import A as Y -- import A as Y
import A; import qualified A as Y
import A as B; import A as C
import A; import B; import A -- import A
import qualified A; import A
import B; import A; import A -- import A
import A hiding(Foo); import A hiding(Bar)
</TEST>
-}


module Hint.Import where

import HSE.All
import Type
import Util
import Data.List
import Data.Maybe


importHint :: ModuHint
importHint _ x = concatMap (wrap . snd) $ groupSortFst
                 [((importModule i,importPkg i),i) | i <- universeBi x, not $ importSrc i]


wrap :: [ImportDecl] -> [Idea]
wrap o = [ rawIdea Error "Use fewer imports" (importLoc $ head x) (f o) (f x)
         | Just x <- [simplify o]]
    where f = unlines . map prettyPrint


simplify :: [ImportDecl] -> Maybe [ImportDecl]
simplify [] = Nothing
simplify (x:xs) = case simplifyHead x xs of
    Nothing -> fmap (x:) $ simplify xs
    Just xs -> Just $ fromMaybe xs $ simplify xs


simplifyHead :: ImportDecl -> [ImportDecl] -> Maybe [ImportDecl]
simplifyHead x [] = Nothing
simplifyHead x (y:ys) = case reduce x y of
    Nothing -> fmap (y:) $ simplifyHead x ys
    Just xy -> Just $ xy : ys


-- Useful fields in import are:
-- importModule :: ModuleName [same]
-- importPkg :: Maybe String [same]
-- importQualified :: Bool
-- importSrc :: Bool [False]
-- importAs :: Maybe ModuleName
-- importSpecs :: Maybe (Bool, [ImportSpec])

reduce :: ImportDecl -> ImportDecl -> Maybe ImportDecl
reduce x y | qual, as, specs = Just x
           | qual, as, Just (False, xs) <- importSpecs x, Just (False, ys) <- importSpecs y =
                Just x{importSpecs = Just (False, nub $ xs ++ ys)}
           | qual, as, isNothing (importSpecs x) || isNothing (importSpecs y) = Just x{importSpecs=Nothing}
           | not (importQualified x), qual, specs, length ass == 1 = Just x{importAs=Just $ head ass}
    where
        qual = importQualified x == importQualified y
        as = importAs x == importAs y
        ass = mapMaybe importAs [x,y]
        specs = importSpecs x == importSpecs y

reduce _ _ = Nothing

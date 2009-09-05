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
</TEST>
-}


module Hint.Import where

import HSE.All
import Type
import Data.List
import Data.Maybe
import Data.Function


importHint :: ModuHint
importHint _ x = concatMap (wrap . map snd) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst)
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
           | not (importQualified x), qual, specs, isNothing (importAs x) || isNothing (importAs y) = Just x{importAs=Nothing}
    where
        qual = importQualified x == importQualified y
        as = importAs x == importAs y
        specs = importSpecs x == importSpecs y

reduce _ _ = Nothing

{-
    Reduce the number of import declarations.
    Two import declarations can be combined if:
      (note, A[] is A with whatever import list, or none)
    
    import A(B); import A(C) = import A(B,C)
    import A; import A(C) = import A
    import A[]; import A[] as Y = import A[] as Y
    import A[]; import A[] = import A[]

<TEST>
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


reduce :: ImportDecl -> ImportDecl -> Maybe ImportDecl
reduce x y | x == y{importLoc=importLoc x} = Just x
reduce _ _ = Nothing

{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Find and match:

    1:2:[] -> [1,2]
    ['a':'b'] -> "ab"
    [a]++b -> a : b, but only if not in a chain of ++'s
    "c"++x -> 'c':x, but only if not in a chain of ++'s
-}


module Hint.List where

import Util
import Type
import Language.Haskell.Exts


listHint :: Hint
listHint = listDecl

listDecl :: HsDecl -> [Idea]
listDecl = concatMap (listExp False) . children0Exp nullSrcLoc

-- boolean = are you in a ++ chain
listExp :: Bool -> (SrcLoc,HsExp) -> [Idea]
listExp b (loc,x) =
        if null res then concatMap (listExp $ isAppend x) $ children1Exp loc x else [head res]
    where
        res = [idea name loc x x2 | (name,f) <- checks, Just x2 <- [f b x]]


isAppend (view -> App2 op _ _) = op ~= "++"
isAppend _ = False


checks = let (*) = (,) in
         ["Use a string literal" * useString
         ,"Use a list literal" * useList
         ,"Use (:)" * useCons
         ]


useString b (HsList xs) | not (null xs) && all isCharExp xs = Just $ HsLit $ HsString [x | HsLit (HsChar x) <- xs]
useString b _ = Nothing

useList b = fmap HsList . f True
    where
        f first (view -> Nil) = if first then Nothing else Just []
        f first (view -> Cons a b) = fmap (a:) $ f False b
        f first _ = Nothing

useCons False (view -> App2 op x y) | op ~= "++", Just x2 <- f x = Just $ HsInfixApp x2 (HsQConOp list_cons_name) y
    where
        f (HsLit (HsString [x])) = Just $ HsLit $ HsChar x
        f (HsList [x]) = Just x
        f _ = Nothing
useCons _ _ = Nothing

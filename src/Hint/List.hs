{-# LANGUAGE ViewPatterns #-}

{-
    Find and match:

    1:2:[] -> [1,2]
    ['a':'b'] -> "ab"
    [a]++b -> a : b, but only if not in a chain of ++'s
    "c"++x -> 'c':x, but only if not in a chain of ++'s
-}


module Hint.List where

import Hint.Util
import Hint.Type
import Language.Haskell.Exts


listHint :: Hint
listHint = listDecl

listDecl :: HsDecl -> [Idea]
listDecl = concatMap listExp . children0Exp nullSrcLoc

listExp :: (SrcLoc,HsExp) -> [Idea]
listExp (loc,x) =
        if null res then concatMap listExp $ children1Exp loc x else [head res]
    where
        res = [Idea name loc (Just $ prettyPrint x) (Just $ prettyPrint x2) | (name,f) <- checks, Just x2 <- [f x]]


checks = let (*) = (,) in
         ["Use a string literal" * useString
         ,"Use a list literal" * useList
         ]


useString (HsList xs) | not (null xs) && all isCharExp xs = Just $ HsLit $ HsString [x | HsLit (HsChar x) <- xs]
useString _ = Nothing

useList = fmap HsList . f True
    where
        f first (view -> Nil) = if first then Nothing else Just []
        f first (view -> Cons a b) = fmap (a:) $ f False b
        f first _ = Nothing

{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Find and match:

<TEST>
yes = 1:2:[] where res = [1,2]
yes = ['h','e','l','l','o'] where res = "hello"

-- [a]++b -> a : b, but only if not in a chain of ++'s
yes = [x] ++ xs where res = x : xs
yes = "x" ++ xs where res = 'x' : xs
no = [x] ++ xs ++ ys
no = xs ++ [x] ++ ys
yes = [if a then b else c] ++ xs where res = (if a then b else c) : xs
</TEST>
-}


module Hint.List where

import HSE.All
import Type


listHint :: Hint
listHint = listDecl

listDecl :: Decl -> [Idea]
listDecl = concatMap (listExp False) . children0Exp nullSrcLoc

-- boolean = are you in a ++ chain
listExp :: Bool -> (SrcLoc,Exp) -> [Idea]
listExp b (loc,x) =
        if null res then concatMap (listExp $ isAppend x) $ children1Exp loc x else [head res]
    where
        res = [idea name loc x x2 | (name,f) <- checks, Just x2 <- [f b x]]


isAppend (view -> App2 op _ _) = op ~= "++"
isAppend _ = False


checks = let (*) = (,) in
         ["Use string literal" * useString
         ,"Use list literal" * useList
         ,"Use :" * useCons
         ]


useString b (List xs) | xs /= [] && all isChar xs = Just $ Lit $ String $ map fromChar xs
useString b _ = Nothing

useList b = fmap List . f True
    where
        f first x | x ~= "[]" = if first then Nothing else Just []
        f first (view -> App2 c a b) | c ~= ":" = fmap (a:) $ f False b
        f first _ = Nothing

useCons False (view -> App2 op x y) | op ~= "++", Just x2 <- f x, not $ isAppend y =
        Just $ InfixApp x2 (QConOp list_cons_name) y
    where
        f (Lit (String [x])) = Just $ Lit $ Char x
        f (List [x]) = Just $ paren x
        f _ = Nothing
useCons _ _ = Nothing

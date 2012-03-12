{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Find and match:

<TEST>
yes = 1:2:[] -- [1,2]
yes = ['h','e','l','l','o'] -- "hello"

-- [a]++b -> a : b, but only if not in a chain of ++'s
yes = [x] ++ xs -- x : xs
yes = "x" ++ xs -- 'x' : xs
no = [x] ++ xs ++ ys
no = xs ++ [x] ++ ys
yes = [if a then b else c] ++ xs -- (if a then b else c) : xs
yes = [1] : [2] : [3] : [4] : [5] : [] -- [[1], [2], [3], [4], [5]]
yes = if x == e then l2 ++ xs else [x] ++ check_elem xs -- x : check_elem xs
data Yes = Yes (Maybe [Char]) -- Maybe String
yes = y :: [Char] -> a -- String -> a
instance C [Char]
foo = [a b] ++ xs -- a b : xs
</TEST>
-}


module Hint.List where

import Hint.Type


listHint :: DeclHint
listHint _ _ = listDecl

listDecl :: Decl_ -> [Idea]
listDecl x = concatMap (listExp False) (childrenBi x) ++ stringType x

-- boolean = are you in a ++ chain
listExp :: Bool -> Exp_ -> [Idea]
listExp b (fromParen -> x) =
        if null res then concatMap (listExp $ isAppend x) $ children x else [head res]
    where
        res = [warn name x x2 | (name,f) <- checks, Just x2 <- [f b x]]


isAppend (view -> App2 op _ _) = op ~= "++"
isAppend _ = False


checks = let (*) = (,) in
         ["Use string literal" * useString
         ,"Use list literal" * useList
         ,"Use :" * useCons
         ]


useString b (List _ xs) | xs /= [] && all isChar xs = Just $ Lit an $ String an s (show s)
    where s = map fromChar xs
useString b _ = Nothing

useList b = fmap (List an) . f True
    where
        f first x | x ~= "[]" = if first then Nothing else Just []
        f first (view -> App2 c a b) | c ~= ":" = fmap (a:) $ f False b
        f first _ = Nothing

useCons False (view -> App2 op x y) | op ~= "++", Just x2 <- f x, not $ isAppend y =
        Just $ InfixApp an x2 (QConOp an $ list_cons_name an) y
    where
        f (Lit _ (String _ [x] _)) = Just $ Lit an $ Char an x (show x)
        f (List _ [x]) = Just $ if isApp x then x else paren x
        f _ = Nothing
useCons _ _ = Nothing



typeListChar = TyList an (TyCon an (toNamed "Char"))
typeString = TyCon an (toNamed "String")


stringType :: Decl_ -> [Idea]
stringType x = case x of
    InstDecl _ _ _ x -> f x
    _ -> f x
    where
        f x = concatMap g $ childrenBi x

        g :: Type_ -> [Idea]
        g (fromTyParen -> x) = [warn "Use String" x (transform f x) | any (=~= typeListChar) $ universe x]
            where f x = if x =~= typeListChar then typeString else x

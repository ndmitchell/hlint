
module Sample where

import Data.List

myfunc f = concat . map f

myfunc2 f g x = map f (map g x)


myfunc3 e l' = concat.map (\x->if x==e then l' else [x])

myfunc4 e l2 (x:xs) = if x == e then (l2 ++ xs) else [x] ++ check_elem xs
    where check_elem x = undefined


myfunc5 x = (x !! 0) + (x !! 2)


myfunc6 x = f x
    where f x = concat . map head

myfunc7 f g = concat . map f . g

myfunc8 f x = concat $ map f x

myfunc10 = "test" ++ concatMap (' ':) ["of","this"]

myfunc11 = concat . intersperse " "

myfunc12 f a = if f a then True else False

myfunc13 f a = if f a then False else True

myfunc14 a b = not (a == b)

myfunc14_b a b = not (a /= b)

myfunc15 a b = if a then 1 else if b then 1 else 2

myfunc15_NO a b = if a then 1 else if b then 3 else 2

myfunc16 a = a >>= return . id

myfunc17 a b = if b < 42 then [a] else []

myfunc18 a = ['h','e','l','l','o']

{-
-- out of scope for the new version
myfunc18 (x:xs) = x

myfunc19 f (x:xs) = f x : myfunc19 f xs
myfunc19 f [] = []

myfunc20 (x:xs) = head x : myfunc20 xs
myfunc20 [] = []

myfunc21 [] = 0 :: Int
myfunc21 (x:xs) = x + myfunc21 xs

myfunc22 x = rev [] x
    where
        rev acc [] = acc
        rev acc (x:xs) = rev (x:acc) xs

myfunc23 x blah = case x of
                      Just y -> y
                      Nothing -> blah

myfunc24 f x y = mapM f x >> return y
-}

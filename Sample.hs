
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

myfunc9 f g x = concat $ map f $ g x

myfunc10 = "test" ++ concatMap (' ':) ["of","this"]

myfunc11 = concat . intersperse " "


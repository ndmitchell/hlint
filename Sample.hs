
module Sample where

myfunc f = concat . map f

myfunc2 f g x = map f (map g x)


myfunc3 e l' = concat.map (\x->if x==e then l' else [x])

myfunc4 e l2 (x:xs) = if x == e then (l2 ++ xs) else [x] ++ check_elem xs
    where check_elem x = undefined

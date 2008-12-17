

-- LIST

hint = concat (map f x) ==> concatMap f x
hint "Use one map" = map f (map g x) ==> map (f . g) x
hint = (x !! 0) ==> head x
hint = take n (repeat x) ==> replicate n x
hint = (x ++ concatMap (' ':) y) ==> unwords (x:y)
hint = concat (intersperse " " x) ==> unwords x
hint = head (reverse x) ==> last x
hint "Use index" = head (drop n x) ==> (x !! n)
hint = reverse (tail (reverse x)) ==> init x
hint = isPrefixOf (reverse x) (reverse y) ==> isSuffixOf x y

-- BOOL

hint = not (a == b) ==> (a /= b)
hint = not (a /= b) ==> (a == b)
hint "Redundant if" = (if a then True else False) ==> a
hint "Redundant if" = (if a then False else True) ==> not a
hint "Redundant if" = (if a then t else (if b then t else f)) ==> if a || b then t else f
hint "Redundant if" = (if a then (if b then t else f) else f) ==> if a && b then t else f
hint "Use if" = case a of {True -> t; False -> f} ==> if a then t else f
hint "Use if" = case a of {True -> t; _ -> f} ==> if a then t else f
hint "Use if" = case a of {False -> f; _ -> t} ==> if a then t else f

-- MONAD

hint = m >>= return . f ==> liftM f m

-- LIST COMP

hint "Use a list comprehension" = (if b then [x] else []) ==> [x | b]

-- SEQ

hint "The seq is redundant" = (x `seq` x) ==> x
hint "The $! is redundant" = (id $! x) ==> x

-- MAYBE

hint = maybe x id  ==> fromMaybe x
hint = maybe False (const True) ==> isJust
hint = maybe True (const False) ==> isNothing

-- COMPLEX

hint "Use isPrefixOf, and then remove the (==) test" = (take i s == t) ==> ((i == length t) && (t `isPrefixOf` s))
    where _ = (isList t || isLit t) && isLit i


{-
<TEST>
yes = concat . map f
yes = foo . bar . concat . map f . baz . bar
yes = map f (map g x)
yes = concat.map (\x->if x==e then l' else [x])
yes = f x where f x = concat . map head
yes = concat . map f . g
yes = concat $ map f x
yes = "test" ++ concatMap (' ':) ["of","this"]
yes = concat . intersperse " "
yes = if f a then True else False
yes = if f a then False else True
yes = not (a == b)
yes = not (a /= b)
yes = if a then 1 else if b then 1 else 2
no  = if a then 1 else if b then 3 else 2
yes = a >>= return . id
yes = (x !! 0) + (x !! 2)
yes = if x == e then l2 ++ xs else [x] ++ check_elem xs
yes = if b < 42 then [a] else []
yes = take 5 (foo xs) == "hello"
no  = take n (foo xs) == "hello"
yes = head (reverse xs)
yes = reverse xs `isPrefixOf` reverse ys
</TEST>
-}


{-
-- TODO: Add RecMatch for things like map/foldr etc, with a similar entry in hints

-- more complicated, saved for later

-- map f x
redefined_map = mop
    where
        mop f (x:xs) = f x : mop f xs
        mop f [] = []

-- map<f> x
special_map f = mop
    where
        mop (x:xs) = f x : mop xs
        mop [] = []


-- foldr f z x
special_foldr f z = fold
    where
        fold [] = z
        fold (x:xs) = f x (fold xs)

-- foldl f z x
special_foldl1 f = fold
    where
        fold acc [] = acc
        fold acc (x:xs) = fold (f x acc) xs

special_foldl2 f = fold
    where
        fold [] acc = acc
        fold (x:xs) acc = fold xs (f x acc)

-}

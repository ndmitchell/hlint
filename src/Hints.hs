

-- LIST

hint = concat (map f x) ==> concatMap f x
hint "Use one map" = map f (map g x) ==> map (f . g) x
hint "Use (:)" = ([x] ++ y) ==> (x : y)
hint = (x !! 0) ==> head x
hint = take n (repeat x) ==> replicate n x
hint = (x ++ concatMap (' ':) y) ==> unwords (x:y)
hint = concat (intersperse " " x) ==> unwords x

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


{-
-- more complicated, saved for later

-- head x
head_head err (x:xs) = x
head_head err [] = error err

-- fromJust x
use_fromJust err (Just x) = x
use_fromJust err Nothing = error err


-- fromMaybe def x
use_fromMaybe def (Just x) = x
use_fromMaybe def Nothing = def

-- listMaybe x
use_listMaybe [] = Nothing
use_listMaybe (x:xs) = Just x

-- maybeToList x
use_maybeToList (Just x) = [x]
use_maybeToList Nothing = []

-- mapM_ f x >> y
use_mapM_ f x y = mapM f x >> y


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

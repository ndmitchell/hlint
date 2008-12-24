-- I/O

hint = putStrLn (show x) ==> print x
hint = mapM_ putChar ==> putStr

-- ORD

hint = compare x y /= GT ==> x <= y
hint = compare x y == LT ==> x < y
hint = compare x y /= LT ==> x >= y
hint = compare x y == GT ==> x > y

-- READ/SHOW

hint = showsPrec 0 x "" ==> show x
hint = readsPrec 0 ==> reads
hint = showsPrec 0 ==> shows

-- LIST

hint = concat (map f x) ==> concatMap f x
hint "Use one map" = map f (map g x) ==> map (f . g) x
hint = x !! 0 ==> head x
hint = take n (repeat x) ==> replicate n x
hint = x ++ concatMap (' ':) y ==> unwords (x:y)
hint = concat (intersperse " " x) ==> unwords x
hint = head (reverse x) ==> last x
hint "Use index" = head (drop n x) ==> x !! n
hint = reverse (tail (reverse x)) ==> init x
hint = isPrefixOf (reverse x) (reverse y) ==> isSuffixOf x y
hint = foldr (++) [] x ==> concat x
hint = span (not . p) ==> break p
hint = break (not . p) ==> span p
hint = concatMap (++ "\n") ==> unlines
hint = or (map p x) ==> any p x
hint = and (map p x) ==> all p x
hint = zipWith (,) ==> zip
hint = zipWith3 (,,) ==> zip3
hint = length x == 0 ==> null x
hint "Use null" = length x /= 0 ==> not (null x)

-- FOLDS

hint = foldr (&&) True ==> and
hint = foldr (>>) (return ()) ==> sequence_
hint = foldr (||) False ==> or
hint = foldl (+) 0 ==> sum
hint = foldl (*) 1 ==> product

-- BOOL

hint = not (a == b) ==> a /= b
hint = not (a /= b) ==> a == b
hint "Redundant if" = (if a then True else False) ==> a
hint "Redundant if" = (if a then False else True) ==> not a
hint "Redundant if" = (if a then t else (if b then t else f)) ==> if a || b then t else f
hint "Redundant if" = (if a then (if b then t else f) else f) ==> if a && b then t else f
hint "Redundant if" = (if x then True else y) ==> x || y
hint "Redundant if" = (if x then y else False) ==> x && y
hint "Use if" = case a of {True -> t; False -> f} ==> if a then t else f
hint "Use if" = case a of {True -> t; _ -> f} ==> if a then t else f
hint "Use if" = case a of {False -> f; _ -> t} ==> if a then t else f

-- ARROW

hint = id *** g ==> second g
hint = f *** id ==> first f

-- MONAD

hint = m >>= return . f ==> liftM f m
hint = (if x then y else return ()) ==> when x $ y
hint = sequence (map f as) ==> mapM f as
hint = sequence_ (map f as) ==> mapM_ f as

-- LIST COMP

hint "Use a list comprehension" = (if b then [x] else []) ==> [x | b]

-- SEQ

hint "The seq is redundant" = x `seq` x ==> x
hint "The $! is redundant" = id $! x ==> x

-- MAYBE

hint = maybe x id  ==> fromMaybe x
hint = maybe False (const True) ==> isJust
hint = maybe True (const False) ==> isNothing

-- MATHS

hint = x + negate y ==> x - y
hint = 0 - x ==> negate x
hint = log y / log x ==> logBase x y
hint = x ** 0.5 ==> sqrt x
hint = sin x / cos x ==> tan x
hint = sinh x / cosh x ==> tanh x
hint = n `rem` 2 == 0 ==> even n
hint = n `rem` 2 /= 0 ==> even n
hint = not (even x) ==> odd x
hint = not (odd x) ==> even x
hint "Use 1" = x ^ 0 ==> 1

-- EVALUATE

-- TODO: These should be moved in to HSE\Evaluate.hs and applied
--       through a special evaluate hint mechanism
hint "Evaluate" = True && x ==> x
hint "Evaluate" = False && x ==> False
hint "Evaluate" = True || x ==> True
hint "Evaluate" = False || x ==> x
hint "Evaluate" = not True ==> False
hint "Evaluate" = not False ==> True
hint "Evaluate" = Nothing >>= k ==> Nothing
hint "Evaluate" = either f g (Left x) ==> f x
hint "Evaluate" = either f g (Right y) ==> g y
hint "Evaluate" = fst (x,y) ==> x
hint "Evaluate" = snd (x,y) ==> y
hint "Evaluate" = f (fst p) (snd p) ==> uncurry f p
hint "Evaluate" = init [x] ==> []
hint "Evaluate" = null [] ==> True
hint "Evaluate" = length [] ==> 0
hint "Evaluate" = foldl f z [] ==> z
hint "Evaluate" = foldr f z [] ==> z
hint "Evaluate" = foldr1 f [x] ==> x
hint "Evaluate" = scanr f q0 [] ==> [q0]
hint "Evaluate" = scanr1 f [] ==> []
hint "Evaluate" = scanr1 f [x] ==> [x]
hint "Evaluate" = take n [] ==> []
hint "Evaluate" = drop n [] ==> []
hint "Evaluate" = takeWhile p [] ==> []
hint "Evaluate" = dropWhile p [] ==> []
hint "Evaluate" = span p [] ==> ([],[])
hint "Evaluate" = lines "" ==> []
hint "Evaluate" = unwords [] ==> ""
hint "Evaluate" = x - 0 ==> x
hint "Evaluate" = x * 1 ==> x
hint "Evaluate" = x / 1 ==> x
hint "Evaluate" = id x ==> x

-- COMPLEX

hint "Use isPrefixOf" = (take i s == t) ==> _eval_ ((i == length t) && (t `isPrefixOf` s))
    where _ = (isList t || isLit t) && isLit i

hint "Unnecessary $" = f $ x ==> f x
    where _ = isAtom x

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
yes = if f a then True else b
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
yes = operator foo $ operator
no = operator foo $ operator bar
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

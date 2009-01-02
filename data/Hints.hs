
-- Important hints:

-- I/O

fix = putStrLn (show x) ==> print x
fix = mapM_ putChar ==> putStr

-- ORD

fix = compare x y /= GT ==> x <= y
fix = compare x y == LT ==> x < y
fix = compare x y /= LT ==> x >= y
fix = compare x y == GT ==> x > y

-- READ/SHOW

fix = showsPrec 0 x "" ==> show x
fix = readsPrec 0 ==> reads
fix = showsPrec 0 ==> shows

-- LIST

fix = concat (map f x) ==> concatMap f x
fix "Use map once" = map f (map g x) ==> map (f . g) x
warn = x !! 0 ==> head x
fix = take n (repeat x) ==> replicate n x
fix = x ++ concatMap (' ':) y ==> unwords (x:y)
fix = concat (intersperse " " x) ==> unwords x
fix = head (reverse x) ==> last x
fix = head (drop n x) ==> x !! n
fix = reverse (tail (reverse x)) ==> init x
fix = isPrefixOf (reverse x) (reverse y) ==> isSuffixOf x y
fix = foldr (++) [] x ==> concat x
fix = span (not . p) ==> break p
fix = break (not . p) ==> span p
fix = concatMap (++ "\n") ==> unlines
fix = or (map p x) ==> any p x
fix = and (map p x) ==> all p x
fix = zipWith (,) ==> zip
fix = zipWith3 (,,) ==> zip3
warn = length x == 0 ==> null x
warn "Use null" = length x /= 0 ==> not (null x)
fix "Use :" = (\x -> [x]) ==> (:[])

-- FOLDS

fix = foldr (&&) True ==> and
fix = foldr (>>) (return ()) ==> sequence_
fix = foldr (||) False ==> or
fix = foldl (+) 0 ==> sum
warn = foldl (*) 1 ==> product

-- FUNCTION

fix = (\x -> x) ==> id
fix = (\(_,y) -> y) ==> snd
fix = (\(x,_) -> x) ==> fst
fix = (\x y-> f (x,y)) ==> curry f
fix = (\(x,y) -> f x y) ==> uncurry f

-- BOOL

fix = not (a == b) ==> a /= b
fix = not (a /= b) ==> a == b
fix "Redundant if" = (if a then True else False) ==> a
fix "Redundant if" = (if a then False else True) ==> not a
fix "Redundant if" = (if a then t else (if b then t else f)) ==> if a || b then t else f
fix "Redundant if" = (if a then (if b then t else f) else f) ==> if a && b then t else f
fix "Redundant if" = (if x then True else y) ==> x || y
fix "Redundant if" = (if x then y else False) ==> x && y
fix "Use if" = case a of {True -> t; False -> f} ==> if a then t else f
fix "Use if" = case a of {True -> t; _ -> f} ==> if a then t else f
fix "Use if" = case a of {False -> f; _ -> t} ==> if a then t else f

-- ARROW

warn = id *** g ==> second g
warn = f *** id ==> first f
warn = (\(x,y) -> (f x, g y)) ==> f *** g
warn = (\x -> (f x, g x)) ==> f &&& g
warn = (\(x,y) -> (f x,y)) ==> first f
warn = (\(x,y) -> (x,g y)) ==> second g

-- MONAD

fix "Monad law, left identity" = return a >>= f ==> f a
fix "Monad law, right identity" = m >>= return ==> m
warn = m >>= return . f ==> liftM f m
fix = (if x then y else return ()) ==> when x $ y
fix = sequence (map f as) ==> mapM f as
fix = sequence_ (map f as) ==> mapM_ f as

-- LIST COMP

warn "Use list comprehension" = (if b then [x] else []) ==> [x | b]

-- SEQ

fix "Redundant seq" = x `seq` x ==> x
fix "Redundant $!" = id $! x ==> x

-- MAYBE

fix = maybe x id  ==> fromMaybe x
fix = maybe False (const True) ==> isJust
fix = maybe True (const False) ==> isNothing

-- MATHS

warn = x + negate y ==> x - y
warn = 0 - x ==> negate x
warn = log y / log x ==> logBase x y
warn = x ** 0.5 ==> sqrt x
warn = sin x / cos x ==> tan x
warn = sinh x / cosh x ==> tanh x
warn = n `rem` 2 == 0 ==> even n
warn = n `rem` 2 /= 0 ==> odd n
warn = not (even x) ==> odd x
warn = not (odd x) ==> even x
warn "Use 1" = x ^ 0 ==> 1

-- EVALUATE

-- TODO: These should be moved in to HSE\Evaluate.hs and applied
--       through a special evaluate hint mechanism
fix "Evaluate" = True && x ==> x
fix "Evaluate" = False && x ==> False
fix "Evaluate" = True || x ==> True
fix "Evaluate" = False || x ==> x
fix "Evaluate" = not True ==> False
fix "Evaluate" = not False ==> True
fix "Evaluate" = Nothing >>= k ==> Nothing
fix "Evaluate" = either f g (Left x) ==> f x
fix "Evaluate" = either f g (Right y) ==> g y
fix "Evaluate" = fst (x,y) ==> x
fix "Evaluate" = snd (x,y) ==> y
fix "Evaluate" = f (fst p) (snd p) ==> uncurry f p
fix "Evaluate" = init [x] ==> []
fix "Evaluate" = null [] ==> True
fix "Evaluate" = length [] ==> 0
fix "Evaluate" = foldl f z [] ==> z
fix "Evaluate" = foldr f z [] ==> z
fix "Evaluate" = foldr1 f [x] ==> x
fix "Evaluate" = scanr f q0 [] ==> [q0]
fix "Evaluate" = scanr1 f [] ==> []
fix "Evaluate" = scanr1 f [x] ==> [x]
fix "Evaluate" = take n [] ==> []
fix "Evaluate" = drop n [] ==> []
fix "Evaluate" = takeWhile p [] ==> []
fix "Evaluate" = dropWhile p [] ==> []
fix "Evaluate" = span p [] ==> ([],[])
fix "Evaluate" = lines "" ==> []
fix "Evaluate" = unwords [] ==> ""
fix "Evaluate" = x - 0 ==> x
fix "Evaluate" = x * 1 ==> x
fix "Evaluate" = x / 1 ==> x
fix "Evaluate" = id x ==> x

-- COMPLEX

fix "Use isPrefixOf" = (take i s == t) ==> _eval_ ((i == length t) && (t `isPrefixOf` s))
    where _ = (isList t || isLit t) && isLit i

fix "Redundant $" = f $ x ==> f x
    where _ = isAtom x

warn = (do a <- f; g a) ==> f >>= g
    where _ = isAtom f || isApp f



{-
<TEST>
yes = concat . map f where res = concatMap f
yes = foo . bar . concat . map f . baz . bar where res = concatMap f . baz . bar
yes = map f (map g x) where res = map (f . g) x
yes = concat.map (\x->if x==e then l' else [x]) where res = concatMap (\x->if x==e then l' else [x])
yes = f x where f x = concat . map head
yes = concat . map f . g where res = concatMap f . g
yes = concat $ map f x where res = concatMap f x
yes = "test" ++ concatMap (' ':) ["of","this"] where res = unwords ("test":["of","this"])
yes = concat . intersperse " " where res = unwords
yes = if f a then True else b where res = f a || b
yes = not (a == b) where res = a /= b
yes = not (a /= b) where res = a == b
yes = if a then 1 else if b then 1 else 2 where res = if a || b then 1 else 2
no  = if a then 1 else if b then 3 else 2
yes = a >>= return . id where res = liftM id a
yes = (x !! 0) + (x !! 2) where res = head x
yes = if x == e then l2 ++ xs else [x] ++ check_elem xs where res = x : check_elem xs
yes = if b < 42 then [a] else [] where res = [a | b < 42]
yes = take 5 (foo xs) == "hello" where res = "hello" `isPrefixOf` foo xs
no  = take n (foo xs) == "hello"
yes = head (reverse xs) where res = last xs
yes = reverse xs `isPrefixOf` reverse ys where res = isSuffixOf xs ys
yes = operator foo $ operator where res = operator foo operator
no = operator foo $ operator bar
no = putStrLn $ show (length xs) ++ "Test"
yes = do line <- getLine; putStrLn line where res = getLine >>= putStrLn 
yes = ftable ++ map (\ (c, x) -> (toUpper c, urlEncode x)) ftable where res = toUpper *** urlEncode
yes = map (\(a,b) -> a) xs where res = fst
yes = map (\(a,_) -> a) xs where res = fst
yes = readFile $ args !! 0 where res = head args
yes = if Debug `elem` opts then ["--debug"] else [] where res = ["--debug" | Debug `elem` opts]
yes = if nullPS s then return False else if headPS s /= '\n' then return False else alter_input tailPS >> return True
    where res = if nullPS s || (headPS s /= '\n') then return False else alter_input tailPS >> return True
yes = if foo then do stuff; moreStuff; lastOfTheStuff else return ()
    where res = when foo $ (do stuff ; moreStuff ; lastOfTheStuff)
</TEST>
-}

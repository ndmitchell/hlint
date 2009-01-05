
-- Important hints:

-- I/O

error = putStrLn (show x) ==> print x
error = mapM_ putChar ==> putStr

-- ORD

error = compare x y /= GT ==> x <= y
error = compare x y == LT ==> x < y
error = compare x y /= LT ==> x >= y
error = compare x y == GT ==> x > y

-- READ/SHOW

error = showsPrec 0 x "" ==> show x
error = readsPrec 0 ==> reads
error = showsPrec 0 ==> shows

-- LIST

error = concat (map f x) ==> concatMap f x
error "Use map once" = map f (map g x) ==> map (f . g) x
warn  = x !! 0 ==> head x
error = take n (repeat x) ==> replicate n x
error = x ++ concatMap (' ':) y ==> unwords (x:y)
error = concat (intersperse " " x) ==> unwords x
error = head (reverse x) ==> last x
error = head (drop n x) ==> x !! n
error = reverse (tail (reverse x)) ==> init x
error = isPrefixOf (reverse x) (reverse y) ==> isSuffixOf x y
error = foldr (++) [] x ==> concat x
error = span (not . p) ==> break p
error = break (not . p) ==> span p
error = concatMap (++ "\n") ==> unlines
error = or (map p x) ==> any p x
error = and (map p x) ==> all p x
error = zipWith (,) ==> zip
error = zipWith3 (,,) ==> zip3
warn  = length x == 0 ==> null x
warn  "Use null" = length x /= 0 ==> not (null x)
error "Use :" = (\x -> [x]) ==> (:[])

-- FOLDS

error = foldr (&&) True ==> and
error = foldr (>>) (return ()) ==> sequence_
error = foldr (||) False ==> or
error = foldl (+) 0 ==> sum
warn  = foldl (*) 1 ==> product

-- FUNCTION

error = (\x -> x) ==> id
error = (\(_,y) -> y) ==> snd
error = (\(x,_) -> x) ==> fst
error = (\x y-> f (x,y)) ==> curry f
error = (\(x,y) -> f x y) ==> uncurry f

-- BOOL

error = not (a == b) ==> a /= b
error = not (a /= b) ==> a == b
error "Redundant if" = (if a then True else False) ==> a
error "Redundant if" = (if a then False else True) ==> not a
error "Redundant if" = (if a then t else (if b then t else f)) ==> if a || b then t else f
error "Redundant if" = (if a then (if b then t else f) else f) ==> if a && b then t else f
error "Redundant if" = (if x then True else y) ==> x || y
error "Redundant if" = (if x then y else False) ==> x && y
error "Use if" = case a of {True -> t; False -> f} ==> if a then t else f
error "Use if" = case a of {True -> t; _ -> f} ==> if a then t else f
error "Use if" = case a of {False -> f; _ -> t} ==> if a then t else f

-- ARROW

warn  = id *** g ==> second g
warn  = f *** id ==> first f
warn  = (\(x,y) -> (f x, g y)) ==> f *** g
warn  = (\x -> (f x, g x)) ==> f &&& g
warn  = (\(x,y) -> (f x,y)) ==> first f
warn  = (\(x,y) -> (x,g y)) ==> second g

-- MONAD

error "Monad law, left identity" = return a >>= f ==> f a
error "Monad law, right identity" = m >>= return ==> m
warn  = m >>= return . f ==> liftM f m
error = (if x then y else return ()) ==> when x $ y
error = sequence (map f as) ==> mapM f as
error = sequence_ (map f as) ==> mapM_ f as

-- LIST COMP

warn  "Use list comprehension" = (if b then [x] else []) ==> [x | b]

-- SEQ

error "Redundant seq" = x `seq` x ==> x
error "Redundant $!" = id $! x ==> x

-- MAYBE

error = maybe x id  ==> fromMaybe x
error = maybe False (const True) ==> isJust
error = maybe True (const False) ==> isNothing

-- MATHS

warn  = x + negate y ==> x - y
warn  = 0 - x ==> negate x
warn  = log y / log x ==> logBase x y
warn  = x ** 0.5 ==> sqrt x
warn  = sin x / cos x ==> tan x
warn  = sinh x / cosh x ==> tanh x
warn  = n `rem` 2 == 0 ==> even n
warn  = n `rem` 2 /= 0 ==> odd n
warn  = not (even x) ==> odd x
warn  = not (odd x) ==> even x
warn  "Use 1" = x ^ 0 ==> 1

-- EVALUATE

-- TODO: These should be moved in to HSE\Evaluate.hs and applied
--       through a special evaluate hint mechanism
error "Evaluate" = True && x ==> x
error "Evaluate" = False && x ==> False
error "Evaluate" = True || x ==> True
error "Evaluate" = False || x ==> x
error "Evaluate" = not True ==> False
error "Evaluate" = not False ==> True
error "Evaluate" = Nothing >>= k ==> Nothing
error "Evaluate" = either f g (Left x) ==> f x
error "Evaluate" = either f g (Right y) ==> g y
error "Evaluate" = fst (x,y) ==> x
error "Evaluate" = snd (x,y) ==> y
error "Evaluate" = f (fst p) (snd p) ==> uncurry f p
error "Evaluate" = init [x] ==> []
error "Evaluate" = null [] ==> True
error "Evaluate" = length [] ==> 0
error "Evaluate" = foldl f z [] ==> z
error "Evaluate" = foldr f z [] ==> z
error "Evaluate" = foldr1 f [x] ==> x
error "Evaluate" = scanr f q0 [] ==> [q0]
error "Evaluate" = scanr1 f [] ==> []
error "Evaluate" = scanr1 f [x] ==> [x]
error "Evaluate" = take n [] ==> []
error "Evaluate" = drop n [] ==> []
error "Evaluate" = takeWhile p [] ==> []
error "Evaluate" = dropWhile p [] ==> []
error "Evaluate" = span p [] ==> ([],[])
error "Evaluate" = lines "" ==> []
error "Evaluate" = unwords [] ==> ""
error "Evaluate" = x - 0 ==> x
error "Evaluate" = x * 1 ==> x
error "Evaluate" = x / 1 ==> x
error "Evaluate" = id x ==> x

-- COMPLEX

error "Use isPrefixOf" = (take i s == t) ==> _eval_ ((i == length t) && (t `isPrefixOf` s))
    where _ = (isList t || isLit t) && isLit i

error "Redundant $" = f $ x ==> f x
    where _ = isAtom x

warn  = (do a <- f; g a) ==> f >>= g
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

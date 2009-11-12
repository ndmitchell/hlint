
module HLint.Default where

-- I/O

error = putStrLn (show x) ==> print x
error = mapM_ putChar ==> putStr
error = hGetChar stdin ==> getChar
error = hGetLine stdin ==> getLine
error = hGetContents stdin ==> getContents
error = hPutChar stdout ==> putChar
error = hPutStr stdout ==> putStr
error = hPutStrLn stdout ==> putStrLn
error = hPrint stdout ==> print


-- ORD

error = not (a == b) ==> a /= b
error = not (a /= b) ==> a == b
error = not (a >  b) ==> a <= b
error = not (a >= b) ==> a <  b
error = not (a <  b) ==> a >= b
error = not (a <= b) ==> a >  b
error = compare x y /= GT ==> x <= y
error = compare x y == LT ==> x < y
error = compare x y /= LT ==> x >= y
error = compare x y == GT ==> x > y
error = compare (f x) (f y) ==> Data.Ord.comparing f x y
error = on compare f ==> Data.Ord.comparing f


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
error = map (uncurry f) (zip x y) ==> zipWith f x y
error = not (elem x y) ==> notElem x y

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
error = (\x y-> f (x,y)) ==> curry f where _ = notIn [x,y] f
error = (\(x,y) -> f x y) ==> uncurry f where _ = notIn [x,y] f
warn  = (\x -> f x y) ==> flip f y where _ = notIn x [f,y]
error "Redundant id" = id x ==> x
error "Redundant const" = const x y ==> x

-- BOOL

error "Redundant ==" = a == True ==> a
warn  "Redundant ==" = a == False ==> not a
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

error = id *** g ==> second g
error = f *** id ==> first f
warn  = (\(x,y) -> (f x, g y)) ==> f Control.Arrow.*** g where _ = notIn [x,y] [f,g]
warn  = (\x -> (f x, g x)) ==> f Control.Arrow.&&& g where _ = notIn x [f,g]
warn  = (\(x,y) -> (f x,y)) ==> Control.Arrow.first f where _ = notIn [x,y] f
warn  = (\(x,y) -> (x,f y)) ==> Control.Arrow.second f where _ = notIn [x,y] f

-- FUNCTOR

error "Functor law" = fmap f (fmap g x) ==> fmap (f . g) x
error "Functor law" = fmap id ==> id

-- MONAD

error "Monad law, left identity" = return a >>= f ==> f a
error "Monad law, right identity" = m >>= return ==> m
warn  = m >>= return . f ==> fmap f m
error = (if x then y else return ()) ==> Control.Monad.when x $ _noParen_ y
error = (if x then return () else y) ==> Control.Monad.unless x $ _noParen_ y
error = sequence (map f x) ==> mapM f x
error = sequence_ (map f x) ==> mapM_ f x
warn  = flip mapM ==> Control.Monad.forM
warn  = flip mapM_ ==> Control.Monad.forM_
error = when (not x) ==> unless x
error = x >>= id ==> Control.Monad.join x
error = liftM f (liftM g x) ==> liftM (f . g) x

-- MONAD LIST

error = liftM unzip (mapM f x) ==> Control.Monad.mapAndUnzipM f x
error = sequence (zipWith f x y) ==> Control.Monad.zipWithM f x y
error = sequence_ (zipWith f x y) ==> Control.Monad.zipWithM_ f x y
error = sequence (replicate n x) ==> Control.Monad.replicateM n x
error = sequence_ (replicate n x) ==> Control.Monad.replicateM_ n x
error = mapM f (map g x) ==> mapM (f . g) x
error = mapM_ f (map g x) ==> mapM_ (f . g) x

-- LIST COMP

warn  "Use list comprehension" = (if b then [x] else []) ==> [x | b]

-- SEQ

error "Redundant seq" = x `seq` x ==> x
error "Redundant $!" = id $! x ==> x

-- MAYBE

error = maybe x id  ==> Data.Maybe.fromMaybe x
error = maybe False (const True) ==> Data.Maybe.isJust
error = maybe True (const False) ==> Data.Maybe.isNothing
error = catMaybes (map f x) ==> mapMaybe f x

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

error "Use isPrefixOf" = (take i s == t) ==> _eval_ ((i == length t) && (t `Data.List.isPrefixOf` s))
    where _ = (isList t || isLit t) && isLit i

warn  = (do a <- f; g a) ==> f >>= g
    where _ = (isAtom f || isApp f) && notIn a g



{-
<TEST>
yes = concat . map f -- concatMap f
yes = foo . bar . concat . map f . baz . bar -- concatMap f . baz . bar
yes = map f (map g x) -- map (f . g) x
yes = concat.map (\x->if x==e then l' else [x]) -- concatMap (\x->if x==e then l' else [x])
yes = f x where f x = concat . map head -- concatMap head
yes = concat . map f . g -- concatMap f . g
yes = concat $ map f x -- concatMap f x
yes = "test" ++ concatMap (' ':) ["of","this"] -- unwords ("test":["of","this"])
yes = concat . intersperse " " -- unwords
yes = if f a then True else b -- f a || b
yes = not (a == b) -- a /= b
yes = not (a /= b) -- a == b
yes = if a then 1 else if b then 1 else 2 -- if a || b then 1 else 2
no  = if a then 1 else if b then 3 else 2
yes = a >>= return . id -- fmap id a
yes = (x !! 0) + (x !! 2) -- head x
yes = if b < 42 then [a] else [] -- [a | b < 42]
yes = take 5 (foo xs) == "hello" -- "hello" `Data.List.isPrefixOf` foo xs
no  = take n (foo xs) == "hello"
yes = head (reverse xs) -- last xs
yes = reverse xs `isPrefixOf` reverse ys -- isSuffixOf xs ys
no = putStrLn $ show (length xs) ++ "Test"
yes = do line <- getLine; putStrLn line -- getLine >>= putStrLn 
yes = ftable ++ map (\ (c, x) -> (toUpper c, urlEncode x)) ftable -- toUpper Control.Arrow.*** urlEncode
yes = map (\(a,b) -> a) xs -- fst
yes = map (\(a,_) -> a) xs -- fst
yes = readFile $ args !! 0 -- head args
yes = if Debug `elem` opts then ["--debug"] else [] -- ["--debug" | Debug `elem` opts]
yes = if nullPS s then return False else if headPS s /= '\n' then return False else alter_input tailPS >> return True \
    -- if nullPS s || (headPS s /= '\n') then return False else alter_input tailPS >> return True
yes = if foo then do stuff; moreStuff; lastOfTheStuff else return () \
    -- Control.Monad.when foo $ do stuff ; moreStuff ; lastOfTheStuff
yes = foo $ \(a, b) -> (a, y + b) -- Control.Arrow.second ((+) y)
no  = foo $ \(a, b) -> (a, a + b)
yes = map (uncurry (+)) $ zip [1 .. 5] [6 .. 10] -- zipWith (+) [1 .. 5] [6 .. 10]
no = do iter <- textBufferGetTextIter tb ; textBufferSelectRange tb iter iter
no = flip f x $ \y -> y*y+y
no = \x -> f x (g x)
yes = \x -> f x (g y) -- flip f (g y)
no = foo (\ v -> f v . g)

import Prelude \
yes = flip mapM -- Control.Monad.forM
import Control.Monad \
yes = flip mapM -- forM
import Control.Monad(forM) \
yes = flip mapM -- forM
import Control.Monad(forM_) \
yes = flip mapM -- Control.Monad.forM
import qualified Control.Monad \
yes = flip mapM -- Control.Monad.forM
</TEST>
-}

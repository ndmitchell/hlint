
module HLint.Default where

import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Function
import Data.Int
import Data.List as X
import Data.Maybe
import Data.Monoid
import Data.Ord
import System.IO

import IO as System.IO
import List as Data.List
import Maybe as Data.Maybe
import Monad as Control.Monad

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

error = not (a == b) ==> a /= b where note = "incorrect if either value is NaN"
error = not (a /= b) ==> a == b where note = "incorrect if either value is NaN"
error = not (a >  b) ==> a <= b where note = "incorrect if either value is NaN"
error = not (a >= b) ==> a <  b where note = "incorrect if either value is NaN"
error = not (a <  b) ==> a >= b where note = "incorrect if either value is NaN"
error = not (a <= b) ==> a >  b where note = "incorrect if either value is NaN"
error = compare x y /= GT ==> x <= y
error = compare x y == LT ==> x < y
error = compare x y /= LT ==> x >= y
error = compare x y == GT ==> x > y
error = compare (f x) (f y) ==> Data.Ord.comparing f x y
error = on compare f ==> Data.Ord.comparing f
error = x == y || x == z ==> x `elem` [y,z]
error = x /= y && x /= z ==> x `notElem` [y,z]

-- READ/SHOW

error = showsPrec 0 x "" ==> show x
error = readsPrec 0 ==> reads
error = showsPrec 0 ==> shows

-- LIST

error = concat (map f x) ==> concatMap f x
warn = concat [a,b] ==> a ++ b
-- these hints pick up way too much
--warn = a++b++c++d++e++f++g ==> concat [a, b, c, d, e, f, g]
--warn = a++b++c++d++e++f ==> concat [a, b, c, d, e, f]
--warn = a++b++c++d++e ==> concat [a, b, c, d, e]
error "Use map once" = map f (map g x) ==> map (f . g) x
warn  = x !! 0 ==> head x
error = take n (repeat x) ==> replicate n x
error = head (reverse x) ==> last x
error = head (drop n x) ==> x !! n
error = reverse (tail (reverse x)) ==> init x
error = isPrefixOf (reverse x) (reverse y) ==> isSuffixOf x y
error = foldr (++) [] ==> concat
error = span (not . p) ==> break p
error = break (not . p) ==> span p
error = concatMap (++ "\n") ==> unlines
error = or (map p x) ==> any p x
error = and (map p x) ==> all p x
error = zipWith (,) ==> zip
error = zipWith3 (,,) ==> zip3
warn  = length x == 0 ==> null x where note = "increases laziness"
warn  "Use null" = length x /= 0 ==> not (null x)
error "Use :" = (\x -> [x]) ==> (:[])
error = map (uncurry f) (zip x y) ==> zipWith f x y
error = not (elem x y) ==> notElem x y
warn  = foldr f z (map g x) ==> foldr (f . g) z x
error = x ++ concatMap (' ':) y ==> unwords (x:y)
error = intercalate " " ==> unwords
warn  = concat (intersperse x y) ==> intercalate x y where _ = notEq x " "
warn  = concat (intersperse " " x) ==> unwords x
error "Use any" = null (filter f x) ==> not (any f x)
error "Use any" = filter f x == [] ==> not (any f x)
error = filter f x /= [] ==> any f x

-- FOLDS

error = foldr (&&) True ==> and
error = foldl (&&) True ==> and
error = foldr (||) False ==> or
error = foldl (||) False ==> or
error = foldr (>>) (return ()) ==> sequence_
error = foldl (+) 0 ==> sum
error = foldl (*) 1 ==> product

-- FUNCTION

error = (\x -> x) ==> id
error = (\(_,y) -> y) ==> snd
error = (\(x,_) -> x) ==> fst
warn "Use curry" = (\x y-> f (x,y)) ==> curry f where _ = notIn [x,y] f
warn "Use uncurry" = (\(x,y) -> f x y) ==> uncurry f where _ = notIn [x,y] f
error "Redundant $" = (($) . f) ==> f
error "Redundant $" = (f $) ==> f
warn  = (\x -> y) ==> const y where _ = isAtom y && notIn x y
error "Redundant flip" = flip f x y ==> f y x where _ = isApp original
error "Redundant id" = id x ==> x
error "Redundant const" = const x y ==> x

-- BOOL

error "Redundant ==" = a == True ==> a
warn  "Redundant ==" = a == False ==> not a
error "Redundant if" = (if a then True else False) ==> a
error "Redundant if" = (if a then False else True) ==> not a
error "Redundant if" = (if a then t else (if b then t else f)) ==> if a || b then t else f
error "Redundant if" = (if a then (if b then t else f) else f) ==> if a && b then t else f
error "Redundant if" = (if x then True else y) ==> x || y where _ = notEq y False
error "Redundant if" = (if x then y else False) ==> x && y where _ = notEq y True
warn  "Use if" = case a of {True -> t; False -> f} ==> if a then t else f
warn  "Use if" = case a of {False -> f; True -> t} ==> if a then t else f
warn  "Use if" = case a of {True -> t; _ -> f} ==> if a then t else f
warn  "Use if" = case a of {False -> f; _ -> t} ==> if a then t else f

-- ARROW

error = id *** g ==> second g
error = f *** id ==> first f
error = zip (map f x) (map g x) ==> map (f Control.Arrow.&&& g) x
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
error = (if x then y else return ()) ==> Control.Monad.when x $ _noParen_ y where _ = not (isAtom y)
error = (if x then y else return ()) ==> Control.Monad.when x y where _ = isAtom y
error = (if x then return () else y) ==> Control.Monad.unless x $ _noParen_ y where _ = not (isAtom y)
error = (if x then return () else y) ==> Control.Monad.unless x y where _ = isAtom y
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
warn  "Redundant list comprehension" = [x | x <- y] ==> y where _ = isVar x

-- SEQ

error "Redundant seq" = x `seq` x ==> x
error "Redundant $!" = id $! x ==> x

-- MAYBE

error = maybe x id ==> Data.Maybe.fromMaybe x
error = maybe False (const True) ==> Data.Maybe.isJust
error = maybe True (const False) ==> Data.Maybe.isNothing
error = not (isNothing x) ==> isJust x
error = not (isJust x) ==> isNothing x
error = maybe [] (:[]) ==> maybeToList
error = catMaybes (map f x) ==> mapMaybe f x
error = concatMap (maybeToList . f) ==> Data.Maybe.mapMaybe f
error = concatMap maybeToList ==> catMaybes
error = maybe n Just x ==> Control.Monad.mplus x n
warn  = (case x of Nothing -> y; Just a -> a)  ==> fromMaybe y x
warn  = (case x of Just a -> a; Nothing -> y)  ==> fromMaybe y x
error = (if isNothing x then y else fromJust x) ==> fromMaybe y x
error = (if isJust x then fromJust x else y) ==> fromMaybe y x
error = isJust x && (fromJust x == y) ==> x == Just y
error = mapMaybe f (map g x) ==> mapMaybe (f . g) x

-- INFIX

warn "Use infix" = X.elem x y ==> x `X.elem` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = X.notElem x y ==> x `X.notElem` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = X.isInfixOf x y ==> x `X.isInfixOf` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = X.isSuffixOf x y ==> x `X.isSuffixOf` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = X.isPrefixOf x y ==> x `X.isPrefixOf` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = X.union x y ==> x `X.union` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = X.intersect x y ==> x `X.intersect` y where _ = not (isInfixApp original) && not (isParen result)

-- MATHS

error "Redundant fromIntegral" = fromIntegral x ==> x where _ = isLitInt x
error "Redundant fromInteger" = fromInteger x ==> x where _ = isLitInt x
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
warn  = x ** 0.5 ==> sqrt x
warn  = x ^^ y ==> x ** y where _ = isLitInt y
warn  "Use 1" = x ^ 0 ==> 1

-- EXCEPTION

error "Use Control.Exception.catch" = Prelude.catch ==> Control.Exception.catch where note = "Prelude.catch does not catch most exceptions"
warn = flip Control.Exception.catch ==> handle
warn = flip (catchJust p) ==> handleJust p
warn = Control.Exception.bracket b (const a) (const t) ==> Control.Exception.bracket_ b a t
warn = Control.Exception.bracket (openFile x y) hClose ==> withFile x y
warn = Control.Exception.bracket (openBinaryFile x y) hClose ==> withBinaryFile x y

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
error "Evaluate" = scanr f z [] ==> [z]
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
error "Evaluate" = concat [a] ==> a
error "Evaluate" = concat [] ==> []

-- COMPLEX

error "Use isPrefixOf" = (take i s == t) ==> _eval_ ((i == length t) && (t `Data.List.isPrefixOf` s))
    where _ = (isList t || isLit t) && isLit i

{-
-- clever hint, but not actually a good idea
warn  = (do a <- f; g a) ==> f >>= g
    where _ = (isAtom f || isApp f) && notIn a g
-}

test = hints named test are to allow people to put test code within hint files
testPrefix = and any prefix also works


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
yes = ftable ++ map (\ (c, x) -> (toUpper c, urlEncode x)) ftable -- toUpper Control.Arrow.*** urlEncode
yes = map (\(a,b) -> a) xs -- fst
yes = map (\(a,_) -> a) xs -- fst
yes = readFile $ args !! 0 -- head args
yes = if Debug `elem` opts then ["--debug"] else [] -- ["--debug" | Debug `elem` opts]
yes = if nullPS s then return False else if headPS s /= '\n' then return False else alter_input tailPS >> return True \
    -- if nullPS s || (headPS s /= '\n') then return False else alter_input tailPS >> return True
yes = if foo then do stuff; moreStuff; lastOfTheStuff else return () \
    -- Control.Monad.when foo $ do stuff ; moreStuff ; lastOfTheStuff
yes = if foo then stuff else return () -- Control.Monad.when foo stuff
yes = foo $ \(a, b) -> (a, y + b) -- Control.Arrow.second ((+) y)
no  = foo $ \(a, b) -> (a, a + b)
yes = map (uncurry (+)) $ zip [1 .. 5] [6 .. 10] -- zipWith (+) [1 .. 5] [6 .. 10]
no = do iter <- textBufferGetTextIter tb ; textBufferSelectRange tb iter iter
no = flip f x $ \y -> y*y+y
no = \x -> f x (g x)
no = foo (\ v -> f v . g)
yes = concat . intersperse " " -- unwords
yes = Prelude.concat $ intersperse " " xs -- unwords xs
yes = concat $ Data.List.intersperse " " xs -- unwords xs
yes = if a then True else False -- a
yes = if x then true else False -- x && true
yes = elem x y -- x `elem` y
yes = foo (elem x y) -- x `elem` y
no  = x `elem` y
no  = elem 1 [] : []
test a = foo (\x -> True) -- const True
h a = flip f x (y z) -- f (y z) x
h a = flip f x $ y z
yes x = case x of {True -> a ; False -> b} -- if x then a else b
yes x = case x of {False -> a ; _ -> b} -- if x then b else a
no = const . ok . toResponse $ "saved"
yes = case x z of Nothing -> y z; Just pattern -> pattern -- fromMaybe (y z) (x z)
yes = if p then s else return () -- Control.Monad.when p s
error = a $$$$ b $$$$ c ==> a . b $$$$$ c
yes = when (not . null $ asdf) -- unless (null asdf)
yes = id 1 -- 1
yes = case concat (map f x) of [] -> [] -- concatMap f x
yes = Map.union a b -- a `Map.union` b
yes = [v | v <- xs] -- xs
no  = [Left x | Left x <- xs]
yes = Map.union a b -- a `Map.union` b
when p s = if p then s else return ()
yes = x ^^ 18 -- x ** 18
no = x ^^ 18.5
instance Arrow (->) where first f = f *** id
yes = fromInteger 12 -- 12
yes = catch -- Control.Exception.catch
import Prelude hiding (catch); no = catch
import Control.Exception as E; no = E.catch
main = do f; putStrLn $ show x -- print x

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
import qualified Control.Monad as CM \
yes = flip mapM -- CM.forM
import qualified Control.Monad as CM(forM,filterM) \
yes = flip mapM -- CM.forM
import Control.Monad as CM(forM,filterM) \
yes = flip mapM -- forM
import Control.Monad hiding (forM) \
yes = flip mapM -- Control.Monad.forM
import Control.Monad hiding (filterM) \
yes = flip mapM -- forM
import qualified Data.Text.Lazy as DTL \
main = DTL.concat $ map (`DTL.snoc` '-') [DTL.pack "one", DTL.pack "two", DTL.pack "three"]
import Text.Blaze.Html5.Attributes as A \
main = A.id (stringValue id')
</TEST>
-}

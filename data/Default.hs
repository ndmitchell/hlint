
module HLint.Default where

import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Foldable
import Data.Foldable(asum, sequenceA_, traverse_, for_)
import Data.Traversable(traverse, for)
import Control.Applicative
import Data.Function
import Data.Int
import Data.Char
import Data.List as Data.List
import Data.List as X
import Data.Maybe
import Data.Monoid
import System.IO
import Control.Concurrent.Chan
import System.Mem.Weak
import Control.Exception.Base
import System.Exit
import Data.Either
import Numeric

import IO as System.IO
import List as Data.List
import Maybe as Data.Maybe
import Monad as Control.Monad
import Char as Data.Char

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
error = hWaitForInput a 0 ==> hReady a
error = hPutStrLn a (show b) ==> hPrint a b
error = hIsEOF stdin ==> isEOF

-- EXIT

error = exitWith ExitSuccess ==> exitSuccess

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
--warning = x == a || x == b || x == c ==> x `elem` [a,b,c] where note = ValidInstance "Eq" x
--warning = x /= a && x /= b && x /= c ==> x `notElem` [a,b,c] where note = ValidInstance "Eq" x
--error = compare (f x) (f y) ==> Data.Ord.comparing f x y -- not that great
--error = on compare f ==> Data.Ord.comparing f -- not that great
error = head (sort x) ==> minimum x
error = last (sort x) ==> maximum x
error = head (sortBy f x) ==> minimumBy f x
    where _ = isCompare f
error = last (sortBy f x) ==> maximumBy f x
    where _ = isCompare f
error "Avoid reverse" = reverse (sort x) ==> sortBy (flip compare) x
error "Avoid reverse" = reverse (sortBy f x) ==> sortBy (flip f) x
    where _ = isCompare f
warn  = flip (g `on` h) ==> flip g `on` h
warn  = (f `on` g) `on` h ==> f `on` (g . h)


-- READ/SHOW

error = showsPrec 0 x "" ==> show x
error = readsPrec 0 ==> reads
error = showsPrec 0 ==> shows
warn = showIntAtBase 16 intToDigit ==> showHex
warn = showIntAtBase 8 intToDigit ==> showOct

-- LIST

error = concat (map f x) ==> concatMap f x
warn = concat [a, b] ==> a ++ b
warn "Use map once" = map f (map g x) ==> map (f . g) x
warn  = x !! 0 ==> head x
error = take n (repeat x) ==> replicate n x
    where _ = noQuickCheck -- takes too long
error = map f (replicate n x) ==> replicate n (f x)
    where _ = noQuickCheck -- takes too long
error = map f (repeat x) ==> repeat (f x)
    where _ = noQuickCheck -- takes forever
error = cycle [x] ==> repeat x
    where _ = noQuickCheck -- takes forever
error = head (reverse x) ==> last x
error = head (drop n x) ==> x !! n where _ = isNat n
error = reverse (tail (reverse x)) ==> init x where note = IncreasesLaziness
error "Avoid reverse" = reverse (reverse x) ==> x where note = IncreasesLaziness
-- error = take (length x - 1) x ==> init x -- not true for x == []
error = isPrefixOf (reverse x) (reverse y) ==> isSuffixOf x y
error = foldr (++) [] ==> concat
error = foldl (++) [] ==> concat where note = IncreasesLaziness
error = foldl f (head x) (tail x) ==> foldl1 f x
error = foldr f (last x) (init x) ==> foldr1 f x
error = span (not . p) ==> break p
error = break (not . p) ==> span p
error = (takeWhile p x, dropWhile p x) ==> span p x
error = fst (span p x) ==> takeWhile p x
error = snd (span p x) ==> dropWhile p x
error = fst (break p x) ==> takeWhile (not . p) x
error = snd (break p x) ==> dropWhile (not . p) x
error = concatMap (++ "\n") ==> unlines
error = map id ==> id
error = or (map p x) ==> any p x
error = and (map p x) ==> all p x
error = zipWith (,) ==> zip
error = zipWith3 (,,) ==> zip3
warn  = length x == 0 ==> null x where note = IncreasesLaziness
warn  = x == [] ==> null x
warn  "Use null" = length x /= 0 ==> not (null x) where note = IncreasesLaziness
warn  "Use :" = (\x -> [x]) ==> (:[])
error = map (uncurry f) (zip x y) ==> zipWith f x y
warn  = map f (zip x y) ==> zipWith (curry f) x y where _ = isVar f
error = not (elem x y) ==> notElem x y
warn  = foldr f z (map g x) ==> foldr (f . g) z x
error = x ++ concatMap (' ':) y ==> unwords (x:y)
error = intercalate " " ==> unwords
warn  = concat (intersperse x y) ==> intercalate x y where _ = notEq x " "
warn  = concat (intersperse " " x) ==> unwords x
error "Use any" = null (filter f x) ==> not (any f x)
error "Use any" = filter f x == [] ==> not (any f x)
error = filter f x /= [] ==> any f x
error = any id ==> or
error = all id ==> and
error = any ((==) a) ==> elem a where note = ValidInstance "Eq" a
error = any (== a) ==> elem a
error = any (a ==) ==> elem a where note = ValidInstance "Eq" a
error = all ((/=) a) ==> notElem a where note = ValidInstance "Eq" a
error = all (/= a) ==> notElem a where note = ValidInstance "Eq" a
error = all (a /=) ==> notElem a where note = ValidInstance "Eq" a
error = elem True ==> or
error = notElem False ==> and
error = findIndex ((==) a) ==> elemIndex a
error = findIndex (a ==) ==> elemIndex a
error = findIndex (== a) ==> elemIndex a
error = findIndices ((==) a) ==> elemIndices a
error = findIndices (a ==) ==> elemIndices a
error = findIndices (== a) ==> elemIndices a
error = lookup b (zip l [0..]) ==> elemIndex b l
warn "Length always non-negative" = length x >= 0 ==> True
warn "Use null" = length x > 0 ==> not (null x) where note = IncreasesLaziness
warn "Use null" = length x >= 1 ==> not (null x) where note = IncreasesLaziness
error "Take on a non-positive" = take i x ==> [] where _ = isNegZero i
error "Drop on a non-positive" = drop i x ==> x where _ = isNegZero i
error = last (scanl f z x) ==> foldl f z x
error = head (scanr f z x) ==> foldr f z x
error = iterate id ==> repeat
    where _ = noQuickCheck -- takes forever
error = zipWith f (repeat x) ==> map (f x)
error = zipWith f x (repeat y) ==> map (\x -> f x y) x

-- BY

error = deleteBy (==) ==> delete
error = groupBy (==) ==> group
error = insertBy compare ==> insert
error = intersectBy (==) ==> intersect
error = maximumBy compare ==> maximum
error = minimumBy compare ==> minimum
error = nubBy (==) ==> nub
error = sortBy compare ==> sort
error = unionBy (==) ==> union

-- FOLDS

error = foldr  (>>) (return ()) ==> sequence_
    where _ = noQuickCheck
error = foldr  (&&) True ==> and
error = foldl  (&&) True ==> and where note = IncreasesLaziness
error = foldr1 (&&)  ==> and where note = RemovesError "on []"
error = foldl1 (&&)  ==> and where note = RemovesError "on []"
error = foldr  (||) False ==> or
error = foldl  (||) False ==> or where note = IncreasesLaziness
error = foldr1 (||)  ==> or where note = RemovesError "on []"
error = foldl1 (||)  ==> or where note = RemovesError "on []"
error = foldl  (+) 0 ==> sum
error = foldr  (+) 0 ==> sum
error = foldl1 (+)   ==> sum where note = RemovesError "on []"
error = foldr1 (+)   ==> sum where note = RemovesError "on []"
error = foldl  (*) 1 ==> product
error = foldr  (*) 1 ==> product
error = foldl1 (*)   ==> product where note = RemovesError "on []"
error = foldr1 (*)   ==> product where note = RemovesError "on []"
error = foldl1 max   ==> maximum
error = foldr1 max   ==> maximum
error = foldl1 min   ==> minimum
error = foldr1 min   ==> minimum
error = foldr mplus mzero ==> msum
    where _ = noQuickCheck

-- FUNCTION

error = (\x -> x) ==> id
error = (\x y -> x) ==> const
error = (\(x,y) -> y) ==> snd
error = (\(x,y) -> x) ==> fst
warn "Use curry" = (\x y -> f (x,y)) ==> curry f
warn "Use uncurry" = (\(x,y) -> f x y) ==> uncurry f where note = IncreasesLaziness
error "Redundant $" = (($) . f) ==> f
error "Redundant $" = (f $) ==> f
warn  = (\x -> y) ==> const y where _ = isAtom y
error "Redundant flip" = flip f x y ==> f y x where _ = isApp original
warn  = (\a b -> g (f a) (f b)) ==> g `Data.Function.on` f
error "Evaluate" = id x ==> x
error "Redundant id" = id . x ==> x
error "Redundant id" = x . id ==> x

-- CHAR

error = a >= 'a' && a <= 'z' ==> isAsciiLower a
error = a >= 'A' && a <= 'Z' ==> isAsciiUpper a
error = a >= '0' && a <= '9' ==> isDigit a
error = a >= '0' && a <= '7' ==> isOctDigit a
error = isLower a || isUpper a ==> isAlpha a
error = isUpper a || isLower a ==> isAlpha a

-- BOOL

error "Redundant ==" = x == True ==> x
warn  "Redundant ==" = x == False ==> not x
error "Redundant ==" = True == a ==> a
warn  "Redundant ==" = False == a ==> not a
error "Redundant /=" = a /= True ==> not a
warn  "Redundant /=" = a /= False ==> a
error "Redundant /=" = True /= a ==> not a
warn  "Redundant /=" = False /= a ==> a
error "Redundant if" = (if a then x else x) ==> x where note = IncreasesLaziness
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
warn  "Redundant if" = (if c then (True, x) else (False, x)) ==> (c, x) where note = IncreasesLaziness
warn  "Redundant if" = (if c then (False, x) else (True, x)) ==> (not c, x) where note = IncreasesLaziness
warn = or [x, y] ==> x || y
warn = or [x, y, z] ==> x || y || z
warn = and [x, y] ==> x && y
warn = and [x, y, z] ==> x && y && z
error "Redundant if" = (if x then False else y) ==> not x && y where _ = notEq y True
error "Redundant if" = (if x then y else True) ==> not x || y where _ = notEq y False
error "Redundant not" = not (not x) ==> x
-- error "Too strict if" = (if c then f x else f y) ==> f (if c then x else y) where note = IncreasesLaziness
-- also breaks types, see #87

-- ARROW

error = id *** g ==> second g
error = f *** id ==> first f
error = zip (map f x) (map g x) ==> map (f Control.Arrow.&&& g) x
warn  = (\(x,y) -> (f x, g y)) ==> f Control.Arrow.*** g
warn  = (\x -> (f x, g x)) ==> f Control.Arrow.&&& g
warn  = (\(x,y) -> (f x,y)) ==> Control.Arrow.first f
warn  = (\(x,y) -> (x,f y)) ==> Control.Arrow.second f
warn  = (f (fst x), g (snd x)) ==> (f Control.Arrow.*** g) x
warn "Redundant pair" = (fst x, snd x) ==>  x where note = DecreasesLaziness

-- FUNCTOR

error "Functor law" = fmap f (fmap g x) ==> fmap (f . g) x where _ = noQuickCheck
error "Functor law" = fmap id ==> id where _ = noQuickCheck
warn = fmap f $ x ==> f Control.Applicative.<$> x
    where _ = (isApp x || isAtom x) && noQuickCheck

-- MONAD

error "Monad law, left identity" = return a >>= f ==> f a where _ = noQuickCheck
error "Monad law, right identity" = m >>= return ==> m where _ = noQuickCheck
warn  = m >>= return . f ==> Control.Monad.liftM f m where _ = noQuickCheck -- cannot be fmap, because is in Functor not Monad
error = (if x then y else return ()) ==> Control.Monad.when x $ _noParen_ y where _ = not (isAtom y) && noQuickCheck
error = (if x then y else return ()) ==> Control.Monad.when x y where _ = isAtom y && noQuickCheck
error = (if x then return () else y) ==> Control.Monad.unless x $ _noParen_ y where _ = not (isAtom y) && noQuickCheck
error = (if x then return () else y) ==> Control.Monad.unless x y where _ = isAtom y && noQuickCheck
error = sequence (map f x) ==> mapM f x where _ = noQuickCheck
error = sequence_ (map f x) ==> mapM_ f x where _ = noQuickCheck
warn  = flip mapM ==> Control.Monad.forM where _ = noQuickCheck
warn  = flip mapM_ ==> Control.Monad.forM_ where _ = noQuickCheck
warn  = flip forM ==> mapM where _ = noQuickCheck
warn  = flip forM_ ==> mapM_ where _ = noQuickCheck
error = when (not x) ==> unless x where _ = noQuickCheck
error = x >>= id ==> Control.Monad.join x where _ = noQuickCheck
error = liftM f (liftM g x) ==> liftM (f . g) x where _ = noQuickCheck
error = fmap f (fmap g x) ==> fmap (f . g) x where _ = noQuickCheck
warn  = a >> return () ==> Control.Monad.void a
    where _ = (isAtom a || isApp a) && noQuickCheck
error = fmap (const ()) ==> Control.Monad.void where _ = noQuickCheck
error = flip (>=>) ==> (<=<) where _ = noQuickCheck
error = flip (<=<) ==> (>=>) where _ = noQuickCheck
error = flip (>>=) ==> (=<<) where _ = noQuickCheck
error = flip (=<<) ==> (>>=) where _ = noQuickCheck
warn  = (\x -> f x >>= g) ==> f Control.Monad.>=> g where _ = noQuickCheck
warn  = (\x -> f =<< g x) ==> f Control.Monad.<=< g where _ = noQuickCheck
error = a >> forever a ==> forever a where _ = noQuickCheck
warn  = liftM2 id ==> ap where _ = noQuickCheck
error = mapM (uncurry f) (zip l m) ==> zipWithM f l m where _ = noQuickCheck

-- STATE MONAD

error = fst (runState x y) ==> evalState x y where _ = noQuickCheck
error = snd (runState x y) ==> execState x y where _ = noQuickCheck

-- MONAD LIST

error = liftM unzip (mapM f x) ==> Control.Monad.mapAndUnzipM f x where _ = noQuickCheck
error = sequence (zipWith f x y) ==> Control.Monad.zipWithM f x y where _ = noQuickCheck
error = sequence_ (zipWith f x y) ==> Control.Monad.zipWithM_ f x y where _ = noQuickCheck
error = sequence (replicate n x) ==> Control.Monad.replicateM n x where _ = noQuickCheck
error = sequence_ (replicate n x) ==> Control.Monad.replicateM_ n x where _ = noQuickCheck
error = mapM f (replicate n x) ==> Control.Monad.replicateM n (f x) where _ = noQuickCheck
error = mapM_ f (replicate n x) ==> Control.Monad.replicateM_ n (f x) where _ = noQuickCheck
error = mapM f (map g x) ==> mapM (f . g) x where _ = noQuickCheck
error = mapM_ f (map g x) ==> mapM_ (f . g) x where _ = noQuickCheck
error = mapM id ==> sequence where _ = noQuickCheck
error = mapM_ id ==> sequence_ where _ = noQuickCheck

-- APPLICATIVE / TRAVERSABLE

error = flip traverse ==> for where _ = noQuickCheck
error = flip for ==> traverse where _ = noQuickCheck
error = flip traverse_ ==> for_ where _ = noQuickCheck
error = flip for_ ==> traverse_ where _ = noQuickCheck
error = foldr (*>) (pure ()) ==> sequenceA_ where _ = noQuickCheck
error = foldr (<|>) empty ==> asum where _ = noQuickCheck
error = liftA2 (flip ($)) ==> (<**>) where _ = noQuickCheck
error = Just <$> a <|> pure Nothing ==> optional a where _ = noQuickCheck


-- LIST COMP

warn  "Use list comprehension" = (if b then [x] else []) ==> [x | b]
warn  "Redundant list comprehension" = [x | x <- y] ==> y where _ = isVar x

-- SEQ

error "Redundant seq" = x `seq` x ==> x
error "Redundant $!" = id $! x ==> x
error "Redundant seq" = x `seq` y ==> y where _ = isWHNF x
error "Redundant $!" = f $! x ==> f x where _ = isWHNF x
error "Redundant evaluate" = evaluate x ==> return x where _ = isWHNF x

-- MAYBE

error = maybe x id ==> Data.Maybe.fromMaybe x
error = maybe False (const True) ==> Data.Maybe.isJust
error = maybe True (const False) ==> Data.Maybe.isNothing
error = not (isNothing x) ==> isJust x
error = not (isJust x) ==> isNothing x
error = maybe [] (:[]) ==> maybeToList
error = catMaybes (map f x) ==> mapMaybe f x
warn  = (case x of Nothing -> y; Just a -> a)  ==> fromMaybe y x
error = (if isNothing x then y else f (fromJust x)) ==> maybe y f x
error = (if isJust x then f (fromJust x) else y) ==> maybe y f x
error = maybe Nothing (Just . f) ==> fmap f
warn  = map fromJust . filter isJust  ==>  Data.Maybe.catMaybes
error  = x == Nothing  ==>  isNothing x
error  = Nothing == x  ==>  isNothing x
error  = x /= Nothing  ==>  Data.Maybe.isJust x
error  = Nothing /= x  ==>  Data.Maybe.isJust x
error = concatMap (maybeToList . f) ==> Data.Maybe.mapMaybe f
error = concatMap maybeToList ==> catMaybes
error = maybe n Just x ==> Control.Monad.mplus x n
warn  = (case x of Just a -> a; Nothing -> y)  ==> fromMaybe y x
error = (if isNothing x then y else fromJust x) ==> fromMaybe y x
error = (if isJust x then fromJust x else y) ==> fromMaybe y x
error = isJust x && (fromJust x == y) ==> x == Just y
error = mapMaybe f (map g x) ==> mapMaybe (f . g) x
error = fromMaybe a (fmap f x) ==> maybe a f x
error = mapMaybe id ==> catMaybes
warn = [x | Just x <- a] ==> Data.Maybe.catMaybes a
warn = (case m of Nothing -> Nothing; Just x -> x) ==> Control.Monad.join m
warn = maybe Nothing id ==> join
warn "Too strict maybe" = maybe (f x) (f . g) ==> f . maybe x g where note = IncreasesLaziness

-- EITHER

error = [a | Left a <- a] ==> lefts a
error = [a | Right a <- a] ==> rights a
error = either Left (Right . f) ==> fmap f

-- INFIX

warn "Use infix" = elem x y ==> x `elem` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = notElem x y ==> x `notElem` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = isInfixOf x y ==> x `isInfixOf` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = isSuffixOf x y ==> x `isSuffixOf` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = isPrefixOf x y ==> x `isPrefixOf` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = union x y ==> x `union` y where _ = not (isInfixApp original) && not (isParen result)
warn "Use infix" = intersect x y ==> x `intersect` y where _ = not (isInfixApp original) && not (isParen result)

-- MATHS

error "Redundant fromIntegral" = fromIntegral x ==> x where _ = isLitInt x
error "Redundant fromInteger" = fromInteger x ==> x where _ = isLitInt x
warn  = x + negate y ==> x - y
warn  = 0 - x ==> negate x
error "Redundant negate" = negate (negate x) ==> x
warn  = log y / log x ==> logBase x y
warn  = sin x / cos x ==> tan x
warn  = n `rem` 2 == 0 ==> even n
warn  = n `rem` 2 /= 0 ==> odd n
warn  = not (even x) ==> odd x
warn  = not (odd x) ==> even x
warn  = x ** 0.5 ==> sqrt x
warn  "Use 1" = x ^ 0 ==> 1
warn  = round (x - 0.5) ==> floor x

-- CONCURRENT

warn = mapM_ (writeChan a) ==> writeList2Chan a

-- EXCEPTION

warn = flip Control.Exception.catch ==> handle
warn = flip handle ==> Control.Exception.catch
warn = flip (catchJust p) ==> handleJust p
warn = flip (handleJust p) ==> catchJust p
warn = Control.Exception.bracket b (const a) (const t) ==> Control.Exception.bracket_ b a t
warn = Control.Exception.bracket (openFile x y) hClose ==> withFile x y
warn = Control.Exception.bracket (openBinaryFile x y) hClose ==> withBinaryFile x y
warn = throw (ErrorCall a) ==> error a
error = toException NonTermination ==> nonTermination
error = toException NestedAtomically ==> nestedAtomically

-- WEAK POINTERS

error = mkWeak a a b ==> mkWeakPtr a b
error = mkWeak a (a, b) c ==> mkWeakPair a b c

-- FOLDABLE

error "Use Foldable.forM_" = (case m of Nothing -> return (); Just x -> f x) ==> Data.Foldable.forM_ m f
    where _ = noQuickCheck
error "Use Foldable.forM_" = when (isJust m) (f (fromJust m)) ==> Data.Foldable.forM_ m f
    where _ = noQuickCheck

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
error "Evaluate" = take n [] ==> [] where note = IncreasesLaziness
error "Evaluate" = drop n [] ==> [] where note = IncreasesLaziness
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
error "Evaluate" = zip [] [] ==> []
error "Evaluate" = const x y ==> x

-- COMPLEX

{-
-- these would be a good idea, but we have not yet proven them and they seem to have side conditions
error "Use isPrefixOf" = take (length t) s == t ==> t `Data.List.isPrefixOf` s
error "Use isPrefixOf" = (take i s == t) ==> _eval_ ((i >= length t) && (t `Data.List.isPrefixOf` s))
    where _ = (isList t || isLit t) && isPos i
-}

{-
-- clever hint, but not actually a good idea
warn  = (do a <- f; g a) ==> f >>= g
    where _ = (isAtom f || isApp f)
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
yes = a >>= return . bob -- Control.Monad.liftM bob a
yes = (x !! 0) + (x !! 2) -- head x
yes = if b < 42 then [a] else [] -- [a | b < 42]
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
yes = [v | v <- xs] -- xs
no  = [Left x | Left x <- xs]
when p s = if p then s else return ()
no = x ^^ 18.5
instance Arrow (->) where first f = f *** id
yes = fromInteger 12 -- 12
import Prelude hiding (catch); no = catch
import Control.Exception as E; no = E.catch
main = do f; putStrLn $ show x -- print x
main = map (writer,) $ map arcObj $ filter (rdfPredEq (Res dctreferences)) ts -- map ((writer,) . arcObj) (filter (rdfPredEq (Res dctreferences)) ts)
h x y = return $! (x, y) -- return (x, y)
h x y = return $! x
getInt = do { x <- readIO "0"; return $! (x :: Int) }
foo = evaluate [12] -- return [12]
test = \ a -> f a >>= \ b -> return (a, b)
fooer input = catMaybes . map Just $ input -- mapMaybe Just
yes = mapMaybe id -- catMaybes
main = print $ map (\_->5) [2,3,5] -- const 5
main = head $ drop n x
main = head $ drop (-3) x -- x
main = head $ drop 2 x -- x !! 2
main = drop 0 x -- x
main = take 0 x -- []
main = take (-5) x -- []
main = take (-y) x
main = take 4 x
main = let (first, rest) = (takeWhile p l, dropWhile p l) in rest -- span p l
main = map $ \ d -> ([| $d |], [| $d |])
pairs (x:xs) = map (\y -> (x,y)) xs ++ pairs xs
{-# ANN foo "HLint: ignore" #-};foo = map f (map g x) -- @Ignore ???
yes = fmap lines $ abc 123 -- lines Control.Applicative.<$> abc 123
no = fmap lines $ abc $ def 123
test = foo . not . not -- id
test = map (not . not) xs -- id
used = not . not . any (`notElem` special) . fst . derives -- any (`notElem` special) . fst . derives
test = foo . id . map -- map
test = food id xs
yes = baz baz >> return () -- Control.Monad.void (baz baz)
no = foo >>= bar >>= something >>= elsee >> return ()
no = f (#) x
data Pair = P {a :: !Int}; foo = return $! P{a=undefined}
data Pair = P {a :: !Int}; foo = return $! P undefined
foo = return $! Just undefined -- return (Just undefined)
foo = return $! (a,b) -- return (a,b)
foo = return $! 1
foo = return $! "test"
bar = [x| (x,_) <- pts]
return' x = x `seq` return x
foo = last (sortBy (compare `on` fst) xs) -- maximumBy (compare `on` fst) xs
g = \ f -> parseFile f >>= (\ cu -> return (f, cu))
foo = bar $ \(x,y) -> x x y
foo = (\x -> f x >>= g) -- f Control.Monad.>=> g
foo = (\f -> h f >>= g) -- h Control.Monad.>=> g
foo = (\f -> h f >>= f)
foo = bar $ \x -> [x,y]
foo = bar $ \x -> [z,y] -- const [z,y]
f condition tChar tBool = if condition then _monoField tChar else _monoField tBool

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

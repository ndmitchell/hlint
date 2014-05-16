{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules, ScopedTypeVariables, DeriveDataTypeable, ViewPatterns #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, GeneralizedNewtypeDeriving #-}

-- | Used with --quickcheck
module HLint_QuickCheck where

import System.IO.Unsafe
import Data.Typeable
import Data.List
import Data.Maybe
import Control.Exception
import Control.Monad
import System.IO
import Control.Concurrent.Chan
import System.Mem.Weak(Weak)
import Test.QuickCheck hiding ((==>))
import Test.QuickCheck.Test hiding (test)

default(Maybe Bool,Int,Dbl)

newtype Dbl = Dbl Double deriving (Enum,Floating,Fractional,Num,Read,Real,RealFloat,RealFrac,Show,Typeable,Arbitrary,CoArbitrary)

instance Eq Dbl where
    Dbl a == Dbl b | isNaN a && isNaN b = True
                   | otherwise = abs (a - b) < 1e-4 || let s = a+b in s /= 0 && abs ((a-b)/s) < 1e-8

instance Ord Dbl where
    compare a b | a == b = EQ
    compare (Dbl a) (Dbl b) = compare a b

instance (Show a, Show b) => Show (a -> b) where show _ = "<func>"
instance Show a => Show (IO a) where show _ = "<IO>"
instance Show a => Show (Weak a) where show _ = "<Weak>"
instance Show a => Show (Chan a) where show _ = "<Chan>"

instance Eq (IO a) where _ == _ = True
instance Eq SomeException where a == b = show a == show b

instance Typeable IOMode where typeOf _ = typeOf ()

instance Arbitrary Handle where arbitrary = elements [stdin, stdout, stderr]
instance CoArbitrary Handle where coarbitrary _ = variant 0
instance Arbitrary IOMode where arbitrary = elements [ReadMode,WriteMode,AppendMode,ReadWriteMode]
instance Arbitrary a => Arbitrary (IO a) where arbitrary = fmap return arbitrary
instance Arbitrary (Chan a) where arbitrary = return $ unsafePerformIO newChan

instance Exception (Maybe Bool)

data Test a = Test Bool a a deriving (Show, Typeable)
instance Functor Test where
    fmap f (Test a b c) = Test a (f b) (f c)

a ==> b = Test False a b
a ?==> b = Test True a b

class Testable2 a where
    property2 :: Test a -> Property
instance Testable2 a => Testable (Test a) where
    property = property2
instance Eq a => Testable2 a where
    property2 (Test bx (catcher -> x) (catcher -> y)) =
        property $ (bx && isNothing x) || x == y
instance (Arbitrary a, Show a, Testable2 b) => Testable2 (a -> b) where
    property2 x = property $ \a -> fmap ($ a) x

test :: (Show p, Testable p, Typeable p) => FilePath -> Int -> String -> p -> IO ()
test file line hint p = do
    res <- quickCheckWithResult stdArgs{chatty=False} p
    unless (isSuccess res) $ do
        putStrLn $ "\n" ++ file ++ ":" ++ show line ++ ": " ++ hint
        print $ typeOf p
        putStr $ output res

catcher :: a -> Maybe a
catcher x = unsafePerformIO $ do
    res <- try $ evaluate x
    return $ case res of
        Left (_ :: SomeException) -> Nothing
        Right v -> Just v

_noParen_ = id
_eval_ = id


---------------------------------------------------------------------
-- EXAMPLES

main :: IO ()
main = do
    test "data\\Default.hs" 144 "findIndex ((==) a) ==> elemIndex a" $
        \ a -> (findIndex ((==) a)) ==> (elemIndex a)
    test "data\\Default.hs" 179 "foldr1 (&&) ==> and" $
        ((foldr1 (&&)) ?==> (and))
    test "data\\Default.hs" 407 "sinh x / cosh x ==> tanh x" $
        \ x -> (sqrt x) ==> (x ** 0.5)

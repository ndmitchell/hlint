{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

-- | Used with --quickcheck
module HLint_QuickCheck where

import System.IO.Unsafe
import Data.Typeable
import Data.List
import Control.Exception
import System.IO
import Control.Concurrent.Chan
import System.Mem.Weak(Weak)
import Test.QuickCheck hiding ((==>))

default(Maybe Bool,Int,Double)

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

a ==> b = a :==> b
data Test a = a :==> a deriving (Show, Typeable)

class Testable2 a where property2 :: a -> a -> Property
instance Testable2 a => Testable (Test a) where property (x :==> y) = property2 x y
instance Eq a => Testable2 a where property2 x y = property $ catcher x == catcher y
instance (Arbitrary a, Show a, Testable2 b) => Testable2 (a -> b) where property2 x y = property $ \a -> property2 (x a) (y a)

hlintTest :: (Show p, Testable p, Typeable p) => Int -> String -> p -> IO ()
hlintTest i s x = do
    putStrLn $ "test" ++ show i ++ " :: " ++ show (typeOf x)
    putStrLn s
    quickCheck x

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
    hlintTest 92 "findIndex ((==) a) ==> elemIndex a" _test92

_test92 = \ a -> (findIndex ((==) a)) ==> (elemIndex a)

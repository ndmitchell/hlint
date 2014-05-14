{-# LANGUAGE NoMonomorphismRestriction, ExtendedDefaultRules, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

-- | Used with --quickcheck
module HLint_QuickCheck where

import System.IO.Unsafe
import Data.Typeable
import Control.Exception
import System.IO
import Control.Concurrent
import System.Mem.Weak
import Test.QuickCheck hiding ((==>))
default(Maybe Bool,Int,Double)
data Test a = a :==> a deriving (Show, Typeable)
a ==> b = a :==> b
instance Testable2 a => Testable (Test a) where property (x :==> y) = property2 x y
class Testable2 a where property2 :: a -> a -> Property
instance Eq a => Testable2 a where property2 x y = property $ catcher x == catcher y
instance (Arbitrary a, Show a, Testable2 b) => Testable2 (a -> b) where property2 x y = property $ \a -> property2 (x a) (y a)

hlintTest :: (Show p, Testable p, Typeable p) => Int -> String -> p -> IO ()
hlintTest i s x = do putStrLn $ "test" ++ show i ++ " :: " ++ show (typeOf x); putStrLn s; quickCheck x
catcher :: a -> Maybe a
catcher x = unsafePerformIO $ do
    res <- try $ evaluate x
    return $ case res of
        Left (_ :: SomeException) -> Nothing
        Right v -> Just v
instance (Show a, Show b) => Show (a -> b) where show x = "<func>"
instance (Show a) => Show (IO a) where show x = "<IO>"
instance Show (Weak a) where show x = "<Weak>"
instance Eq (IO a) where _ == _ = True
instance Arbitrary Handle where arbitrary = elements [stdin, stdout, stderr]
instance CoArbitrary Handle where coarbitrary _ = variant 0
instance Arbitrary IOMode where arbitrary = elements [ReadMode,WriteMode,AppendMode,ReadWriteMode]
instance Typeable IOMode where typeOf _ = typeOf ()
instance Arbitrary a => Arbitrary (IO a) where arbitrary = fmap return arbitrary
instance Eq SomeException where a == b = show a == show b
instance Exception (Maybe Bool)
instance Arbitrary (Chan a)
instance Show (Chan a)
_noParen_ = id
_eval_ = id

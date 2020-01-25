{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Util.HsExtendInstances (
    HsExtendInstances(..)
  , extendInstances', unExtendInstances'
  , astEq', astListEq') where

import Outputable
import Data.Function

newtype HsExtendInstances a =
  HsExtendInstances a deriving Outputable -- Wrapper of terms.
-- The issue is that at times, terms we work with in this program are
-- not in `Eq` and `Ord` and we need them to be. This work-around
-- resorts to implementing `Eq` and `Ord` for the these types via
-- lexicographical comparisons of string representations. As long as
-- two different terms never map to the same string representation,
-- basing `Eq` and `Ord` on their string representations rather than
-- the term types themselves, leads to identical results.
toStr :: Outputable a => HsExtendInstances a -> String
toStr (HsExtendInstances e) = Outputable.showSDocUnsafe $ Outputable.ppr e
instance Outputable a => Eq (HsExtendInstances a) where (==) a b = toStr a == toStr b
instance Outputable a => Ord (HsExtendInstances a) where compare = compare `on` toStr
instance Outputable a => Show (HsExtendInstances a) where show = toStr

extendInstances' :: a -> HsExtendInstances a
extendInstances' = HsExtendInstances

unExtendInstances' :: HsExtendInstances a -> a
unExtendInstances' (HsExtendInstances x) = x

astEq' :: Outputable a => a -> a -> Bool
astEq' a b = extendInstances' a == extendInstances' b

astListEq' :: Outputable a => [a] -> [a] -> Bool
astListEq' as bs = length as == length bs && all (uncurry astEq') (zip as bs)

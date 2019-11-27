{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Util.W (
    W(..)
  , wrap, unwrap
  , eqLoc', eqNoLoc', eqNoLocLists') where

import Outputable
import SrcLoc

import GHC.Util.DynFlags
import GHC.Util.SrcLoc

import Data.Function
import Data.Data
import Data.Generics.Uniplate.Data ()

newtype W a = W a deriving Outputable -- Wrapper of terms.
-- The issue is that at times, terms we work with in this program are
-- not in `Eq` and `Ord` and we need them to be. This work-around
-- resorts to implementing `Eq` and `Ord` for the these types via
-- lexicographical comparisons of string representations. As long as
-- two different terms never map to the same string representation,
-- basing `Eq` and `Ord` on their string representations rather than
-- the term types themselves, leads to identical results.
wToStr :: Outputable a => W a -> String
wToStr (W e) = showPpr baseDynFlags e
instance Outputable a => Eq (W a) where (==) a b = wToStr a == wToStr b
instance Outputable a => Ord (W a) where compare = compare `on` wToStr
instance Outputable a => Show (W a) where show = wToStr

wrap :: a -> W a
wrap = W

unwrap :: W a -> a
unwrap (W x) = x

-- Compare two terms for absolute equality.
eqLoc' :: Outputable a => a -> a -> Bool
eqLoc' a b = wrap a == wrap b

-- Compare two terms for equality modulo locs.
eqNoLoc' :: (Data a, Outputable a, HasSrcSpan a) => a -> a -> Bool
eqNoLoc' a b = wrap (stripLocs' a)  == wrap (stripLocs' b)

eqNoLocLists' :: (Data a, Outputable a, HasSrcSpan a) => [a] -> [a] -> Bool
eqNoLocLists' as bs = length as == length bs && all (uncurry eqNoLoc') (zip as bs)

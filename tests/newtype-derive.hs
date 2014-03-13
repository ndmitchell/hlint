{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Test where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)

newtype A f = A f
  deriving (Foldable, Functor, Traversable, Typeable)

-- These hints are for test purposes, and are not intended to
-- be used for real.

module HLint.Test where

import "hint" HLint.Builtin.Naming


error = Prelude.readFile ==> bad

error = (x :: Int) ==> (x :: Int32)
    where _ = notTypeSafe


error "Test1" = scanr ==> scanr
error "Test2" = filter ==> filter
error "Test3" = foldr ==> foldr
error "Test4" = foldl ==> foldl

ignore "Test1" = ""
ignore "Test3"
ignore "Test2" = ignoreTest
warn = ignoreTest3
ignore = Ignore_Test

{-# ANN module "HLint: ignore Test4" #-}
{-# ANN annTest2 "HLint: error" #-}
{-# ANN annTest3 "HLint: warn" #-}
{-# ANN type Ann_Test "HLint: ignore" #-}


error = concat (map f x) ==> Data.List.concatMap f x

infix 9 +
error = a * (b+c) ==> undefined


{-
<TEST>
main = readFile "foo" >>= putStr            \
  -- bad

import Prelude hiding(readFile)             \
import Data.ByteString.Char8(readFile)      \
test = readFile "foo" >>= putStr

import Prelude as Prelude2                  \
yes = Prelude2.readFile "foo" >>= putStr    \
  -- bad

yes = 32 :: Int -- 32 :: Int32

ignoreTest = filter -- @Ignore ???
ignoreTest2 = filter -- @Error ???
ignoreTest3 = filter -- @Warning ???
ignoreAny = scanr -- @Ignore ???
ignoreNew = foldr -- @Ignore ???
type Ignore_Test = Int -- @Ignore ???

annTest = foldl -- @Ignore ???
annTest2 = foldl -- @Error ???
annTest3 = scanr -- @Warning ???
type Ann_Test = Int -- @Ignore ???

concatMap f x = concat (map f x)
concatMop f x = concat (map f x) -- Data.List.concatMap f x

yes = 1 * 2+3 -- undefined
</TEST>
-}



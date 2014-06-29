-- These hints are for test purposes, and are not intended to
-- be used for real.

-- FIXME: Should make this module modules in one file, so can easily test lots of
--        things without them overlapping
module HLint.Test where

import "hint" HLint.Builtin.All


error = Prelude.readFile ==> bad

error = (x :: Int) ==> (x :: Int32)
    where _ = noTypeCheck


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
{-# ANN annTest3 ("HLint: warn" :: String) #-}
{-# ANN type Ann_Test ("HLint: ignore") #-}


error = concat (map f x) ==> Data.List.concatMap f x

infix 9 +
error = a * (b+c) ==> undefined

error = Array.head ==> head
error = tail ==> Array.tail
warn = id Control.Arrow.*** id ==> id

error = zip [1..length x] x ==> zipFrom 1 x

error = before a ==> after a

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
yes = before 12 -- after 12

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

import Foo; test = Foo.id 1

test = head
import Array; test = Array.head -- head
test = Array.head -- head
test = head
import qualified Array; test = head
import Array(tail); test = head
import Array(head); test = head -- head
import Array as A; test = A.head -- head
test = tail -- Array.tail
import qualified Array as B; test = tail -- B.tail
import Control.Arrow; test = id *** id -- id
test = id Control.Arrow.*** id -- id
import Control.Arrow as Q; test = id Q.*** id -- id
zip [1..length x]
zip [1..length x] x -- zipFrom 1 x

{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-} \
{-# LANGUAGE RecordWildCards #-} -- @Ignore ???

{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-} \
{-# LANGUAGE RecordWildCards #-} -- @Ignore ???

{-# ANN module "HLint: ignore Use import/export shortcut" #-} \
module ABCD(module A, module B, module C) where \
import A; import B; import C -- @Ignore ???

{-# ANN lam "HLint: ignore Redundant lambda" #-} \
lam = \x -> x x x -- @Ignore ???

{-# ANN module "HLint: ignore Reduce duplication" #-} \
dup = do a; a; a; a; a; a -- @Ignore ???

</TEST>
-}

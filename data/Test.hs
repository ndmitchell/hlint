-- These hints are for test purposes, and are not intended to
-- be used for real.

module HLint.Test where


error = Prelude.readFile ==> bad

error = (x :: Int) ==> (x :: Int32)
    where _ = notTypeSafe


error "Test1" = map ==> map
error "Test2" = filter ==> filter
error "Test3" = foldr ==> foldr

ignore "Test1" = ""
ignore "Test3"
ignore "Test2" = ignoreTest
warn = ignoreTest3


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
ignoreAny = map -- @Ignore ???
ignoreNew = foldr -- @Ignore ???

</TEST>
-}

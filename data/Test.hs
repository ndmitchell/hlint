-- These hints are for test purposes, and are not intended to
-- be used for real.

module HLint.Test where


error = Prelude.readFile ==> bad

error = (x :: Int) ==> (x :: Int32)
    where _ = notTypeSafe


error "Test1" = map ==> map
error "Test2" = filter ==> filter

ignore "Test1" = ""
ignore "Test2" = ignoreTest


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
ignoreAny = map -- @Ignore ???

</TEST>
-}

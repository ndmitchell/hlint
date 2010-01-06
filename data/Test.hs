-- These hints are for test purposes, and are not intended to
-- be used for real.

module HLint.Test where


error = Prelude.readFile ==> bad

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
</TEST>
-}

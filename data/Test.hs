-- These hints are for test purposes, and are not intended to
-- be used for real.

module HLint.Test where


error = Prelude.readFile ==> bad

{-
<TEST>
yes = readFile "foo" >>= putStr where res = bad
</TEST>
-}

{-
<TEST>
import Prelude hiding(readFile)
import Data.ByteString.Char8(readFile)
no = readFile "foo" >>= putStr
</TEST>
-}

{-
<TEST>
import Prelude as Prelude2
yes = Prelude2.readFile "foo" >>= putStr where res = bad
</TEST>
-}

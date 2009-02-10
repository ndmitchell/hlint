
module HLint.Dollar where

error = a $ b $ c ==> a . b $ c

{-
<TEST>
yes = concat $ concat $ map f x where res = concat . concat $ map f x
</TEST>
-}


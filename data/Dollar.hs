
module HLint.Dollar where

warn = a $ b $ c ==> a . b $ c

{-
<TEST>
yes = concat $ concat $ map f x -- concat . concat $ map f x
</TEST>
-}


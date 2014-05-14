
-- Used with --typecheck
module HLint_TypeCheck where

(==>) :: a -> a -> a
(==>) = undefined

_noParen_ = id
_eval_ = id


---------------------------------------------------------------------
-- EXAMPLES

main :: IO ()
main = return ()

{-# LINE 116 "data\\Default.hs" #-}
_test64 = \ p x -> (and (map p x)) ==> (all p x)

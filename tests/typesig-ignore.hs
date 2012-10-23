-- Bug #563
module Foo where
{-# ANN foobar "HLint: ignore Use String" #-}

foobar :: [Char]
foobar = []

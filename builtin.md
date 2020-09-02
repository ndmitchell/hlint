# Built-in Hints

This page is auto-generated from `cabal run hlint test -- --generate-summary`
or `stack run hlint test -- --generate-summary`.

<table>
<tr>
<th>Hint</th>
<th>Severity</th>
<th>Support Refactoring?</th>
</tr>
<tr>
<td rowspan=2>Avoid lambda</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
f = foo (\y -> g x . h $ y)
</code>
<br>
Found:
<code>
\ y -> g x . h $ y
</code>
<br>
Suggestion:
<code>
g x . h
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Avoid lambda</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
f = foo (\x y -> fun x y)
</code>
<br>
Found:
<code>
(\ x y -> fun x y)
</code>
<br>
Suggestion:
<code>
fun
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Avoid lambda using `infix`</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo x = bar (\ d -> search d table)
</code>
<br>
Found:
<code>
(\ d -> search d table)
</code>
<br>
Suggestion:
<code>
(`search` table)
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Avoid restricted function</td>
<td>Warning</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo = unsafePerformIO
</code>
<br>
Found:
<code>
unsafePerformIO
</code>
<br>
Suggestion:
<code>

</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Collapse lambdas</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
f = foo (\x -> \y -> x x y y)
</code>
<br>
Found:
<code>
\ x -> \ y -> x x y y
</code>
<br>
Suggestion:
<code>
\ x y -> x x y y
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Eta reduce</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
fun x y z = f x y z
</code>
<br>
Found:
<code>
fun x y z = f x y z
</code>
<br>
Suggestion:
<code>
fun = f
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Fix pragma markup</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
{- MISSING HASH #-}
</code>
<br>
Found:
<code>
{- MISSING HASH #-}
</code>
<br>
Suggestion:
<code>
{-# MISSING HASH #-}
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Long function</td>
<td>Warning</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<pre>
f = do 
 x <- y 
 return x
</pre>
Found:
<pre>
= do x <- y
     return x
</pre>
Suggestion:
<code>

</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Long type list</td>
<td>Warning</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
f :: Bool -> Int -> (Int -> Proxy '[a, b])
</code>
<br>
Found:
<code>
f :: Bool -> Int -> (Int -> Proxy '[a, b])
</code>
<br>
Suggestion:
<code>

</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Many arg function</td>
<td>Warning</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<pre>
f :: Int -> Int -> Int 
f = undefined
</pre>
Found:
<code>
f :: Int -> Int -> Int
</code>
<br>
Suggestion:
<code>

</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Many imports</td>
<td>Warning</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
import A; import B
</code>
<br>
Found:
<pre>
import A
import B

</pre>
Suggestion:
<code>

</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Missing NOINLINE pragma</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
slaves = unsafePerformIO Multimap.newIO
</code>
<br>
Found:
<code>
slaves = unsafePerformIO Multimap.newIO
</code>
<br>
Suggestion:
<pre>
{-# NOINLINE slaves #-}
slaves = unsafePerformIO Multimap.newIO
</pre>
</td>
</tr>
<tr>
<td rowspan=2>Move brackets to avoid $</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = (b $ c d) ++ e
</code>
<br>
Found:
<code>
(b $ c d) ++ e
</code>
<br>
Suggestion:
<code>
b (c d) ++ e
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Move guards forward</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo = [x + 1 | x <- [1..10], feature]
</code>
<br>
Found:
<code>
[x + 1 | x <- [1 .. 10], feature]
</code>
<br>
Suggestion:
<code>
[x + 1 | feature, x <- [1 .. 10]]
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Move map inside list comprehension</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo = map f [x + 1 | x <- [1..10]]
</code>
<br>
Found:
<code>
map f [x + 1 | x <- [1 .. 10]]
</code>
<br>
Suggestion:
<code>
[f (x + 1) | x <- [1 .. 10]]
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Reduce duplication</td>
<td>Ignore</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
{-# ANN main "HLint: ignore Reduce duplication" #-}; main = do a; a; a; a; a; a
</code>
<br>
Found:
<pre>
a
a
a

</pre>
Suggestion:
<code>
Combine with src/Hint/Duplicate.hs:1:73
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Reduce duplication</td>
<td>Suggestion</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<pre>
foo = a where {a = 1; b = 2; c = 3} 
bar = a where {a = 1; b = 2; c = 3}
</pre>
Found:
<pre>
a = 1
b = 2
c = 3

</pre>
Suggestion:
<code>
Combine with src/Hint/Duplicate.hs:2:16-20
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant $</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = split "to" $ names
</code>
<br>
Found:
<code>
split "to" $ names
</code>
<br>
Suggestion:
<code>
split "to" names
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant True guards</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo = [myexpr | True, a]
</code>
<br>
Found:
<code>
[myexpr | True, a]
</code>
<br>
Suggestion:
<code>
[myexpr | a]
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant as</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
import A as A
</code>
<br>
Found:
<code>
import A as A
</code>
<br>
Suggestion:
<code>
import A
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant as-pattern</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo x@_ = x
</code>
<br>
Found:
<code>
x@_
</code>
<br>
Suggestion:
<code>
x
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant bang pattern</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo = case v of !True -> x
</code>
<br>
Found:
<code>
!True
</code>
<br>
Suggestion:
<code>
True
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant bracket</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = (f x) x
</code>
<br>
Found:
<code>
(f x) x
</code>
<br>
Suggestion:
<code>
f x x
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant bracket</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = (foo)
</code>
<br>
Found:
<code>
(foo)
</code>
<br>
Suggestion:
<code>
foo
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant case</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo = case f v of _ -> x
</code>
<br>
Found:
<code>
case f v of { _ -> x }
</code>
<br>
Suggestion:
<code>
x
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant do</td>
<td>Ignore</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = do (bar+foo)
</code>
<br>
Found:
<code>
do
</code>
<br>
Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant guard</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo x | otherwise = y
</code>
<br>
Found:
<code>
foo x | otherwise = y
</code>
<br>
Suggestion:
<code>
foo x = y
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant irrefutable pattern</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo = let ~x = 1 in y
</code>
<br>
Found:
<code>
~x
</code>
<br>
Suggestion:
<code>
x
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant lambda</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
f a = \x -> x + x
</code>
<br>
Found:
<code>
f a = \ x -> x + x
</code>
<br>
Suggestion:
<code>
f a x = x + x
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant return</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = do bar; a <- foo; return a
</code>
<br>
Found:
<pre>
do bar
   a <- foo
   return a
</pre>
Suggestion:
<pre>
do bar
   foo
</pre>
</td>
</tr>
<tr>
<td rowspan=2>Redundant section</td>
<td>Suggestion</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
issue970 = (f x +) (g x)
</code>
<br>
Found:
<code>
(f x +) (g x)
</code>
<br>
Suggestion:
<code>
f x + (g x)
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant variable capture</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
main = do _ <- forM_ f xs; bar
</code>
<br>
Found:
<code>
_ <- forM_ f xs
</code>
<br>
Suggestion:
<code>
forM_ f xs
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant void</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
main = void $ forM_ f xs
</code>
<br>
Found:
<code>
void $ forM_ f xs
</code>
<br>
Suggestion:
<code>
forM_ f xs
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Redundant where</td>
<td>Suggestion</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo x = x + x where
</code>
<br>
Found:
<code>
where
</code>
<br>
Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Short-circuited list comprehension</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo = [myexpr | False]
</code>
<br>
Found:
<code>
[myexpr | False]
</code>
<br>
Suggestion:
<code>
[]
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Unused LANGUAGE pragma</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<pre>
{-# LANGUAGE Arrows #-} 
f = id
</pre>
Found:
<code>
{-# LANGUAGE Arrows #-}
</code>
<br>
Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use :</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = [x] ++ xs
</code>
<br>
Found:
<code>
[x] ++ xs
</code>
<br>
Suggestion:
<code>
x : xs
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use <$></td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = do x <- bar; return (f x)
</code>
<br>
Found:
<pre>
do x <- bar
   return (f x)
</pre>
Suggestion:
<code>
do f <$> bar
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use DerivingStrategies</td>
<td>Ignore</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
newtype Foo = Foo Int deriving (Show, Eq)
</code>
<br>
Found:
<pre>
newtype Foo
  = Foo Int
  deriving (Show, Eq)
</pre>
Suggestion:
<code>

</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use LANGUAGE pragmas</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
{-# OPTIONS_GHC -cpp #-}
</code>
<br>
Found:
<pre>
{-# OPTIONS_GHC -cpp #-}

</pre>
Suggestion:
<pre>
{-# LANGUAGE CPP #-}

</pre>
</td>
</tr>
<tr>
<td rowspan=2>Use String</td>
<td>Ignore</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
data Yes = Yes (Maybe [Char])
</code>
<br>
Found:
<code>
Maybe [Char]
</code>
<br>
Suggestion:
<code>
Maybe String
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use camelCase</td>
<td>Suggestion</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
data Yes = Bar | Test_Bar
</code>
<br>
Found:
<code>
data Yes = Bar | Test_Bar
</code>
<br>
Suggestion:
<code>
data Yes = Bar | TestBar
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use explicit module export list</td>
<td>Ignore</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
module Foo(module Foo) where foo = 1
</code>
<br>
Found:
<pre>
module Foo (
        module Foo
    ) where
</pre>
Suggestion:
<pre>
module Foo (
         ... 
    ) where
</pre>
</td>
</tr>
<tr>
<td rowspan=2>Use fewer LANGUAGE pragmas</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
{-# LANGUAGE RebindableSyntax, EmptyCase, RebindableSyntax #-}
</code>
<br>
Found:
<code>
{-# LANGUAGE RebindableSyntax, EmptyCase, RebindableSyntax #-}
</code>
<br>
Suggestion:
<code>
{-# LANGUAGE RebindableSyntax, EmptyCase #-}
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use fewer imports</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
import A; import A
</code>
<br>
Found:
<pre>
import A
import A

</pre>
Suggestion:
<pre>
import A

</pre>
</td>
</tr>
<tr>
<td rowspan=2>Use foldM</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
f [] a = return a ; f (x:xs) a = a + x >>= \fax -> f xs fax
</code>
<br>
Found:
<pre>
f [] a = return a
f (x : xs) a = a + x >>= \ fax -> f xs fax
</pre>
Suggestion:
<code>
f xs a = foldM (+) a xs
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use foldM_</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
folder f a xs = foldM f a xs >> return ()
</code>
<br>
Found:
<code>
foldM f a xs
</code>
<br>
Suggestion:
<code>
foldM_ f a xs
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use foldl</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
f z (x:xs) = f (z*x) xs ; f z [] = z
</code>
<br>
Found:
<pre>
f z (x : xs) = f (z * x) xs
f z [] = z
</pre>
Suggestion:
<code>
f z xs = foldl (*) z xs
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use foldr</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
f (x:xs) = negate x + f xs ; f [] = 0
</code>
<br>
Found:
<pre>
f (x : xs) = negate x + f xs
f [] = 0
</pre>
Suggestion:
<code>
f xs = foldr ((+) . negate) 0 xs
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use forM_</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = do forM files $ \x -> return (); return ()
</code>
<br>
Found:
<code>
forM files $ \ x -> return ()
</code>
<br>
Suggestion:
<code>
forM_ files $ \ x -> return ()
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use guards</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes x y = if a then b else if c then d else e
</code>
<br>
Found:
<code>
yes x y = if a then b else if c then d else e
</code>
<br>
Suggestion:
<pre>
yes x y
  | a = b
  | c = d
  | otherwise = e
</pre>
</td>
</tr>
<tr>
<td rowspan=2>Use join</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = do x <- bar; x
</code>
<br>
Found:
<pre>
do x <- bar
   x
</pre>
Suggestion:
<code>
do join bar
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use lambda</td>
<td>Suggestion</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo = bar (\x -> case x of Y z -> z)
</code>
<br>
Found:
<code>
\ x -> case x of { Y z -> z }
</code>
<br>
Suggestion:
<code>
\ (Y z) -> z
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use lambda-case</td>
<td>Suggestion</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = blah (\ x -> case x of A -> a; B -> b)
</code>
<br>
Found:
<pre>
\ x
  -> case x of
       A -> a
       B -> b
</pre>
Suggestion:
<pre>
\case
  A -> a
  B -> b
</pre>
</td>
</tr>
<tr>
<td rowspan=2>Use let</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = do x <- return y; foo x
</code>
<br>
Found:
<code>
x <- return y
</code>
<br>
Suggestion:
<code>
let x = y
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use list literal</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = 1:2:[]
</code>
<br>
Found:
<code>
1 : 2 : []
</code>
<br>
Suggestion:
<code>
[1, 2]
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use list literal pattern</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes (1:2:[]) = 1
</code>
<br>
Found:
<code>
(1 : 2 : [])
</code>
<br>
Suggestion:
<code>
[1, 2]
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use map</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
f (x:xs) = x + 1 : f xs ; f [] = []
</code>
<br>
Found:
<pre>
f (x : xs) = x + 1 : f xs
f [] = []
</pre>
Suggestion:
<code>
f xs = map (+ 1) xs
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use mapM_</td>
<td>Warning</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = do mapM print a; return b
</code>
<br>
Found:
<code>
mapM print a
</code>
<br>
Suggestion:
<code>
mapM_ print a
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use module export list</td>
<td>Ignore</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
module Foo where foo = 1
</code>
<br>
Found:
<code>
module Foo where
</code>
<br>
Suggestion:
<pre>
module Foo (
        module Foo
    ) where
</pre>
</td>
</tr>
<tr>
<td rowspan=2>Use newtype instead of data</td>
<td>Suggestion</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
data Foo = Foo Int
</code>
<br>
Found:
<code>
data Foo = Foo Int
</code>
<br>
Suggestion:
<code>
newtype Foo = Foo Int
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use otherwise</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo x | a = b | True = d
</code>
<br>
Found:
<pre>
foo x
  | a = b
  | True = d
</pre>
Suggestion:
<pre>
foo x
  | a = b
  | otherwise = d
</pre>
</td>
</tr>
<tr>
<td rowspan=2>Use pragma syntax</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
{- INLINE Y -}
</code>
<br>
Found:
<code>
{- INLINE Y -}
</code>
<br>
Suggestion:
<code>
{-# INLINE Y #-}
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use record patterns</td>
<td>Suggestion</td>
<td>Yes</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo (Bar _ _ _ _) = x
</code>
<br>
Found:
<code>
Bar _ _ _ _
</code>
<br>
Suggestion:
<code>
Bar {}
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use section</td>
<td>Suggestion</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
f = foo ((*) x)
</code>
<br>
Found:
<code>
((*) x)
</code>
<br>
Suggestion:
<code>
(x *)
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Use tuple-section</td>
<td>Suggestion</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
yes = blah (\ x -> (y, x))
</code>
<br>
Found:
<code>
\ x -> (y, x)
</code>
<br>
Suggestion:
<code>
(y,)
</code>
<br>
</td>
</tr>
<tr>
<td rowspan=2>Used otherwise as a pattern</td>
<td>Warning</td>
<td>No</td>
</tr>
<tr>
<td colspan=2>
Example:
<code>
foo otherwise = 1
</code>
<br>
Found:
<code>
otherwise
</code>
<br>
Suggestion:
<code>
_
</code>
<br>
</td>
</tr>
</table>

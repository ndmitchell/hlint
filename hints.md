# Summary of Hints

This page is auto-generated from `hlint --generate-summary`.

## Builtin Bracket

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Redundant section</td>
<td>Examples:<br />
&mdash;Example:
<code>
issue970 = (f x +) (g x)
</code>
<br>
&emsp;Found:
<code>
(f x +) (g x)
</code>
<br>
&emsp;Suggestion:
<code>
f x + (g x)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant bracket</td>
<td>Examples:<br />
&mdash;Example:
<code>
main = 1; {-# ANN module (1 + (2)) #-}
</code>
<br>
&emsp;Found:
<code>
(2)
</code>
<br>
&emsp;Suggestion:
<code>
2
</code>
<br>
&mdash;Example:
<code>
f x = case x of (Nothing) -> 1; _ -> 2
</code>
<br>
&emsp;Found:
<code>
(Nothing)
</code>
<br>
&emsp;Suggestion:
<code>
Nothing
</code>
<br>
&mdash;Example:
<code>
foo ((True)) = 1
</code>
<br>
&emsp;Found:
<code>
((True))
</code>
<br>
&emsp;Suggestion:
<code>
True
</code>
<br>
&mdash;Example:
<code>
foo (True) = 1
</code>
<br>
&emsp;Found:
<code>
(True)
</code>
<br>
&emsp;Suggestion:
<code>
True
</code>
<br>
&mdash;Example:
<code>
yes = ((+5))
</code>
<br>
&emsp;Found:
<code>
((+ 5))
</code>
<br>
&emsp;Suggestion:
<code>
(+ 5)
</code>
<br>
&mdash;Example:
<code>
yes = (($ 1))
</code>
<br>
&emsp;Found:
<code>
(($ 1))
</code>
<br>
&emsp;Suggestion:
<code>
($ 1)
</code>
<br>
&mdash;Example:
<code>
yes = (($ x)) 
</code>
<br>
&emsp;Found:
<code>
(($ x))
</code>
<br>
&emsp;Suggestion:
<code>
($ x)
</code>
<br>
&mdash;Example:
<code>
yes = f (x) y
</code>
<br>
&emsp;Found:
<code>
(x)
</code>
<br>
&emsp;Suggestion:
<code>
x
</code>
<br>
&mdash;Example:
<code>
yes = f ((x))
</code>
<br>
&emsp;Found:
<code>
((x))
</code>
<br>
&emsp;Suggestion:
<code>
x
</code>
<br>
&mdash;Example:
<code>
yes = foo (bar)
</code>
<br>
&emsp;Found:
<code>
(bar)
</code>
<br>
&emsp;Suggestion:
<code>
bar
</code>
<br>
&mdash;Example:
<code>
yes = (foo)
</code>
<br>
&emsp;Found:
<code>
(foo)
</code>
<br>
&emsp;Suggestion:
<code>
foo
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant bracket</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = (foo . bar x) <$> baz q
</code>
<br>
&emsp;Found:
<code>
(foo . bar x) <$> baz q
</code>
<br>
&emsp;Suggestion:
<code>
foo . bar x <$> baz q
</code>
<br>
&mdash;Example:
<code>
data Foo = Foo {foo :: (Maybe Foo)}
</code>
<br>
&emsp;Found:
<code>
foo :: (Maybe Foo)
</code>
<br>
&emsp;Suggestion:
<code>
foo :: Maybe Foo
</code>
<br>
&mdash;Example:
<code>
foo :: (Maybe Int) -> a
</code>
<br>
&emsp;Found:
<code>
(Maybe Int) -> a
</code>
<br>
&emsp;Suggestion:
<code>
Maybe Int -> a
</code>
<br>
&mdash;Example:
<code>
issue1179 = do(this is a test)
</code>
<br>
&emsp;Found:
<code>
do (this is a test)
</code>
<br>
&emsp;Suggestion:
<code>
do this is a test
</code>
<br>
&mdash;Example:
<code>
issue909 = foo (\((x : z) -> y) -> 9 + x * 7)
</code>
<br>
&emsp;Found:
<code>
\ ((x : z) -> y) -> 9 + x * 7
</code>
<br>
&emsp;Suggestion:
<code>
\ (x : z -> y) -> 9 + x * 7
</code>
<br>
&mdash;Example:
<code>
main = do f; (print x)
</code>
<br>
&emsp;Found:
<pre>
do f
   (print x)
</pre>
&emsp;Suggestion:
<pre>
do f
   print x
</pre>
&mdash;Example:
<code>
yes = (`foo` (bar baz))
</code>
<br>
&emsp;Found:
<code>
(`foo` (bar baz))
</code>
<br>
&emsp;Suggestion:
<code>
(`foo` bar baz)
</code>
<br>
&mdash;Example:
<code>
yes = \ x -> (x && x)
</code>
<br>
&emsp;Found:
<code>
\ x -> (x && x)
</code>
<br>
&emsp;Suggestion:
<code>
\ x -> x && x
</code>
<br>
&mdash;Example:
<code>
yes = C { f = (e h) }
</code>
<br>
&emsp;Found:
<code>
C {f = (e h)}
</code>
<br>
&emsp;Suggestion:
<code>
C {f = e h}
</code>
<br>
&mdash;Example:
<code>
yes = foo ((x y), z)
</code>
<br>
&emsp;Found:
<code>
((x y), z)
</code>
<br>
&emsp;Suggestion:
<code>
(x y, z)
</code>
<br>
&mdash;Example:
<code>
yes = [(foo bar)]
</code>
<br>
&emsp;Found:
<code>
[(foo bar)]
</code>
<br>
&emsp;Suggestion:
<code>
[foo bar]
</code>
<br>
&mdash;Example:
<code>
yes = (a foo) :: Int
</code>
<br>
&emsp;Found:
<code>
  (a foo) :: Int
</code>
<br>
&emsp;Suggestion:
<code>
  a foo :: Int
</code>
<br>
&mdash;Example:
<code>
yes = if x then (f y) else z
</code>
<br>
&emsp;Found:
<code>
if x then (f y) else z
</code>
<br>
&emsp;Suggestion:
<code>
if x then f y else z
</code>
<br>
&mdash;Example:
<code>
yes = if (f x) then y else z
</code>
<br>
&emsp;Found:
<code>
if (f x) then y else z
</code>
<br>
&emsp;Suggestion:
<code>
if f x then y else z
</code>
<br>
&mdash;Example:
<code>
yes = (f x) ||| y
</code>
<br>
&emsp;Found:
<code>
(f x) ||| y
</code>
<br>
&emsp;Suggestion:
<code>
f x ||| y
</code>
<br>
&mdash;Example:
<code>
yes = foo ((x x))
</code>
<br>
&emsp;Found:
<code>
((x x))
</code>
<br>
&emsp;Suggestion:
<code>
(x x)
</code>
<br>
&mdash;Example:
<code>
yes = (foo bar)
</code>
<br>
&emsp;Found:
<code>
(foo bar)
</code>
<br>
&emsp;Suggestion:
<code>
foo bar
</code>
<br>
&mdash;Example:
<code>
yes = (f x) x
</code>
<br>
&emsp;Found:
<code>
(f x) x
</code>
<br>
&emsp;Suggestion:
<code>
f x x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant $</td>
<td>Examples:<br />
&mdash;Example:
<code>
no = f $ [1,2..5]
</code>
<br>
&emsp;Found:
<code>
f $ [1, 2 .. 5]
</code>
<br>
&emsp;Suggestion:
<code>
f [1, 2 .. 5]
</code>
<br>
&mdash;Example:
<code>
yes = operator foo $ operator
</code>
<br>
&emsp;Found:
<code>
operator foo $ operator
</code>
<br>
&emsp;Suggestion:
<code>
operator foo operator
</code>
<br>
&mdash;Example:
<code>
yes = white $ keysymbol
</code>
<br>
&emsp;Found:
<code>
white $ keysymbol
</code>
<br>
&emsp;Suggestion:
<code>
white keysymbol
</code>
<br>
&mdash;Example:
<code>
yes = split "to" $ names
</code>
<br>
&emsp;Found:
<code>
split "to" $ names
</code>
<br>
&emsp;Suggestion:
<code>
split "to" names
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Move brackets to avoid $</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = (a b $ c d) ++ e
</code>
<br>
&emsp;Found:
<code>
(a b $ c d) ++ e
</code>
<br>
&emsp;Suggestion:
<code>
a b (c d) ++ e
</code>
<br>
&mdash;Example:
<code>
yes = (b $ c d) ++ e
</code>
<br>
&emsp;Found:
<code>
(b $ c d) ++ e
</code>
<br>
&emsp;Suggestion:
<code>
b (c d) ++ e
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
</table>

## Builtin Comment

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use pragma syntax</td>
<td>Examples:<br />
&mdash;Example:
<code>
{- NOINLINE Y -}
</code>
<br>
&emsp;Found:
<code>
{- NOINLINE Y -}
</code>
<br>
&emsp;Suggestion:
<code>
{-# NOINLINE Y #-}
</code>
<br>
&mdash;Example:
<code>
{- INLINE[~k] f -}
</code>
<br>
&emsp;Found:
<code>
{- INLINE[~k] f -}
</code>
<br>
&emsp;Suggestion:
<code>
{-# INLINE[~k] f #-}
</code>
<br>
&mdash;Example:
<code>
{- INLINE Y -}
</code>
<br>
&emsp;Found:
<code>
{- INLINE Y -}
</code>
<br>
&emsp;Suggestion:
<code>
{-# INLINE Y #-}
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Fix pragma markup</td>
<td>Examples:<br />
&mdash;Example:
<code>
{- MISSING HASH #-}
</code>
<br>
&emsp;Found:
<code>
{- MISSING HASH #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# MISSING HASH #-}
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
</table>

## Builtin Export

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use module export list</td>
<td>Examples:<br />
&mdash;Example:
<code>
module Foo where foo = 1
</code>
<br>
&emsp;Found:
<code>
module Foo where
</code>
<br>
&emsp;Suggestion:
<pre>
module Foo (
        module Foo
    ) where
</pre>
Does not support refactoring.
</td>
<td>Ignore</td>
</tr>
<tr>
<td>Use explicit module export list</td>
<td>Examples:<br />
&mdash;Example:
<code>
module Foo(module Foo, foo) where foo = 1
</code>
<br>
&emsp;Found:
<pre>
module Foo (
        module Foo, foo
    ) where
</pre>
&emsp;Suggestion:
<pre>
module Foo (
         ... , foo
    ) where
</pre>
&mdash;Example:
<code>
module Foo(module Foo) where foo = 1
</code>
<br>
&emsp;Found:
<pre>
module Foo (
        module Foo
    ) where
</pre>
&emsp;Suggestion:
<pre>
module Foo (
         ... 
    ) where
</pre>
Does not support refactoring.
</td>
<td>Ignore</td>
</tr>
</table>

## Builtin Extensions

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Unused LANGUAGE pragma</td>
<td>Examples:<br />
&mdash;Example:
<pre>
{-# LANGUAGE OverloadedRecordDot #-} 
f = (. foo)
</pre>
&emsp;Found:
<code>
{-# LANGUAGE OverloadedRecordDot #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE OverloadedRecordDot #-} 
f x = x . foo
</pre>
&emsp;Found:
<code>
{-# LANGUAGE OverloadedRecordDot #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE NoMonomorphismRestriction, NamedFieldPuns #-} 
main = 1
</pre>
&emsp;Found:
<code>
{-# LANGUAGE NoMonomorphismRestriction, NamedFieldPuns #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE NoMonomorphismRestriction #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE ImportQualifiedPost #-} 
import qualified Control.Monad as CM hiding (mapM) 
import Data.Foldable
</pre>
&emsp;Found:
<code>
{-# LANGUAGE ImportQualifiedPost #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE LambdaCase, MultiWayIf, NoRebindableSyntax #-} 
foo = \case True -> 3
</pre>
&emsp;Found:
<code>
{-# LANGUAGE LambdaCase, MultiWayIf, NoRebindableSyntax #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE LambdaCase, NoRebindableSyntax #-}
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE Trustworthy, NamedFieldPuns #-}
</code>
<br>
&emsp;Found:
<code>
{-# LANGUAGE Trustworthy, NamedFieldPuns #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE Trustworthy #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE NumericUnderscores #-} 
avogadro = 6.022140857e+23
</pre>
&emsp;Found:
<code>
{-# LANGUAGE NumericUnderscores #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE NamedFieldPuns #-}
</code>
<br>
&emsp;Found:
<code>
{-# LANGUAGE NamedFieldPuns #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE MultiWayIf #-} 
x = if b1 then v1 else if b2 then v2 else v3
</pre>
&emsp;Found:
<code>
{-# LANGUAGE MultiWayIf #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE PatternSynonyms #-} 
x = 42
</pre>
&emsp;Found:
<code>
{-# LANGUAGE PatternSynonyms #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE PolyKinds, KindSignatures #-} 
data Set (cxt :: * -> *) a = Set [a]
</pre>
&emsp;Found:
<code>
{-# LANGUAGE PolyKinds, KindSignatures #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE PolyKinds #-}
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE PolyKinds, KindSignatures #-}
</code>
<br>
&emsp;Found:
<code>
{-# LANGUAGE PolyKinds, KindSignatures #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE PolyKinds #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE EmptyCase #-} 
main = case () of x -> x
</pre>
&emsp;Found:
<code>
{-# LANGUAGE EmptyCase #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE EmptyCase #-} 
main = case () of x -> x
</pre>
&emsp;Found:
<code>
{-# LANGUAGE EmptyCase #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE MagicHash #-} 
foo = id
</pre>
&emsp;Found:
<code>
{-# LANGUAGE MagicHash #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE DeriveAnyClass #-} 
data Foo a = Foo a deriving (Eq,Data,Functor)
</pre>
&emsp;Found:
<code>
{-# LANGUAGE DeriveAnyClass #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE DeriveAnyClass #-} 
main = id
</pre>
&emsp;Found:
<code>
{-# LANGUAGE DeriveAnyClass #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE OverloadedLabels #-} 
main = id
</pre>
&emsp;Found:
<code>
{-# LANGUAGE OverloadedLabels #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE OverloadedLists #-} 
main = id
</pre>
&emsp;Found:
<code>
{-# LANGUAGE OverloadedLists #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE OverloadedStrings #-} 
main = id
</pre>
&emsp;Found:
<code>
{-# LANGUAGE OverloadedStrings #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE TupleSections #-} 
main = id
</pre>
&emsp;Found:
<code>
{-# LANGUAGE TupleSections #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE NumDecimals #-} 
foo = 12.345e2
</pre>
&emsp;Found:
<code>
{-# LANGUAGE NumDecimals #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE NumDecimals #-} 
foo = id
</pre>
&emsp;Found:
<code>
{-# LANGUAGE NumDecimals #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE LambdaCase #-} 
foo = id
</pre>
&emsp;Found:
<code>
{-# LANGUAGE LambdaCase #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE TypeApplications #-} 
foo = id
</pre>
&emsp;Found:
<code>
{-# LANGUAGE TypeApplications #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE DefaultSignatures #-} 
class Val a where; val :: a
</pre>
&emsp;Found:
<code>
{-# LANGUAGE DefaultSignatures #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE InstanceSigs #-} 
instance Eq a => Eq (T a) where 
  (==) (T x) (T y) = x==y
</pre>
&emsp;Found:
<code>
{-# LANGUAGE InstanceSigs #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE UnboxedTuples #-} 
f :: x -> (x, x); f x = (x, x)
</pre>
&emsp;Found:
<code>
{-# LANGUAGE UnboxedTuples #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-} 
newtype Micro = Micro Int deriving Generic
</pre>
&emsp;Found:
<code>
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE DeriveGeneric #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-} 
deriving instance Show Bar
</pre>
&emsp;Found:
<code>
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE StandaloneDeriving #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
newtype Foo = Foo Int deriving Data
</pre>
&emsp;Found:
<code>
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} 
data Foo = Foo Int deriving Class
</pre>
&emsp;Found:
<code>
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} 
newtype Foo = Foo Int deriving Class
</pre>
&emsp;Found:
<code>
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} 
data Foo = Foo Int deriving Data
</pre>
&emsp;Found:
<code>
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE DeriveDataTypeable #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} 
newtype Foo = Foo Int deriving Data
</pre>
&emsp;Found:
<code>
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE DeriveDataTypeable #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} 
record = 1
</pre>
&emsp;Found:
<code>
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE UnboxedTuples #-} 
record = 1
</pre>
&emsp;Found:
<code>
{-# LANGUAGE UnboxedTuples #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE DisambiguateRecordFields #-} 
record = 1
</pre>
&emsp;Found:
<code>
{-# LANGUAGE RecordWildCards #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE RecordWildCards #-} 
record = 1
</pre>
&emsp;Found:
<code>
{-# LANGUAGE RecordWildCards #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE TypeOperators #-} 
(<+>) :: Int -> Int -> Int 
x <+> y = x + y
</pre>
&emsp;Found:
<code>
{-# LANGUAGE TypeOperators #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE TypeOperators #-} 
foo :: Int -> (<+>) Double Bool 
foo x = y
</pre>
&emsp;Found:
<code>
{-# LANGUAGE TypeOperators #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE TypeOperators, TypeFamilies #-} 
type family Foo a b :: Type where Foo a b = (<+>) a b
</pre>
&emsp;Found:
<code>
{-# LANGUAGE TypeOperators, TypeFamilies #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE TypeFamilies #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE TypeOperators #-} 
data Foo a b = a :+ b
</pre>
&emsp;Found:
<code>
{-# LANGUAGE TypeOperators #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE BangPatterns #-} 
data Foo = Foo !Int
</pre>
&emsp;Found:
<code>
{-# LANGUAGE BangPatterns #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-} 
f x = x + [e| x + 1 |] + [foo| x + 1 |]
</pre>
&emsp;Found:
<code>
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE QuasiQuotes #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE TemplateHaskell #-} 
main = foo ''Bar
</pre>
&emsp;Found:
<code>
{-# LANGUAGE TemplateHaskell #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE RebindableSyntax, ParallelListComp, ImplicitParams #-} 
f = [(a,c) | a <- b | c <- d]
</pre>
&emsp;Found:
<code>
{-# LANGUAGE RebindableSyntax, ParallelListComp, ImplicitParams #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE RebindableSyntax, ParallelListComp #-}
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE Arrows #-} 
f = id
</pre>
&emsp;Found:
<code>
{-# LANGUAGE Arrows #-}
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
</td>
<td>Warning</td>
</tr>
</table>

## Builtin Fixities

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Redundant bracket due to operator fixities</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = (a >>= f) >>= g
</code>
<br>
&emsp;Found:
<code>
(a >>= f) >>= g
</code>
<br>
&emsp;Suggestion:
<code>
a >>= f >>= g
</code>
<br>
&mdash;Example:
<code>
yes = (2 * 3) + 1
</code>
<br>
&emsp;Found:
<code>
(2 * 3) + 1
</code>
<br>
&emsp;Suggestion:
<code>
2 * 3 + 1
</code>
<br>
&mdash;Example:
<code>
yes = 1 + (2 * 3)
</code>
<br>
&emsp;Found:
<code>
1 + (2 * 3)
</code>
<br>
&emsp;Suggestion:
<code>
1 + 2 * 3
</code>
<br>
</td>
<td>Ignore</td>
</tr>
</table>

## Builtin Import

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use fewer imports</td>
<td>Examples:<br />
&mdash;Example:
<pre>
import A (foo) 
import A (bar) 
import A (baz)
</pre>
&emsp;Found:
<pre>
import A ( foo )
import A ( bar )
import A ( baz )

</pre>
&emsp;Suggestion:
<pre>
import A ( foo, bar, baz )

</pre>
&mdash;Example:
<code>
import B; import A; import A
</code>
<br>
&emsp;Found:
<pre>
import A
import A

</pre>
&emsp;Suggestion:
<pre>
import A

</pre>
&mdash;Example:
<code>
import A; import B; import A
</code>
<br>
&emsp;Found:
<pre>
import A
import A

</pre>
&emsp;Suggestion:
<pre>
import A

</pre>
&mdash;Example:
<code>
import A; import A as Y
</code>
<br>
&emsp;Found:
<pre>
import A
import A as Y

</pre>
&emsp;Suggestion:
<pre>
import A as Y

</pre>
&mdash;Example:
<code>
import A; import A hiding (C)
</code>
<br>
&emsp;Found:
<pre>
import A
import A hiding ( C )

</pre>
&emsp;Suggestion:
<pre>
import A

</pre>
&mdash;Example:
<code>
import A(B) ; import A(C)
</code>
<br>
&emsp;Found:
<pre>
import A ( B )
import A ( C )

</pre>
&emsp;Suggestion:
<pre>
import A ( B, C )

</pre>
&mdash;Example:
<code>
import A ;import A(Foo)
</code>
<br>
&emsp;Found:
<pre>
import A
import A ( Foo )

</pre>
&emsp;Suggestion:
<pre>
import A

</pre>
&mdash;Example:
<code>
import A(Foo) ; import A
</code>
<br>
&emsp;Found:
<pre>
import A ( Foo )
import A

</pre>
&emsp;Suggestion:
<pre>
import A

</pre>
&mdash;Example:
<code>
import A; import A; import A
</code>
<br>
&emsp;Found:
<pre>
import A
import A
import A

</pre>
&emsp;Suggestion:
<pre>
import A

</pre>
&mdash;Example:
<code>
import A; import A
</code>
<br>
&emsp;Found:
<pre>
import A
import A

</pre>
&emsp;Suggestion:
<pre>
import A

</pre>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant as</td>
<td>Examples:<br />
&mdash;Example:
<code>
import qualified A as A
</code>
<br>
&emsp;Found:
<code>
import qualified A as A
</code>
<br>
&emsp;Suggestion:
<code>
import qualified A
</code>
<br>
&mdash;Example:
<code>
import A as A
</code>
<br>
&emsp;Found:
<code>
import A as A
</code>
<br>
&emsp;Suggestion:
<code>
import A
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
</table>

## Builtin Lambda

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use tuple-section</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = blah (\ x -> (y, x, z+q))
</code>
<br>
&emsp;Found:
<code>
\ x -> (y, x, z + q)
</code>
<br>
&emsp;Suggestion:
<code>
(y,, z + q)
</code>
<br>
&mdash;Example:
<code>
yes = blah (\ x -> (y, x, y, u, v))
</code>
<br>
&emsp;Found:
<code>
\ x -> (y, x, y, u, v)
</code>
<br>
&emsp;Suggestion:
<code>
(y,, y, u, v)
</code>
<br>
&mdash;Example:
<code>
yes = blah (\ x -> (y, x, z+q))
</code>
<br>
&emsp;Found:
<code>
\ x -> (y, x, z + q)
</code>
<br>
&emsp;Suggestion:
<code>
(y,, z + q)
</code>
<br>
&mdash;Example:
<code>
yes = blah (\ x -> (y, x))
</code>
<br>
&emsp;Found:
<code>
\ x -> (y, x)
</code>
<br>
&emsp;Suggestion:
<code>
(y,)
</code>
<br>
Does not support refactoring.
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use section</td>
<td>Examples:<br />
&mdash;Example:
<code>
f = bar (flip Foo.bar x)
</code>
<br>
&emsp;Found:
<code>
(flip Foo.bar x)
</code>
<br>
&emsp;Suggestion:
<code>
(`Foo.bar` x)
</code>
<br>
&mdash;Example:
<code>
f = foo (flip (Prelude.*) x)
</code>
<br>
&emsp;Found:
<code>
(flip (Prelude.*) x)
</code>
<br>
&emsp;Suggestion:
<code>
(Prelude.* x)
</code>
<br>
&mdash;Example:
<code>
f = foo (flip (*) x)
</code>
<br>
&emsp;Found:
<code>
(flip (*) x)
</code>
<br>
&emsp;Suggestion:
<code>
(* x)
</code>
<br>
&mdash;Example:
<code>
f = foo (flip x y)
</code>
<br>
&emsp;Found:
<code>
(flip x y)
</code>
<br>
&emsp;Suggestion:
<code>
(`x` y)
</code>
<br>
&mdash;Example:
<code>
f = foo (flip op x)
</code>
<br>
&emsp;Found:
<code>
(flip op x)
</code>
<br>
&emsp;Suggestion:
<code>
(`op` x)
</code>
<br>
&mdash;Example:
<code>
f = foo (flip op x)
</code>
<br>
&emsp;Found:
<code>
(flip op x)
</code>
<br>
&emsp;Suggestion:
<code>
(`op` x)
</code>
<br>
&mdash;Example:
<code>
f = foo ((*) x)
</code>
<br>
&emsp;Found:
<code>
((*) x)
</code>
<br>
&emsp;Suggestion:
<code>
(x *)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use lambda-case</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo = bar (\x -> case x of Y z | z > 0 -> z)
</code>
<br>
&emsp;Found:
<code>
\ x -> case x of Y z | z > 0 -> z
</code>
<br>
&emsp;Suggestion:
<code>
\case Y z | z > 0 -> z
</code>
<br>
&mdash;Example:
<code>
yes = blah (\ x -> case x of A -> a; B -> b)
</code>
<br>
&emsp;Found:
<pre>
\ x
  -> case x of
       A -> a
       B -> b
</pre>
&emsp;Suggestion:
<pre>
\case
  A -> a
  B -> b
</pre>
&mdash;Example:
<code>
yes = blah (\ x -> case x of A -> a; B -> b)
</code>
<br>
&emsp;Found:
<pre>
\ x
  -> case x of
       A -> a
       B -> b
</pre>
&emsp;Suggestion:
<pre>
\case
  A -> a
  B -> b
</pre>
Does not support refactoring.
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use lambda</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo = bar (\x -> case x of [y, z] -> z)
</code>
<br>
&emsp;Found:
<code>
\ x -> case x of [y, z] -> z
</code>
<br>
&emsp;Suggestion:
<code>
\ [y, z] -> z
</code>
<br>
&mdash;Example:
<code>
foo = bar (\x -> case x of Y z -> z)
</code>
<br>
&emsp;Found:
<code>
\ x -> case x of Y z -> z
</code>
<br>
&emsp;Suggestion:
<code>
\ (Y z) -> z
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant lambda</td>
<td>Examples:<br />
&mdash;Example:
<code>
f = \x -> x + x
</code>
<br>
&emsp;Found:
<code>
f = \ x -> x + x
</code>
<br>
&emsp;Suggestion:
<code>
f x = x + x
</code>
<br>
&mdash;Example:
<code>
f (Foo a b c) = \c -> c + c
</code>
<br>
&emsp;Found:
<code>
f (Foo a b c) = \ c -> c + c
</code>
<br>
&emsp;Suggestion:
<code>
f (Foo a b _) c = c + c
</code>
<br>
&mdash;Example:
<code>
f (Just a) = \a -> a + a
</code>
<br>
&emsp;Found:
<code>
f (Just a) = \ a -> a + a
</code>
<br>
&emsp;Suggestion:
<code>
f (Just _) a = a + a
</code>
<br>
&mdash;Example:
<code>
a = \x -> x + x
</code>
<br>
&emsp;Found:
<code>
a = \ x -> x + x
</code>
<br>
&emsp;Suggestion:
<code>
a x = x + x
</code>
<br>
&mdash;Example:
<code>
f a = \a -> a + a
</code>
<br>
&emsp;Found:
<code>
f a = \ a -> a + a
</code>
<br>
&emsp;Suggestion:
<code>
f _ a = a + a
</code>
<br>
&mdash;Example:
<code>
f a = \x -> x + x
</code>
<br>
&emsp;Found:
<code>
f a = \ x -> x + x
</code>
<br>
&emsp;Suggestion:
<code>
f a x = x + x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Eta reduce</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo a b c = bar (flux ++ quux) c where flux = a
</code>
<br>
&emsp;Found:
<code>
foo a b c = bar (flux ++ quux) c
</code>
<br>
&emsp;Suggestion:
<code>
foo a b = bar (flux ++ quux)
</code>
<br>
&mdash;Example:
<code>
fun a b = let g x y = h x y in f a b c
</code>
<br>
&emsp;Found:
<code>
g x y = h x y
</code>
<br>
&emsp;Suggestion:
<code>
g = h
</code>
<br>
&mdash;Example:
<code>
fun a b = f a b c where g x y = h x y
</code>
<br>
&emsp;Found:
<code>
g x y = h x y
</code>
<br>
&emsp;Suggestion:
<code>
g = h
</code>
<br>
&mdash;Example:
<code>
fun x = f . g $ x
</code>
<br>
&emsp;Found:
<code>
fun x = f . g $ x
</code>
<br>
&emsp;Suggestion:
<code>
fun = f . g
</code>
<br>
&mdash;Example:
<code>
fun x y z = f g z
</code>
<br>
&emsp;Found:
<code>
fun x y z = f g z
</code>
<br>
&emsp;Suggestion:
<code>
fun x y = f g
</code>
<br>
&mdash;Example:
<code>
fun x y z = f x x y z
</code>
<br>
&emsp;Found:
<code>
fun x y z = f x x y z
</code>
<br>
&emsp;Suggestion:
<code>
fun x = f x x
</code>
<br>
&mdash;Example:
<code>
fun x y z = f x y z
</code>
<br>
&emsp;Found:
<code>
fun x y z = f x y z
</code>
<br>
&emsp;Suggestion:
<code>
fun = f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Collapse lambdas</td>
<td>Examples:<br />
&mdash;Example:
<code>
f = foo (\x -> \y -> \z -> x x y y z z)
</code>
<br>
&emsp;Found:
<code>
\ x -> \ y -> \ z -> x x y y z z
</code>
<br>
&emsp;Suggestion:
<code>
\ x y z -> x x y y z z
</code>
<br>
&mdash;Example:
<code>
f = foo (\(x:xs) -> \x -> foo x x)
</code>
<br>
&emsp;Found:
<code>
\ (x : xs) -> \ x -> foo x x
</code>
<br>
&emsp;Suggestion:
<code>
\ (_ : xs) x -> foo x x
</code>
<br>
&mdash;Example:
<code>
f = foo (\x -> \x -> foo x x)
</code>
<br>
&emsp;Found:
<code>
\ x -> \ x -> foo x x
</code>
<br>
&emsp;Suggestion:
<code>
\ _ x -> foo x x
</code>
<br>
&mdash;Example:
<code>
f = foo (\x -> \y -> x x y y)
</code>
<br>
&emsp;Found:
<code>
\ x -> \ y -> x x y y
</code>
<br>
&emsp;Suggestion:
<code>
\ x y -> x x y y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Avoid lambda using `infix`</td>
<td>Examples:<br />
&mdash;Example:
<code>
f = a b (\x -> c x d) 
</code>
<br>
&emsp;Found:
<code>
(\ x -> c x d)
</code>
<br>
&emsp;Suggestion:
<code>
(`c` d)
</code>
<br>
&mdash;Example:
<code>
f = foo (\x -> x * y)
</code>
<br>
&emsp;Found:
<code>
(\ x -> x * y)
</code>
<br>
&emsp;Suggestion:
<code>
(* y)
</code>
<br>
&mdash;Example:
<code>
foo x = bar (\ d -> search d table)
</code>
<br>
&emsp;Found:
<code>
(\ d -> search d table)
</code>
<br>
&emsp;Suggestion:
<code>
(`search` table)
</code>
<br>
&mdash;Example:
<code>
foo x = bar (\ d -> search d table)
</code>
<br>
&emsp;Found:
<code>
(\ d -> search d table)
</code>
<br>
&emsp;Suggestion:
<code>
(`search` table)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Avoid lambda</td>
<td>Examples:<br />
&mdash;Example:
<code>
baz = bar (\x -> (x +))
</code>
<br>
&emsp;Found:
<code>
\ x -> (x +)
</code>
<br>
&emsp;Suggestion:
<code>
(+)
</code>
<br>
&mdash;Example:
<code>
foo = bar (\x -> (x `f`))
</code>
<br>
&emsp;Found:
<code>
(\ x -> (x `f`))
</code>
<br>
&emsp;Suggestion:
<code>
f
</code>
<br>
&mdash;Example:
<code>
yes = foo (\x -> Just x)
</code>
<br>
&emsp;Found:
<code>
(\ x -> Just x)
</code>
<br>
&emsp;Suggestion:
<code>
Just
</code>
<br>
&mdash;Example:
<code>
yes = \x -> a x where
</code>
<br>
&emsp;Found:
<code>
\ x -> a x
</code>
<br>
&emsp;Suggestion:
<code>
a
</code>
<br>
&mdash;Example:
<code>
f = foo (\x y -> x + y)
</code>
<br>
&emsp;Found:
<code>
\ x y -> x + y
</code>
<br>
&emsp;Suggestion:
<code>
(+)
</code>
<br>
&mdash;Example:
<code>
f = foo (\x y z -> fun x y z)
</code>
<br>
&emsp;Found:
<code>
(\ x y z -> fun x y z)
</code>
<br>
&emsp;Suggestion:
<code>
fun
</code>
<br>
&mdash;Example:
<code>
f = foo (\x y -> fun x y)
</code>
<br>
&emsp;Found:
<code>
(\ x y -> fun x y)
</code>
<br>
&emsp;Suggestion:
<code>
fun
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Avoid lambda</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = map (\f -> dataDir </> f) dataFiles
</code>
<br>
&emsp;Found:
<code>
(\ f -> dataDir </> f)
</code>
<br>
&emsp;Suggestion:
<code>
(dataDir </>)
</code>
<br>
&mdash;Example:
<code>
f = \x -> bar map (filter x) where
</code>
<br>
&emsp;Found:
<code>
\ x -> bar map (filter x)
</code>
<br>
&emsp;Suggestion:
<code>
bar map . filter
</code>
<br>
&mdash;Example:
<code>
f = \z -> foo $ bar x $ baz z where
</code>
<br>
&emsp;Found:
<code>
\ z -> foo $ bar x $ baz z
</code>
<br>
&emsp;Suggestion:
<code>
foo . bar x . baz
</code>
<br>
&mdash;Example:
<code>
f = \z -> foo $ bar $ baz z where
</code>
<br>
&emsp;Found:
<code>
\ z -> foo $ bar $ baz z
</code>
<br>
&emsp;Suggestion:
<code>
foo . bar . baz
</code>
<br>
&mdash;Example:
<code>
f = \y -> nub $ reverse y where
</code>
<br>
&emsp;Found:
<code>
\ y -> nub $ reverse y
</code>
<br>
&emsp;Suggestion:
<code>
nub . reverse
</code>
<br>
&mdash;Example:
<code>
yes = \x y -> op z y x where
</code>
<br>
&emsp;Found:
<code>
\ x y -> op z y x
</code>
<br>
&emsp;Suggestion:
<code>
flip (op z)
</code>
<br>
&mdash;Example:
<code>
yes = \x y -> op y x where
</code>
<br>
&emsp;Found:
<code>
\ x y -> op y x
</code>
<br>
&emsp;Suggestion:
<code>
flip op
</code>
<br>
&mdash;Example:
<code>
f = foo (\z -> f x $ z)
</code>
<br>
&emsp;Found:
<code>
\ z -> f x $ z
</code>
<br>
&emsp;Suggestion:
<code>
f x
</code>
<br>
&mdash;Example:
<code>
f = foo (\y -> g x . h $ y)
</code>
<br>
&emsp;Found:
<code>
\ y -> g x . h $ y
</code>
<br>
&emsp;Suggestion:
<code>
g x . h
</code>
<br>
&mdash;Example:
<code>
f = foo (\y -> g x . h $ y)
</code>
<br>
&emsp;Found:
<code>
\ y -> g x . h $ y
</code>
<br>
&emsp;Suggestion:
<code>
g x . h
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
</table>

## Builtin List

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use list literal pattern</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes (1:2:[]) = 1
</code>
<br>
&emsp;Found:
<code>
(1 : 2 : [])
</code>
<br>
&emsp;Suggestion:
<code>
[1, 2]
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use list literal</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = [1] : [2] : [3] : [4] : [5] : []
</code>
<br>
&emsp;Found:
<code>
[1] : [2] : [3] : [4] : [5] : []
</code>
<br>
&emsp;Suggestion:
<code>
[[1], [2], [3], [4], [5]]
</code>
<br>
&mdash;Example:
<code>
yes = 1:2:[]
</code>
<br>
&emsp;Found:
<code>
1 : 2 : []
</code>
<br>
&emsp;Suggestion:
<code>
[1, 2]
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use String</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = y :: [Char] -> a
</code>
<br>
&emsp;Found:
<code>
[Char] -> a
</code>
<br>
&emsp;Suggestion:
<code>
String -> a
</code>
<br>
&mdash;Example:
<code>
data Yes = Yes (Maybe [Char])
</code>
<br>
&emsp;Found:
<code>
Maybe [Char]
</code>
<br>
&emsp;Suggestion:
<code>
Maybe String
</code>
<br>
</td>
<td>Ignore</td>
</tr>
<tr>
<td>Use :</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo = [a b] ++ xs
</code>
<br>
&emsp;Found:
<code>
[a b] ++ xs
</code>
<br>
&emsp;Suggestion:
<code>
a b : xs
</code>
<br>
&mdash;Example:
<code>
yes = if x == e then l2 ++ xs else [x] ++ check_elem xs
</code>
<br>
&emsp;Found:
<code>
[x] ++ check_elem xs
</code>
<br>
&emsp;Suggestion:
<code>
x : check_elem xs
</code>
<br>
&mdash;Example:
<code>
yes = [if a then b else c] ++ xs
</code>
<br>
&emsp;Found:
<code>
[if a then b else c] ++ xs
</code>
<br>
&emsp;Suggestion:
<code>
(if a then b else c) : xs
</code>
<br>
&mdash;Example:
<code>
yes = [x] ++ xs
</code>
<br>
&emsp;Found:
<code>
[x] ++ xs
</code>
<br>
&emsp;Suggestion:
<code>
x : xs
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Short-circuited list comprehension</td>
<td>Examples:<br />
&mdash;Example:
<pre>
{-# LANGUAGE MonadComprehensions #-}
foo = [x | False, x <- [1 .. 10]]
</pre>
&emsp;Found:
<code>
[x | False, x <- [1 .. 10]]
</code>
<br>
&emsp;Suggestion:
<code>
[]
</code>
<br>
&mdash;Example:
<code>
foo = [myexpr | False]
</code>
<br>
&emsp;Found:
<code>
[myexpr | False]
</code>
<br>
&emsp;Suggestion:
<code>
[]
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant True guards</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo = [myexpr | True, a]
</code>
<br>
&emsp;Found:
<code>
[myexpr | True, a]
</code>
<br>
&emsp;Suggestion:
<code>
[myexpr | a]
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Move map inside list comprehension</td>
<td>Examples:<br />
&mdash;Example:
<code>
issue1039 = foo (map f [1 | _ <- []])
</code>
<br>
&emsp;Found:
<code>
map f [1 | _ <- []]
</code>
<br>
&emsp;Suggestion:
<code>
[f 1 | _ <- []]
</code>
<br>
&mdash;Example:
<code>
foo = map f [x + 1 | x <- [1..10]]
</code>
<br>
&emsp;Found:
<code>
map f [x + 1 | x <- [1 .. 10]]
</code>
<br>
&emsp;Suggestion:
<code>
[f (x + 1) | x <- [1 .. 10]]
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Move guards forward</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo = [x + 1 | x <- [1..10], let q = even 1, q]
</code>
<br>
&emsp;Found:
<code>
[x + 1 | x <- [1 .. 10], let q = even 1, q]
</code>
<br>
&emsp;Suggestion:
<code>
[x + 1 | let q = even 1, q, x <- [1 .. 10]]
</code>
<br>
&mdash;Example:
<code>
foo = [x + 1 | x <- [1..10], feature]
</code>
<br>
&emsp;Found:
<code>
[x + 1 | x <- [1 .. 10], feature]
</code>
<br>
&emsp;Suggestion:
<code>
[x + 1 | feature, x <- [1 .. 10]]
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
</table>

## Builtin ListRec

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use map</td>
<td>Examples:<br />
&mdash;Example:
<code>
f a (x:xs) b = x + a + b : f a xs b ; f a [] b = []
</code>
<br>
&emsp;Found:
<pre>
f a (x : xs) b = x + a + b : f a xs b
f a [] b = []
</pre>
&emsp;Suggestion:
<code>
f a xs b = map (\ x -> x + a + b) xs
</code>
<br>
&mdash;Example:
<code>
f (x:xs) = x + 1 : f xs ; f [] = []
</code>
<br>
&emsp;Found:
<pre>
f (x : xs) = x + 1 : f xs
f [] = []
</pre>
&emsp;Suggestion:
<code>
f xs = map (+ 1) xs
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use foldr</td>
<td>Examples:<br />
&mdash;Example:
<code>
foos [] x = x; foos (y:ys) x = foo y $ foos ys x
</code>
<br>
&emsp;Found:
<pre>
foos [] x = x
foos (y : ys) x = foo y $ foos ys x
</pre>
&emsp;Suggestion:
<code>
foos ys x = foldr foo x ys
</code>
<br>
&mdash;Example:
<code>
f (x:xs) = negate x + f xs ; f [] = 0
</code>
<br>
&emsp;Found:
<pre>
f (x : xs) = negate x + f xs
f [] = 0
</pre>
&emsp;Suggestion:
<code>
f xs = foldr ((+) . negate) 0 xs
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use foldl</td>
<td>Examples:<br />
&mdash;Example:
<code>
f [] y = y; f (x : xs) y = let z = g x y in f xs z
</code>
<br>
&emsp;Found:
<pre>
f [] y = y
f (x : xs) y = let z = g x y in f xs z
</pre>
&emsp;Suggestion:
<code>
f xs y = foldl (flip g) y xs
</code>
<br>
&mdash;Example:
<code>
f [] y = y; f (x:xs) y = f xs $ g x y
</code>
<br>
&emsp;Found:
<pre>
f [] y = y
f (x : xs) y = f xs $ g x y
</pre>
&emsp;Suggestion:
<code>
f xs y = foldl (flip g) y xs
</code>
<br>
&mdash;Example:
<code>
f z (x:xs) = f (z*x) xs ; f z [] = z
</code>
<br>
&emsp;Found:
<pre>
f z (x : xs) = f (z * x) xs
f z [] = z
</pre>
&emsp;Suggestion:
<code>
f z xs = foldl (*) z xs
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use foldM</td>
<td>Examples:<br />
&mdash;Example:
<code>
f (x:xs) a = a + x >>= \fax -> f xs fax ; f [] a = pure a
</code>
<br>
&emsp;Found:
<pre>
f (x : xs) a = a + x >>= \ fax -> f xs fax
f [] a = pure a
</pre>
&emsp;Suggestion:
<code>
f xs a = foldM (+) a xs
</code>
<br>
&mdash;Example:
<code>
f [] a = return a ; f (x:xs) a = a + x >>= \fax -> f xs fax
</code>
<br>
&emsp;Found:
<pre>
f [] a = return a
f (x : xs) a = a + x >>= \ fax -> f xs fax
</pre>
&emsp;Suggestion:
<code>
f xs a = foldM (+) a xs
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
</table>

## Builtin Monad

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use mapM_</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = mapM async ds >>= mapM wait >> return ()
</code>
<br>
&emsp;Found:
<code>
mapM async ds >>= mapM wait
</code>
<br>
&emsp;Suggestion:
<code>
mapM async ds >>= mapM_ wait
</code>
<br>
&mdash;Example:
<code>
yes = do _ <- mapM print a; return b
</code>
<br>
&emsp;Found:
<code>
mapM print a
</code>
<br>
&emsp;Suggestion:
<code>
mapM_ print a
</code>
<br>
&mdash;Example:
<code>
yes = do mapM print a; return b
</code>
<br>
&emsp;Found:
<code>
mapM print a
</code>
<br>
&emsp;Suggestion:
<code>
mapM_ print a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use let</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = do x <- return $ y + z; foo x
</code>
<br>
&emsp;Found:
<code>
x <- return $ y + z
</code>
<br>
&emsp;Suggestion:
<code>
let x = y + z
</code>
<br>
&mdash;Example:
<code>
yes = do x <- return y; foo x
</code>
<br>
&emsp;Found:
<code>
x <- return y
</code>
<br>
&emsp;Suggestion:
<code>
let x = y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use join</td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = do x <- bar; x
</code>
<br>
&emsp;Found:
<pre>
do x <- bar
   x
</pre>
&emsp;Suggestion:
<code>
do join bar
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use forM_</td>
<td>Examples:<br />
&mdash;Example:
<code>
main = void $ forM f xs
</code>
<br>
&emsp;Found:
<code>
void $ forM f xs
</code>
<br>
&emsp;Suggestion:
<code>
void $ forM_ f xs
</code>
<br>
&mdash;Example:
<code>
yes = do case a of {_ -> forM x y; x:xs -> foo xs}; return ()
</code>
<br>
&emsp;Found:
<code>
forM x y
</code>
<br>
&emsp;Suggestion:
<code>
forM_ x y
</code>
<br>
&mdash;Example:
<code>
yes = do if a then forM x y else return (); return 12
</code>
<br>
&emsp;Found:
<code>
forM x y
</code>
<br>
&emsp;Suggestion:
<code>
forM_ x y
</code>
<br>
&mdash;Example:
<code>
yes = do forM files $ \x -> return (); return ()
</code>
<br>
&emsp;Found:
<code>
forM files $ \ x -> return ()
</code>
<br>
&emsp;Suggestion:
<code>
forM_ files $ \ x -> return ()
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use foldM_</td>
<td>Examples:<br />
&mdash;Example:
<code>
folder f a xs = foldM f a xs >>= \_ -> return ()
</code>
<br>
&emsp;Found:
<code>
foldM f a xs
</code>
<br>
&emsp;Suggestion:
<code>
foldM_ f a xs
</code>
<br>
&mdash;Example:
<code>
folder f a xs = foldM f a xs >> return ()
</code>
<br>
&emsp;Found:
<code>
foldM f a xs
</code>
<br>
&emsp;Suggestion:
<code>
foldM_ f a xs
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <$></td>
<td>Examples:<br />
&mdash;Example:
<code>
yes = do x <- bar; return (f $ g x)
</code>
<br>
&emsp;Found:
<pre>
do x <- bar
   return (f $ g x)
</pre>
&emsp;Suggestion:
<code>
do f . g <$> bar
</code>
<br>
&mdash;Example:
<code>
yes = do x <- bar; return $ f (g x)
</code>
<br>
&emsp;Found:
<pre>
do x <- bar
   return $ f (g x)
</pre>
&emsp;Suggestion:
<code>
do f . g <$> bar
</code>
<br>
&mdash;Example:
<code>
yes = do x <- bar; pure $ f x
</code>
<br>
&emsp;Found:
<pre>
do x <- bar
   pure $ f x
</pre>
&emsp;Suggestion:
<code>
do f <$> bar
</code>
<br>
&mdash;Example:
<code>
yes = do x <- bar; return $ f x
</code>
<br>
&emsp;Found:
<pre>
do x <- bar
   return $ f x
</pre>
&emsp;Suggestion:
<code>
do f <$> bar
</code>
<br>
&mdash;Example:
<code>
yes = do x <- bar; return (f x)
</code>
<br>
&emsp;Found:
<pre>
do x <- bar
   return (f x)
</pre>
&emsp;Suggestion:
<code>
do f <$> bar
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant void</td>
<td>Examples:<br />
&mdash;Example:
<code>
main = void $ forM_ f xs
</code>
<br>
&emsp;Found:
<code>
void $ forM_ f xs
</code>
<br>
&emsp;Suggestion:
<code>
forM_ f xs
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant variable capture</td>
<td>Examples:<br />
&mdash;Example:
<code>
main = do _ <- forM_ f xs; bar
</code>
<br>
&emsp;Found:
<code>
_ <- forM_ f xs
</code>
<br>
&emsp;Suggestion:
<code>
forM_ f xs
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant return</td>
<td>Examples:<br />
&mdash;Example:
<code>
main = do a; when b c; return ()
</code>
<br>
&emsp;Found:
<pre>
do a
   when b c
   return ()
</pre>
&emsp;Suggestion:
<pre>
do a
   when b c
</pre>
&mdash;Example:
<code>
main = do bar; forM_ f xs; return ()
</code>
<br>
&emsp;Found:
<pre>
do bar
   forM_ f xs
   return ()
</pre>
&emsp;Suggestion:
<pre>
do bar
   forM_ f xs
</pre>
&mdash;Example:
<code>
main = do foo x; return 3; bar z
</code>
<br>
&emsp;Found:
<code>
return 3
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<code>
yes = do bar; a <- foo; return a
</code>
<br>
&emsp;Found:
<pre>
do bar
   a <- foo
   return a
</pre>
&emsp;Suggestion:
<pre>
do bar
   foo
</pre>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant do</td>
<td>Examples:<br />
&mdash;Example:
<code>
main = do f a $ sleep 10
</code>
<br>
&emsp;Found:
<code>
do
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<code>
main = f $ do g a $ sleep 10
</code>
<br>
&emsp;Found:
<code>
do
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE BlockArguments #-}; main = print do 17
</code>
<br>
&emsp;Found:
<code>
do
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
&mdash;Example:
<code>
yes = do (bar+foo)
</code>
<br>
&emsp;Found:
<code>
do
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
</td>
<td>Ignore</td>
</tr>
</table>

## Builtin Naming

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use camelCase</td>
<td>Examples:<br />
&mdash;Example:
<code>
cast_foo = 1
</code>
<br>
&emsp;Found:
<code>
cast_foo = ...
</code>
<br>
&emsp;Suggestion:
<code>
castFoo = ...
</code>
<br>
&mdash;Example:
<code>
data Yes = FOO_A | Foo_B
</code>
<br>
&emsp;Found:
<code>
data Yes = FOO_A | Foo_B
</code>
<br>
&emsp;Suggestion:
<code>
data Yes = FOO_A | FooB
</code>
<br>
&mdash;Example:
<code>
yes_fooPattern Nothing = 0
</code>
<br>
&emsp;Found:
<code>
yes_fooPattern Nothing = ...
</code>
<br>
&emsp;Suggestion:
<code>
yesFooPattern Nothing = ...
</code>
<br>
&mdash;Example:
<code>
yes_foo = yes_foo + yes_foo
</code>
<br>
&emsp;Found:
<code>
yes_foo = ...
</code>
<br>
&emsp;Suggestion:
<code>
yesFoo = ...
</code>
<br>
&mdash;Example:
<code>
data Yes = Bar | Test_Bar
</code>
<br>
&emsp;Found:
<code>
data Yes = Bar | Test_Bar
</code>
<br>
&emsp;Suggestion:
<code>
data Yes = Bar | TestBar
</code>
<br>
Does not support refactoring.
</td>
<td>Suggestion</td>
</tr>
</table>

## Builtin NewType

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use newtype instead of data</td>
<td>Examples:<br />
&mdash;Example:
<code>
data instance Foo Int = Bar {field :: Bool}
</code>
<br>
&emsp;Found:
<code>
data instance Foo Int = Bar {field :: Bool}
</code>
<br>
&emsp;Suggestion:
<code>
newtype instance Foo Int = Bar {field :: Bool}
</code>
<br>
&mdash;Example:
<code>
data instance Foo Int = Bar Bool
</code>
<br>
&emsp;Found:
<code>
data instance Foo Int = Bar Bool
</code>
<br>
&emsp;Suggestion:
<code>
newtype instance Foo Int = Bar Bool
</code>
<br>
&mdash;Example:
<code>
data A = A ()
</code>
<br>
&emsp;Found:
<code>
data A = A ()
</code>
<br>
&emsp;Suggestion:
<code>
newtype A = A ()
</code>
<br>
&mdash;Example:
<code>
data A = A {b :: !C}
</code>
<br>
&emsp;Found:
<code>
data A = A {b :: !C}
</code>
<br>
&emsp;Suggestion:
<code>
newtype A = A {b :: C}
</code>
<br>
&mdash;Example:
<code>
data X = Y {-# UNPACK #-} !Int
</code>
<br>
&emsp;Found:
<code>
data X = Y {-# UNPACK #-} !Int
</code>
<br>
&emsp;Suggestion:
<code>
newtype X = Y Int
</code>
<br>
&mdash;Example:
<code>
data Foo a = () => Foo a
</code>
<br>
&emsp;Found:
<code>
data Foo a = Foo a
</code>
<br>
&emsp;Suggestion:
<code>
newtype Foo a = Foo a
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE RankNTypes #-}; data Foo = Foo (forall a . a)
</code>
<br>
&emsp;Found:
<code>
data Foo = Foo (forall a. a)
</code>
<br>
&emsp;Suggestion:
<code>
newtype Foo = Foo (forall a. a)
</code>
<br>
&mdash;Example:
<code>
data Foo a b = Foo a
</code>
<br>
&emsp;Found:
<code>
data Foo a b = Foo a
</code>
<br>
&emsp;Suggestion:
<code>
newtype Foo a b = Foo a
</code>
<br>
&mdash;Example:
<code>
data Foo = Foo { field :: Int } deriving Show
</code>
<br>
&emsp;Found:
<pre>
data Foo
  = Foo {field :: Int}
  deriving Show
</pre>
&emsp;Suggestion:
<pre>
newtype Foo
  = Foo {field :: Int}
  deriving Show
</pre>
&mdash;Example:
<code>
data Foo = Foo Int deriving (Show, Eq)
</code>
<br>
&emsp;Found:
<pre>
data Foo
  = Foo Int
  deriving (Show, Eq)
</pre>
&emsp;Suggestion:
<pre>
newtype Foo
  = Foo Int
  deriving (Show, Eq)
</pre>
&mdash;Example:
<code>
data Foo = Foo Int
</code>
<br>
&emsp;Found:
<code>
data Foo = Foo Int
</code>
<br>
&emsp;Suggestion:
<code>
newtype Foo = Foo Int
</code>
<br>
Does not support refactoring.
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use DerivingStrategies</td>
<td>Examples:<br />
&mdash;Example:
<code>
newtype instance Foo Int = Bar {field :: Bool} deriving Show
</code>
<br>
&emsp;Found:
<pre>
newtype instance Foo Int
  = Bar {field :: Bool}
  deriving Show
</pre>
&emsp;Suggestion:
<code>

</code>
<br>
&mdash;Example:
<code>
newtype instance Foo Int = Bar Bool deriving (Show, Eq)
</code>
<br>
&emsp;Found:
<pre>
newtype instance Foo Int
  = Bar Bool
  deriving (Show, Eq)
</pre>
&emsp;Suggestion:
<code>

</code>
<br>
&mdash;Example:
<code>
newtype Foo = Foo { getFoo :: Int } deriving (Show, Eq)
</code>
<br>
&emsp;Found:
<pre>
newtype Foo
  = Foo {getFoo :: Int}
  deriving (Show, Eq)
</pre>
&emsp;Suggestion:
<code>

</code>
<br>
&mdash;Example:
<code>
newtype Foo = Foo Int deriving (Show, Eq)
</code>
<br>
&emsp;Found:
<pre>
newtype Foo
  = Foo Int
  deriving (Show, Eq)
</pre>
&emsp;Suggestion:
<code>

</code>
<br>
Does not support refactoring.
</td>
<td>Ignore</td>
</tr>
</table>

## Builtin NumLiteral

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use underscore</td>
<td>Examples:<br />
&mdash;Example:
<pre>
{-# LANGUAGE NumericUnderscores #-} 
3.14159265359
</pre>
&emsp;Found:
<code>
3.14159265359
</code>
<br>
&emsp;Suggestion:
<code>
3.141_592_653_59
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE NumericUnderscores #-} 
0x12abc.523defp+172345
</pre>
&emsp;Found:
<code>
0x12abc.523defp+172345
</code>
<br>
&emsp;Suggestion:
<code>
0x1_2abc.523d_efp+172_345
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE NumericUnderscores #-} 
123456789.0441234e-123456
</pre>
&emsp;Found:
<code>
123456789.0441234e-123456
</code>
<br>
&emsp;Suggestion:
<code>
123_456_789.044_123_4e-123_456
</code>
<br>
&mdash;Example:
<pre>
{-# LANGUAGE NumericUnderscores #-} 
12345
</pre>
&emsp;Found:
<code>
12345
</code>
<br>
&emsp;Suggestion:
<code>
12_345
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
</table>

## Builtin Pattern

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Used otherwise as a pattern</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo otherwise = 1
</code>
<br>
&emsp;Found:
<code>
otherwise
</code>
<br>
&emsp;Suggestion:
<code>
_
</code>
<br>
Does not support refactoring.
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use record patterns</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo (Bar _ _ _ _) = x
</code>
<br>
&emsp;Found:
<code>
Bar _ _ _ _
</code>
<br>
&emsp;Suggestion:
<code>
Bar {}
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use otherwise</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo x | a = b | True = d
</code>
<br>
&emsp;Found:
<pre>
foo x
  | a = b
  | True = d
</pre>
&emsp;Suggestion:
<pre>
foo x
  | a = b
  | otherwise = d
</pre>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use guards</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo x = yes x x where yes x y = if a then b else if c then d else e
</code>
<br>
&emsp;Found:
<code>
yes x y = if a then b else if c then d else e
</code>
<br>
&emsp;Suggestion:
<pre>
yes x y
  | a = b
  | c = d
  | otherwise = e
</pre>
&mdash;Example:
<code>
x `yes` y = if a then b else if c then d else e
</code>
<br>
&emsp;Found:
<code>
x `yes` y = if a then b else if c then d else e
</code>
<br>
&emsp;Suggestion:
<pre>
x `yes` y
  | a = b
  | c = d
  | otherwise = e
</pre>
&mdash;Example:
<code>
yes x y = if a then b else if c then d else e
</code>
<br>
&emsp;Found:
<code>
yes x y = if a then b else if c then d else e
</code>
<br>
&emsp;Suggestion:
<pre>
yes x y
  | a = b
  | c = d
  | otherwise = e
</pre>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant where</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo x = x + x where
</code>
<br>
&emsp;Found:
<code>
where
</code>
<br>
&emsp;Suggestion:
<code>
Perhaps you should remove it.
</code>
<br>
Does not support refactoring.
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant irrefutable pattern</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo ~x = y
</code>
<br>
&emsp;Found:
<code>
~x
</code>
<br>
&emsp;Suggestion:
<code>
x
</code>
<br>
&mdash;Example:
<code>
foo = let ~x = 1 in y
</code>
<br>
&emsp;Found:
<code>
~x
</code>
<br>
&emsp;Suggestion:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant guard</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo x | otherwise = y
</code>
<br>
&emsp;Found:
<code>
foo x | otherwise = y
</code>
<br>
&emsp;Suggestion:
<code>
foo x = y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant case</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo = case v of v -> x
</code>
<br>
&emsp;Found:
<code>
case v of v -> x
</code>
<br>
&emsp;Suggestion:
<code>
x
</code>
<br>
&mdash;Example:
<code>
foo = case f v of _ -> x
</code>
<br>
&emsp;Found:
<code>
case f v of _ -> x
</code>
<br>
&emsp;Suggestion:
<code>
x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant bang pattern</td>
<td>Examples:<br />
&mdash;Example:
<code>
{-# LANGUAGE BangPatterns #-}; l !(() :: ()) = x
</code>
<br>
&emsp;Found:
<code>
!(() :: ())
</code>
<br>
&emsp;Suggestion:
<code>
(() :: ())
</code>
<br>
&mdash;Example:
<code>
foo !Bar { bar = x } = x
</code>
<br>
&emsp;Found:
<code>
!Bar {bar = x}
</code>
<br>
&emsp;Suggestion:
<code>
Bar {bar = x}
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE BangPatterns #-}; foo ![x] = x
</code>
<br>
&emsp;Found:
<code>
![x]
</code>
<br>
&emsp;Suggestion:
<code>
[x]
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE BangPatterns #-}; foo !(x, y) = x
</code>
<br>
&emsp;Found:
<code>
!(x, y)
</code>
<br>
&emsp;Suggestion:
<code>
(x, y)
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE BangPatterns #-}; foo = 1 where g (Just !True) = Nothing
</code>
<br>
&emsp;Found:
<code>
!True
</code>
<br>
&emsp;Suggestion:
<code>
True
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE BangPatterns #-}; foo = 1 where f !False = 2
</code>
<br>
&emsp;Found:
<code>
!False
</code>
<br>
&emsp;Suggestion:
<code>
False
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE BangPatterns #-}; foo = case v of !(I# x) -> y
</code>
<br>
&emsp;Found:
<code>
!(I# x)
</code>
<br>
&emsp;Suggestion:
<code>
(I# x)
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE BangPatterns #-}; foo = case v of !1 -> x
</code>
<br>
&emsp;Found:
<code>
!1
</code>
<br>
&emsp;Suggestion:
<code>
1
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE BangPatterns #-}; foo = case v of !(x : xs) -> x
</code>
<br>
&emsp;Found:
<code>
!(x : xs)
</code>
<br>
&emsp;Suggestion:
<code>
(x : xs)
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE BangPatterns #-}; foo = case v of !(Just x) -> x
</code>
<br>
&emsp;Found:
<code>
!(Just x)
</code>
<br>
&emsp;Suggestion:
<code>
(Just x)
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE BangPatterns #-}; foo = case v of !True -> x
</code>
<br>
&emsp;Found:
<code>
!True
</code>
<br>
&emsp;Suggestion:
<code>
True
</code>
<br>
&mdash;Example:
<code>
foo = case v of !True -> x
</code>
<br>
&emsp;Found:
<code>
!True
</code>
<br>
&emsp;Suggestion:
<code>
True
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant as-pattern</td>
<td>Examples:<br />
&mdash;Example:
<code>
foo x@_ = x
</code>
<br>
&emsp;Found:
<code>
x@_
</code>
<br>
&emsp;Suggestion:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
</table>

## Builtin Pragma

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use fewer LANGUAGE pragmas</td>
<td>Examples:<br />
&mdash;Example:
<pre>
{-# LANGUAGE RebindableSyntax #-} 
{-# LANGUAGE EmptyCase, RebindableSyntax #-}
</pre>
&emsp;Found:
<pre>
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE EmptyCase, RebindableSyntax #-}

</pre>
&emsp;Suggestion:
<code>
{-# LANGUAGE RebindableSyntax, EmptyCase #-}
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE RebindableSyntax, EmptyCase, DuplicateRecordFields, RebindableSyntax #-}
</code>
<br>
&emsp;Found:
<code>
{-# LANGUAGE RebindableSyntax, EmptyCase, DuplicateRecordFields, RebindableSyntax #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE RebindableSyntax, EmptyCase, DuplicateRecordFields #-}
</code>
<br>
&mdash;Example:
<code>
{-# LANGUAGE RebindableSyntax, EmptyCase, RebindableSyntax #-}
</code>
<br>
&emsp;Found:
<code>
{-# LANGUAGE RebindableSyntax, EmptyCase, RebindableSyntax #-}
</code>
<br>
&emsp;Suggestion:
<code>
{-# LANGUAGE RebindableSyntax, EmptyCase #-}
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use LANGUAGE pragmas</td>
<td>Examples:<br />
&mdash;Example:
<code>
{-# OPTIONS_GHC -cpp -w #-}
</code>
<br>
&emsp;Found:
<pre>
{-# OPTIONS_GHC -cpp -w #-}

</pre>
&emsp;Suggestion:
<pre>
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -w #-}

</pre>
&mdash;Example:
<code>
{-# OPTIONS_GHC -cpp -foo #-}
</code>
<br>
&emsp;Found:
<pre>
{-# OPTIONS_GHC -cpp -foo #-}

</pre>
&emsp;Suggestion:
<pre>
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -foo #-}

</pre>
&mdash;Example:
<code>
{-# OPTIONS_GHC -fglasgow-exts #-}
</code>
<br>
&emsp;Found:
<pre>
{-# OPTIONS_GHC -fglasgow-exts #-}

</pre>
&emsp;Suggestion:
<pre>
{-# LANGUAGE ConstrainedClassMethods, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, ExistentialQuantification, ExplicitNamespaces, FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, FunctionalDependencies, GeneralizedNewtypeDeriving, ImplicitParams, KindSignatures, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, ParallelListComp, PatternGuards, PostfixOperators, RankNTypes, RecursiveDo, ScopedTypeVariables, StandaloneDeriving, TypeOperators, TypeSynonymInstances, UnboxedTuples, UnicodeSyntax, UnliftedFFITypes #-}

</pre>
&mdash;Example:
<code>
{-# OPTIONS_GHC -XFoo #-}
</code>
<br>
&emsp;Found:
<pre>
{-# OPTIONS_GHC -XFoo #-}

</pre>
&emsp;Suggestion:
<pre>
{-# LANGUAGE Foo #-}

</pre>
&mdash;Example:
<code>
{-# OPTIONS     -cpp #-}
</code>
<br>
&emsp;Found:
<pre>
{-# OPTIONS     -cpp #-}

</pre>
&emsp;Suggestion:
<pre>
{-# LANGUAGE CPP #-}

</pre>
&mdash;Example:
<code>
{-# OPTIONS_GHC -cpp #-}
</code>
<br>
&emsp;Found:
<pre>
{-# OPTIONS_GHC -cpp #-}

</pre>
&emsp;Suggestion:
<pre>
{-# LANGUAGE CPP #-}

</pre>
</td>
<td>Warning</td>
</tr>
</table>

## Builtin Unsafe

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Missing NOINLINE pragma</td>
<td>Examples:<br />
&mdash;Example:
<code>
entries = unsafePerformIO . baz $ x
</code>
<br>
&emsp;Found:
<code>
entries = unsafePerformIO . baz $ x
</code>
<br>
&emsp;Suggestion:
<pre>
{-# NOINLINE entries #-}
entries = unsafePerformIO . baz $ x
</pre>
&mdash;Example:
<code>
entries = unsafePerformIO . baz $ x
</code>
<br>
&emsp;Found:
<code>
entries = unsafePerformIO . baz $ x
</code>
<br>
&emsp;Suggestion:
<pre>
{-# NOINLINE entries #-}
entries = unsafePerformIO . baz $ x
</pre>
&mdash;Example:
<code>
entries = x where x = unsafePerformIO $ Multimap.newIO
</code>
<br>
&emsp;Found:
<pre>
entries
  = x
  where
      x = unsafePerformIO $ Multimap.newIO
</pre>
&emsp;Suggestion:
<pre>
{-# NOINLINE entries #-}
entries
  = x
  where
      x = unsafePerformIO $ Multimap.newIO
</pre>
&mdash;Example:
<code>
entries = unsafePerformIO $ f y where foo = 1
</code>
<br>
&emsp;Found:
<pre>
entries
  = unsafePerformIO $ f y
  where
      foo = 1
</pre>
&emsp;Suggestion:
<pre>
{-# NOINLINE entries #-}
entries
  = unsafePerformIO $ f y
  where
      foo = 1
</pre>
&mdash;Example:
<code>
entries = unsafePerformIO Multimap.newIO
</code>
<br>
&emsp;Found:
<code>
entries = unsafePerformIO Multimap.newIO
</code>
<br>
&emsp;Suggestion:
<pre>
{-# NOINLINE entries #-}
entries = unsafePerformIO Multimap.newIO
</pre>
</td>
<td>Warning</td>
</tr>
</table>

## Configured hints

<table>
<tr>
<th>Hint Name</th>
<th>Hint</th>
<th>Severity</th>
</tr>
<tr>
<td>Use print</td>
<td>
LHS:
<code>
putStrLn (show x)
</code>
<br>
RHS:
<code>
print x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use putStrLn</td>
<td>
LHS:
<code>
putStr (x ++ "\n")
</code>
<br>
RHS:
<code>
putStrLn x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use putStrLn</td>
<td>
LHS:
<code>
putStr (x ++ y ++ "\n")
</code>
<br>
RHS:
<code>
putStrLn (x ++ y)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use putStr</td>
<td>
LHS:
<code>
mapM_ putChar
</code>
<br>
RHS:
<code>
putStr
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use getChar</td>
<td>
LHS:
<code>
hGetChar stdin
</code>
<br>
RHS:
<code>
getChar
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use getLine</td>
<td>
LHS:
<code>
hGetLine stdin
</code>
<br>
RHS:
<code>
getLine
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use getContents</td>
<td>
LHS:
<code>
hGetContents stdin
</code>
<br>
RHS:
<code>
getContents
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use putChar</td>
<td>
LHS:
<code>
hPutChar stdout
</code>
<br>
RHS:
<code>
putChar
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use putStr</td>
<td>
LHS:
<code>
hPutStr stdout
</code>
<br>
RHS:
<code>
putStr
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use putStrLn</td>
<td>
LHS:
<code>
hPutStrLn stdout
</code>
<br>
RHS:
<code>
putStrLn
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use print</td>
<td>
LHS:
<code>
hPrint stdout
</code>
<br>
RHS:
<code>
print
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use hReady</td>
<td>
LHS:
<code>
hWaitForInput a 0
</code>
<br>
RHS:
<code>
hReady a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use hPrint</td>
<td>
LHS:
<code>
hPutStrLn a (show b)
</code>
<br>
RHS:
<code>
hPrint a b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isEOF</td>
<td>
LHS:
<code>
hIsEOF stdin
</code>
<br>
RHS:
<code>
isEOF
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use writeFile</td>
<td>
LHS:
<code>
withFile f WriteMode (\ h -> hPutStr h x)
</code>
<br>
RHS:
<code>
writeFile f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use writeFile</td>
<td>
LHS:
<code>
withFile f WriteMode (\ h -> hPutStrLn h x)
</code>
<br>
RHS:
<code>
writeFile f (x ++ "\n")
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use appendFile</td>
<td>
LHS:
<code>
withFile f AppendMode (\ h -> hPutStr h x)
</code>
<br>
RHS:
<code>
appendFile f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use appendFile</td>
<td>
LHS:
<code>
withFile f AppendMode (\ h -> hPutStrLn h x)
</code>
<br>
RHS:
<code>
appendFile f (x ++ "\n")
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use exitSuccess</td>
<td>
LHS:
<code>
exitWith ExitSuccess
</code>
<br>
RHS:
<code>
exitSuccess
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use /=</td>
<td>
LHS:
<code>
not (a == b)
</code>
<br>
RHS:
<code>
a /= b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use ==</td>
<td>
LHS:
<code>
not (a /= b)
</code>
<br>
RHS:
<code>
a == b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <=</td>
<td>
LHS:
<code>
not (a > b)
</code>
<br>
RHS:
<code>
a <= b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <</td>
<td>
LHS:
<code>
not (a >= b)
</code>
<br>
RHS:
<code>
a < b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use >=</td>
<td>
LHS:
<code>
not (a < b)
</code>
<br>
RHS:
<code>
a >= b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use ></td>
<td>
LHS:
<code>
not (a <= b)
</code>
<br>
RHS:
<code>
a > b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <=</td>
<td>
LHS:
<code>
compare x y /= GT
</code>
<br>
RHS:
<code>
x <= y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <</td>
<td>
LHS:
<code>
compare x y == LT
</code>
<br>
RHS:
<code>
x < y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use >=</td>
<td>
LHS:
<code>
compare x y /= LT
</code>
<br>
RHS:
<code>
x >= y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use ></td>
<td>
LHS:
<code>
compare x y == GT
</code>
<br>
RHS:
<code>
x > y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant compare</td>
<td>
LHS:
<code>
compare x y == EQ
</code>
<br>
RHS:
<code>
x == y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant compare</td>
<td>
LHS:
<code>
compare x y /= EQ
</code>
<br>
RHS:
<code>
x /= y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use minimum</td>
<td>
LHS:
<code>
head (sort x)
</code>
<br>
RHS:
<code>
minimum x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use maximum</td>
<td>
LHS:
<code>
last (sort x)
</code>
<br>
RHS:
<code>
maximum x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use minimumBy</td>
<td>
LHS:
<code>
head (sortBy f x)
</code>
<br>
RHS:
<code>
minimumBy f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use maximumBy</td>
<td>
LHS:
<code>
last (sortBy f x)
</code>
<br>
RHS:
<code>
maximumBy f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Avoid reverse</td>
<td>
LHS:
<code>
reverse (sortBy f x)
</code>
<br>
RHS:
<code>
sortBy (flip f) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sortOn</td>
<td>
LHS:
<code>
sortBy (flip (comparing f))
</code>
<br>
RHS:
<code>
sortOn (Down . f)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sortOn</td>
<td>
LHS:
<code>
sortBy (comparing f)
</code>
<br>
RHS:
<code>
sortOn f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Avoid reverse</td>
<td>
LHS:
<code>
reverse (sortOn f x)
</code>
<br>
RHS:
<code>
sortOn (Data.Ord.Down . f) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move flip</td>
<td>
LHS:
<code>
flip (g `on` h)
</code>
<br>
RHS:
<code>
flip g `on` h
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Fuse on/on</td>
<td>
LHS:
<code>
(f `on` g) `on` h
</code>
<br>
RHS:
<code>
f `on` (g . h)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use max</td>
<td>
LHS:
<code>
if a >= b then a else b
</code>
<br>
RHS:
<code>
max a b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use max</td>
<td>
LHS:
<code>
if a > b then a else b
</code>
<br>
RHS:
<code>
max a b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use min</td>
<td>
LHS:
<code>
if a <= b then a else b
</code>
<br>
RHS:
<code>
min a b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use min</td>
<td>
LHS:
<code>
if a < b then a else b
</code>
<br>
RHS:
<code>
min a b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use show</td>
<td>
LHS:
<code>
showsPrec 0 x ""
</code>
<br>
RHS:
<code>
show x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use reads</td>
<td>
LHS:
<code>
readsPrec 0
</code>
<br>
RHS:
<code>
reads
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use shows</td>
<td>
LHS:
<code>
showsPrec 0
</code>
<br>
RHS:
<code>
shows
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use showHex</td>
<td>
LHS:
<code>
showIntAtBase 16 intToDigit
</code>
<br>
RHS:
<code>
showHex
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use showOct</td>
<td>
LHS:
<code>
showIntAtBase 8 intToDigit
</code>
<br>
RHS:
<code>
showOct
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use concatMap</td>
<td>
LHS:
<code>
concat (map f x)
</code>
<br>
RHS:
<code>
concatMap f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use concatMap</td>
<td>
LHS:
<code>
concat (f <$> x)
</code>
<br>
RHS:
<code>
concatMap f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use concatMap</td>
<td>
LHS:
<code>
concat (fmap f x)
</code>
<br>
RHS:
<code>
concatMap f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use ++</td>
<td>
LHS:
<code>
concat [a, b]
</code>
<br>
RHS:
<code>
a ++ b
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use map once</td>
<td>
LHS:
<code>
map f (map g x)
</code>
<br>
RHS:
<code>
map (f . g) x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Fuse concatMap/map</td>
<td>
LHS:
<code>
concatMap f (map g x)
</code>
<br>
RHS:
<code>
concatMap (f . g) x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use head</td>
<td>
LHS:
<code>
x !! 0
</code>
<br>
RHS:
<code>
head x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use replicate</td>
<td>
LHS:
<code>
take n (repeat x)
</code>
<br>
RHS:
<code>
replicate n x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant map</td>
<td>
LHS:
<code>
map f (replicate n x)
</code>
<br>
RHS:
<code>
replicate n (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant map</td>
<td>
LHS:
<code>
map f (repeat x)
</code>
<br>
RHS:
<code>
repeat (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use repeat</td>
<td>
LHS:
<code>
cycle [x]
</code>
<br>
RHS:
<code>
repeat x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use last</td>
<td>
LHS:
<code>
head (reverse x)
</code>
<br>
RHS:
<code>
last x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use !!</td>
<td>
LHS:
<code>
head (drop n x)
</code>
<br>
RHS:
<code>
x !! n
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use !!</td>
<td>
LHS:
<code>
head (drop n x)
</code>
<br>
RHS:
<code>
x !! max 0 n
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use tail</td>
<td>
LHS:
<code>
reverse (init x)
</code>
<br>
RHS:
<code>
tail (reverse x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use init</td>
<td>
LHS:
<code>
reverse (tail (reverse x))
</code>
<br>
RHS:
<code>
init x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Avoid reverse</td>
<td>
LHS:
<code>
reverse (reverse x)
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isSuffixOf</td>
<td>
LHS:
<code>
isPrefixOf (reverse x) (reverse y)
</code>
<br>
RHS:
<code>
isSuffixOf x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use concat</td>
<td>
LHS:
<code>
foldr (++) []
</code>
<br>
RHS:
<code>
concat
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use concat</td>
<td>
LHS:
<code>
foldr (++) ""
</code>
<br>
RHS:
<code>
concat
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use concatMap</td>
<td>
LHS:
<code>
foldr ((++) . f) []
</code>
<br>
RHS:
<code>
concatMap f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use concatMap</td>
<td>
LHS:
<code>
foldr ((++) . f) ""
</code>
<br>
RHS:
<code>
concatMap f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use concat</td>
<td>
LHS:
<code>
foldl (++) []
</code>
<br>
RHS:
<code>
concat
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use concat</td>
<td>
LHS:
<code>
foldl (++) ""
</code>
<br>
RHS:
<code>
concat
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use foldl1</td>
<td>
LHS:
<code>
foldl f (head x) (tail x)
</code>
<br>
RHS:
<code>
foldl1 f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use foldr1</td>
<td>
LHS:
<code>
foldr f (last x) (init x)
</code>
<br>
RHS:
<code>
foldr1 f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use map</td>
<td>
LHS:
<code>
foldr (\ c a -> x : a) []
</code>
<br>
RHS:
<code>
map (\ c -> x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use $</td>
<td>
LHS:
<code>
foldr (.) id l z
</code>
<br>
RHS:
<code>
foldr ($) z l
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use break</td>
<td>
LHS:
<code>
span (not . p)
</code>
<br>
RHS:
<code>
break p
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use span</td>
<td>
LHS:
<code>
break (not . p)
</code>
<br>
RHS:
<code>
span p
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use span</td>
<td>
LHS:
<code>
(takeWhile p x, dropWhile p x)
</code>
<br>
RHS:
<code>
span p x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use takeWhile</td>
<td>
LHS:
<code>
fst (span p x)
</code>
<br>
RHS:
<code>
takeWhile p x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use dropWhile</td>
<td>
LHS:
<code>
snd (span p x)
</code>
<br>
RHS:
<code>
dropWhile p x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use takeWhile</td>
<td>
LHS:
<code>
fst (break p x)
</code>
<br>
RHS:
<code>
takeWhile (not . p) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use dropWhile</td>
<td>
LHS:
<code>
snd (break p x)
</code>
<br>
RHS:
<code>
dropWhile (not . p) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use splitAt</td>
<td>
LHS:
<code>
(take n x, drop n x)
</code>
<br>
RHS:
<code>
splitAt n x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use take</td>
<td>
LHS:
<code>
fst (splitAt p x)
</code>
<br>
RHS:
<code>
take p x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use drop</td>
<td>
LHS:
<code>
snd (splitAt p x)
</code>
<br>
RHS:
<code>
drop p x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use unlines</td>
<td>
LHS:
<code>
concatMap (++ "\n")
</code>
<br>
RHS:
<code>
unlines
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant map</td>
<td>
LHS:
<code>
map id
</code>
<br>
RHS:
<code>
id
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use concat</td>
<td>
LHS:
<code>
concatMap id
</code>
<br>
RHS:
<code>
concat
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use any</td>
<td>
LHS:
<code>
or (map p x)
</code>
<br>
RHS:
<code>
any p x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use all</td>
<td>
LHS:
<code>
and (map p x)
</code>
<br>
RHS:
<code>
all p x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant map</td>
<td>
LHS:
<code>
any f (map g x)
</code>
<br>
RHS:
<code>
any (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant map</td>
<td>
LHS:
<code>
all f (map g x)
</code>
<br>
RHS:
<code>
all (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use zip</td>
<td>
LHS:
<code>
zipWith (,)
</code>
<br>
RHS:
<code>
zip
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use zip3</td>
<td>
LHS:
<code>
zipWith3 (,,)
</code>
<br>
RHS:
<code>
zip3
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use unzip</td>
<td>
LHS:
<code>
map fst &&& map snd
</code>
<br>
RHS:
<code>
unzip
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
length x == 0
</code>
<br>
RHS:
<code>
null x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
0 == length x
</code>
<br>
RHS:
<code>
null x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
length x < 1
</code>
<br>
RHS:
<code>
null x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
1 > length x
</code>
<br>
RHS:
<code>
null x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
length x <= 0
</code>
<br>
RHS:
<code>
null x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
0 >= length x
</code>
<br>
RHS:
<code>
null x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
x == []
</code>
<br>
RHS:
<code>
null x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
[] == x
</code>
<br>
RHS:
<code>
null x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
all (const False)
</code>
<br>
RHS:
<code>
null
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
any (const True) x
</code>
<br>
RHS:
<code>
not (null x)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
length x /= 0
</code>
<br>
RHS:
<code>
not (null x)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
0 /= length x
</code>
<br>
RHS:
<code>
not (null x)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use :</td>
<td>
LHS:
<code>
\ x -> [x]
</code>
<br>
RHS:
<code>
(: [])
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use zipWith</td>
<td>
LHS:
<code>
map f (zip x y)
</code>
<br>
RHS:
<code>
zipWith (curry f) x y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use maybe</td>
<td>
LHS:
<code>
map f (fromMaybe [] x)
</code>
<br>
RHS:
<code>
maybe [] (map f) x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use notElem</td>
<td>
LHS:
<code>
not (elem x y)
</code>
<br>
RHS:
<code>
notElem x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Fuse foldr/map</td>
<td>
LHS:
<code>
foldr f z (map g x)
</code>
<br>
RHS:
<code>
foldr (f . g) z x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use unwords</td>
<td>
LHS:
<code>
x ++ concatMap (' ' :) y
</code>
<br>
RHS:
<code>
unwords (x : y)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use unwords</td>
<td>
LHS:
<code>
intercalate " "
</code>
<br>
RHS:
<code>
unwords
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use intercalate</td>
<td>
LHS:
<code>
concat (intersperse x y)
</code>
<br>
RHS:
<code>
intercalate x y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use unwords</td>
<td>
LHS:
<code>
concat (intersperse " " x)
</code>
<br>
RHS:
<code>
unwords x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use all</td>
<td>
LHS:
<code>
null (concat x)
</code>
<br>
RHS:
<code>
all null x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use any</td>
<td>
LHS:
<code>
null (filter f x)
</code>
<br>
RHS:
<code>
not (any f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use any</td>
<td>
LHS:
<code>
filter f x == []
</code>
<br>
RHS:
<code>
not (any f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use any</td>
<td>
LHS:
<code>
filter f x /= []
</code>
<br>
RHS:
<code>
any f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use or</td>
<td>
LHS:
<code>
any id
</code>
<br>
RHS:
<code>
or
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use and</td>
<td>
LHS:
<code>
all id
</code>
<br>
RHS:
<code>
and
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Hoist not</td>
<td>
LHS:
<code>
any (not . f) x
</code>
<br>
RHS:
<code>
not (all f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Hoist not</td>
<td>
LHS:
<code>
all (not . f) x
</code>
<br>
RHS:
<code>
not (any f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use elem</td>
<td>
LHS:
<code>
any ((==) a)
</code>
<br>
RHS:
<code>
elem a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use elem</td>
<td>
LHS:
<code>
any (== a)
</code>
<br>
RHS:
<code>
elem a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use elem</td>
<td>
LHS:
<code>
any (a ==)
</code>
<br>
RHS:
<code>
elem a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use notElem</td>
<td>
LHS:
<code>
all ((/=) a)
</code>
<br>
RHS:
<code>
notElem a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use notElem</td>
<td>
LHS:
<code>
all (/= a)
</code>
<br>
RHS:
<code>
notElem a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use notElem</td>
<td>
LHS:
<code>
all (a /=)
</code>
<br>
RHS:
<code>
notElem a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use or</td>
<td>
LHS:
<code>
elem True
</code>
<br>
RHS:
<code>
or
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use and</td>
<td>
LHS:
<code>
notElem False
</code>
<br>
RHS:
<code>
and
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use or</td>
<td>
LHS:
<code>
True `elem` l
</code>
<br>
RHS:
<code>
or l
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use and</td>
<td>
LHS:
<code>
False `notElem` l
</code>
<br>
RHS:
<code>
and l
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use elemIndex</td>
<td>
LHS:
<code>
findIndex ((==) a)
</code>
<br>
RHS:
<code>
elemIndex a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use elemIndex</td>
<td>
LHS:
<code>
findIndex (a ==)
</code>
<br>
RHS:
<code>
elemIndex a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use elemIndex</td>
<td>
LHS:
<code>
findIndex (== a)
</code>
<br>
RHS:
<code>
elemIndex a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use elemIndices</td>
<td>
LHS:
<code>
findIndices ((==) a)
</code>
<br>
RHS:
<code>
elemIndices a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use elemIndices</td>
<td>
LHS:
<code>
findIndices (a ==)
</code>
<br>
RHS:
<code>
elemIndices a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use elemIndices</td>
<td>
LHS:
<code>
findIndices (== a)
</code>
<br>
RHS:
<code>
elemIndices a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use elemIndex</td>
<td>
LHS:
<code>
lookup b (zip l [0 .. ])
</code>
<br>
RHS:
<code>
elemIndex b l
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use ==</td>
<td>
LHS:
<code>
elem x [y]
</code>
<br>
RHS:
<code>
x == y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use /=</td>
<td>
LHS:
<code>
notElem x [y]
</code>
<br>
RHS:
<code>
x /= y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use max</td>
<td>
LHS:
<code>
length [1 .. n]
</code>
<br>
RHS:
<code>
max 0 n
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Length always non-negative</td>
<td>
LHS:
<code>
length x >= 0
</code>
<br>
RHS:
<code>
True
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Length always non-negative</td>
<td>
LHS:
<code>
0 <= length x
</code>
<br>
RHS:
<code>
True
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
length x > 0
</code>
<br>
RHS:
<code>
not (null x)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
0 < length x
</code>
<br>
RHS:
<code>
not (null x)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
length x >= 1
</code>
<br>
RHS:
<code>
not (null x)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use null</td>
<td>
LHS:
<code>
1 <= length x
</code>
<br>
RHS:
<code>
not (null x)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Take on a non-positive</td>
<td>
LHS:
<code>
take i x
</code>
<br>
RHS:
<code>
[]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Drop on a non-positive</td>
<td>
LHS:
<code>
drop i x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use foldl</td>
<td>
LHS:
<code>
last (scanl f z x)
</code>
<br>
RHS:
<code>
foldl f z x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use foldr</td>
<td>
LHS:
<code>
head (scanr f z x)
</code>
<br>
RHS:
<code>
foldr f z x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use take</td>
<td>
LHS:
<code>
scanl (\ x _ -> a) b (replicate c d)
</code>
<br>
RHS:
<code>
take c (iterate (\ x -> a) b)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use iterate</td>
<td>
LHS:
<code>
foldl (\ x _ -> a) b [1 .. c]
</code>
<br>
RHS:
<code>
iterate (\ x -> a) b !! c
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use repeat</td>
<td>
LHS:
<code>
iterate id
</code>
<br>
RHS:
<code>
repeat
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use map</td>
<td>
LHS:
<code>
zipWith f (repeat x)
</code>
<br>
RHS:
<code>
map (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use map</td>
<td>
LHS:
<code>
zipWith f y (repeat z)
</code>
<br>
RHS:
<code>
map (`f` z) y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use find</td>
<td>
LHS:
<code>
listToMaybe (filter p x)
</code>
<br>
RHS:
<code>
find p x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant take</td>
<td>
LHS:
<code>
zip (take n x) (take n y)
</code>
<br>
RHS:
<code>
take n (zip x y)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant take</td>
<td>
LHS:
<code>
zip (take n x) (take m y)
</code>
<br>
RHS:
<code>
take (min n m) (zip x y)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Monoid law, left identity</td>
<td>
LHS:
<code>
mempty <> x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Monoid law, left identity</td>
<td>
LHS:
<code>
mempty `mappend` x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Monoid law, right identity</td>
<td>
LHS:
<code>
x <> mempty
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Monoid law, right identity</td>
<td>
LHS:
<code>
x `mappend` mempty
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fold</td>
<td>
LHS:
<code>
foldr (<>) mempty
</code>
<br>
RHS:
<code>
Data.Foldable.fold
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fold</td>
<td>
LHS:
<code>
foldr mappend mempty
</code>
<br>
RHS:
<code>
Data.Foldable.fold
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
mempty x
</code>
<br>
RHS:
<code>
mempty
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
x `mempty` y
</code>
<br>
RHS:
<code>
mempty
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Traversable law</td>
<td>
LHS:
<code>
traverse pure
</code>
<br>
RHS:
<code>
pure
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Traversable law</td>
<td>
LHS:
<code>
traverse (pure . f) x
</code>
<br>
RHS:
<code>
pure (fmap f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use traverse</td>
<td>
LHS:
<code>
sequenceA (map f x)
</code>
<br>
RHS:
<code>
traverse f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use traverse</td>
<td>
LHS:
<code>
sequenceA (f <$> x)
</code>
<br>
RHS:
<code>
traverse f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use traverse</td>
<td>
LHS:
<code>
sequenceA (fmap f x)
</code>
<br>
RHS:
<code>
traverse f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use traverse_</td>
<td>
LHS:
<code>
sequenceA_ (map f x)
</code>
<br>
RHS:
<code>
traverse_ f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use traverse_</td>
<td>
LHS:
<code>
sequenceA_ (f <$> x)
</code>
<br>
RHS:
<code>
traverse_ f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use traverse_</td>
<td>
LHS:
<code>
sequenceA_ (fmap f x)
</code>
<br>
RHS:
<code>
traverse_ f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fold</td>
<td>
LHS:
<code>
foldMap id
</code>
<br>
RHS:
<code>
fold
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use foldMap</td>
<td>
LHS:
<code>
fold (f <$> x)
</code>
<br>
RHS:
<code>
foldMap f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use foldMap</td>
<td>
LHS:
<code>
fold (fmap f x)
</code>
<br>
RHS:
<code>
foldMap f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use foldMap</td>
<td>
LHS:
<code>
fold (map f x)
</code>
<br>
RHS:
<code>
foldMap f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Fuse foldMap/fmap</td>
<td>
LHS:
<code>
foldMap f (g <$> x)
</code>
<br>
RHS:
<code>
foldMap (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Fuse foldMap/fmap</td>
<td>
LHS:
<code>
foldMap f (fmap g x)
</code>
<br>
RHS:
<code>
foldMap (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Fuse foldMap/map</td>
<td>
LHS:
<code>
foldMap f (map g x)
</code>
<br>
RHS:
<code>
foldMap (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use delete</td>
<td>
LHS:
<code>
deleteBy (==)
</code>
<br>
RHS:
<code>
delete
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use group</td>
<td>
LHS:
<code>
groupBy (==)
</code>
<br>
RHS:
<code>
group
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use insert</td>
<td>
LHS:
<code>
insertBy compare
</code>
<br>
RHS:
<code>
insert
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use intersect</td>
<td>
LHS:
<code>
intersectBy (==)
</code>
<br>
RHS:
<code>
intersect
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use maximum</td>
<td>
LHS:
<code>
maximumBy compare
</code>
<br>
RHS:
<code>
maximum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use minimum</td>
<td>
LHS:
<code>
minimumBy compare
</code>
<br>
RHS:
<code>
minimum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use nub</td>
<td>
LHS:
<code>
nubBy (==)
</code>
<br>
RHS:
<code>
nub
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sort</td>
<td>
LHS:
<code>
sortBy compare
</code>
<br>
RHS:
<code>
sort
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use union</td>
<td>
LHS:
<code>
unionBy (==)
</code>
<br>
RHS:
<code>
union
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sequence_</td>
<td>
LHS:
<code>
foldr (>>) (return ())
</code>
<br>
RHS:
<code>
sequence_
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use and</td>
<td>
LHS:
<code>
foldr (&&) True
</code>
<br>
RHS:
<code>
and
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use and</td>
<td>
LHS:
<code>
foldl (&&) True
</code>
<br>
RHS:
<code>
and
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use and</td>
<td>
LHS:
<code>
foldr1 (&&)
</code>
<br>
RHS:
<code>
and
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use and</td>
<td>
LHS:
<code>
foldl1 (&&)
</code>
<br>
RHS:
<code>
and
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use or</td>
<td>
LHS:
<code>
foldr (||) False
</code>
<br>
RHS:
<code>
or
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use or</td>
<td>
LHS:
<code>
foldl (||) False
</code>
<br>
RHS:
<code>
or
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use or</td>
<td>
LHS:
<code>
foldr1 (||)
</code>
<br>
RHS:
<code>
or
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use or</td>
<td>
LHS:
<code>
foldl1 (||)
</code>
<br>
RHS:
<code>
or
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sum</td>
<td>
LHS:
<code>
foldl (+) 0
</code>
<br>
RHS:
<code>
sum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sum</td>
<td>
LHS:
<code>
foldr (+) 0
</code>
<br>
RHS:
<code>
sum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sum</td>
<td>
LHS:
<code>
foldl1 (+)
</code>
<br>
RHS:
<code>
sum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sum</td>
<td>
LHS:
<code>
foldr1 (+)
</code>
<br>
RHS:
<code>
sum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use product</td>
<td>
LHS:
<code>
foldl (*) 1
</code>
<br>
RHS:
<code>
product
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use product</td>
<td>
LHS:
<code>
foldr (*) 1
</code>
<br>
RHS:
<code>
product
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use product</td>
<td>
LHS:
<code>
foldl1 (*)
</code>
<br>
RHS:
<code>
product
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use product</td>
<td>
LHS:
<code>
foldr1 (*)
</code>
<br>
RHS:
<code>
product
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use maximum</td>
<td>
LHS:
<code>
foldl1 max
</code>
<br>
RHS:
<code>
maximum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use maximum</td>
<td>
LHS:
<code>
foldr1 max
</code>
<br>
RHS:
<code>
maximum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use minimum</td>
<td>
LHS:
<code>
foldl1 min
</code>
<br>
RHS:
<code>
minimum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use minimum</td>
<td>
LHS:
<code>
foldr1 min
</code>
<br>
RHS:
<code>
minimum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use msum</td>
<td>
LHS:
<code>
foldr mplus mzero
</code>
<br>
RHS:
<code>
msum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use id</td>
<td>
LHS:
<code>
\ x -> x
</code>
<br>
RHS:
<code>
id
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use const</td>
<td>
LHS:
<code>
\ x y -> x
</code>
<br>
RHS:
<code>
const
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use const</td>
<td>
LHS:
<code>
curry fst
</code>
<br>
RHS:
<code>
const
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant curry</td>
<td>
LHS:
<code>
curry snd
</code>
<br>
RHS:
<code>
\ _ x -> x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant flip</td>
<td>
LHS:
<code>
flip const
</code>
<br>
RHS:
<code>
\ _ x -> x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use snd</td>
<td>
LHS:
<code>
\ (x, y) -> y
</code>
<br>
RHS:
<code>
snd
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fst</td>
<td>
LHS:
<code>
\ (x, y) -> x
</code>
<br>
RHS:
<code>
fst
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use curry</td>
<td>
LHS:
<code>
\ x y -> f (x, y)
</code>
<br>
RHS:
<code>
curry f
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use uncurry</td>
<td>
LHS:
<code>
\ (x, y) -> f x y
</code>
<br>
RHS:
<code>
uncurry f
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use uncurry</td>
<td>
LHS:
<code>
f (fst p) (snd p)
</code>
<br>
RHS:
<code>
uncurry f p
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant uncurry</td>
<td>
LHS:
<code>
uncurry (\ x y -> z)
</code>
<br>
RHS:
<code>
\ (x, y) -> z
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant curry</td>
<td>
LHS:
<code>
curry (\ (x, y) -> z)
</code>
<br>
RHS:
<code>
\ x y -> z
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant uncurry</td>
<td>
LHS:
<code>
uncurry (curry f)
</code>
<br>
RHS:
<code>
f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant curry</td>
<td>
LHS:
<code>
curry (uncurry f)
</code>
<br>
RHS:
<code>
f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant uncurry</td>
<td>
LHS:
<code>
uncurry f (a, b)
</code>
<br>
RHS:
<code>
f a b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant $</td>
<td>
LHS:
<code>
($) (f x)
</code>
<br>
RHS:
<code>
f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant $</td>
<td>
LHS:
<code>
(f $)
</code>
<br>
RHS:
<code>
f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant &</td>
<td>
LHS:
<code>
(& f)
</code>
<br>
RHS:
<code>
f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use const</td>
<td>
LHS:
<code>
\ x -> y
</code>
<br>
RHS:
<code>
const y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant flip</td>
<td>
LHS:
<code>
flip f x y
</code>
<br>
RHS:
<code>
f y x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant id</td>
<td>
LHS:
<code>
id x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant id</td>
<td>
LHS:
<code>
id . x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant id</td>
<td>
LHS:
<code>
x . id
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use tuple-section</td>
<td>
LHS:
<code>
((,) x)
</code>
<br>
RHS:
<code>
(_noParen_ x,)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use tuple-section</td>
<td>
LHS:
<code>
flip (,) x
</code>
<br>
RHS:
<code>
(, _noParen_ x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant flip</td>
<td>
LHS:
<code>
flip (flip f)
</code>
<br>
RHS:
<code>
f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant flip</td>
<td>
LHS:
<code>
flip f <*> g
</code>
<br>
RHS:
<code>
f =<< g
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant flip</td>
<td>
LHS:
<code>
g <**> flip f
</code>
<br>
RHS:
<code>
g >>= f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant flip</td>
<td>
LHS:
<code>
flip f =<< g
</code>
<br>
RHS:
<code>
f <*> g
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant flip</td>
<td>
LHS:
<code>
g >>= flip f
</code>
<br>
RHS:
<code>
g Control.Applicative.<**> f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isAsciiLower</td>
<td>
LHS:
<code>
a >= 'a' && a <= 'z'
</code>
<br>
RHS:
<code>
isAsciiLower a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isAsciiUpper</td>
<td>
LHS:
<code>
a >= 'A' && a <= 'Z'
</code>
<br>
RHS:
<code>
isAsciiUpper a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isDigit</td>
<td>
LHS:
<code>
a >= '0' && a <= '9'
</code>
<br>
RHS:
<code>
isDigit a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isOctDigit</td>
<td>
LHS:
<code>
a >= '0' && a <= '7'
</code>
<br>
RHS:
<code>
isOctDigit a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isAlpha</td>
<td>
LHS:
<code>
isLower a || isUpper a
</code>
<br>
RHS:
<code>
isAlpha a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isAlpha</td>
<td>
LHS:
<code>
isUpper a || isLower a
</code>
<br>
RHS:
<code>
isAlpha a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant ==</td>
<td>
LHS:
<code>
x == True
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant ==</td>
<td>
LHS:
<code>
x == False
</code>
<br>
RHS:
<code>
not x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant ==</td>
<td>
LHS:
<code>
True == a
</code>
<br>
RHS:
<code>
a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant ==</td>
<td>
LHS:
<code>
False == a
</code>
<br>
RHS:
<code>
not a
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant /=</td>
<td>
LHS:
<code>
a /= True
</code>
<br>
RHS:
<code>
not a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant /=</td>
<td>
LHS:
<code>
a /= False
</code>
<br>
RHS:
<code>
a
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant /=</td>
<td>
LHS:
<code>
True /= a
</code>
<br>
RHS:
<code>
not a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant /=</td>
<td>
LHS:
<code>
False /= a
</code>
<br>
RHS:
<code>
a
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if a then x else x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if a then True else False
</code>
<br>
RHS:
<code>
a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if a then False else True
</code>
<br>
RHS:
<code>
not a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if a then t else (if b then t else f)
</code>
<br>
RHS:
<code>
if a || b then t else f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if a then (if b then t else f) else f
</code>
<br>
RHS:
<code>
if a && b then t else f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if x then True else y
</code>
<br>
RHS:
<code>
x || y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if x then y else False
</code>
<br>
RHS:
<code>
x && y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant multi-way if</td>
<td>
LHS:
<pre>
if | b -> t
   | otherwise -> f
</pre>
RHS:
<code>
if b then t else f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use if</td>
<td>
LHS:
<pre>
case a of
  True -> t
  False -> f
</pre>
RHS:
<code>
if a then t else f
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use if</td>
<td>
LHS:
<pre>
case a of
  False -> f
  True -> t
</pre>
RHS:
<code>
if a then t else f
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use if</td>
<td>
LHS:
<pre>
case a of
  True -> t
  _ -> f
</pre>
RHS:
<code>
if a then t else f
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use if</td>
<td>
LHS:
<pre>
case a of
  False -> f
  _ -> t
</pre>
RHS:
<code>
if a then t else f
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if c then (True, x) else (False, x)
</code>
<br>
RHS:
<code>
(c, x)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if c then (False, x) else (True, x)
</code>
<br>
RHS:
<code>
(not c, x)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use ||</td>
<td>
LHS:
<code>
or [x, y]
</code>
<br>
RHS:
<code>
x || y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use ||</td>
<td>
LHS:
<code>
or [x, y, z]
</code>
<br>
RHS:
<code>
x || y || z
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use &&</td>
<td>
LHS:
<code>
and [x, y]
</code>
<br>
RHS:
<code>
x && y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use &&</td>
<td>
LHS:
<code>
and [x, y, z]
</code>
<br>
RHS:
<code>
x && y && z
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if x then False else y
</code>
<br>
RHS:
<code>
not x && y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant if</td>
<td>
LHS:
<code>
if x then y else True
</code>
<br>
RHS:
<code>
not x || y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant not</td>
<td>
LHS:
<code>
not (not x)
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use second</td>
<td>
LHS:
<code>
id *** g
</code>
<br>
RHS:
<code>
second g
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use first</td>
<td>
LHS:
<code>
f *** id
</code>
<br>
RHS:
<code>
first f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use &&&</td>
<td>
LHS:
<code>
zip (map f x) (map g x)
</code>
<br>
RHS:
<code>
map (f Control.Arrow.&&& g) x
</code>
<br>
</td>
<td>Ignore</td>
</tr>
<tr>
<td>Use &&&</td>
<td>
LHS:
<code>
\ x -> (f x, g x)
</code>
<br>
RHS:
<code>
f Control.Arrow.&&& g
</code>
<br>
</td>
<td>Ignore</td>
</tr>
<tr>
<td>Redundant pair</td>
<td>
LHS:
<code>
(fst x, snd x)
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use second</td>
<td>
LHS:
<code>
bimap id g
</code>
<br>
RHS:
<code>
second g
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use first</td>
<td>
LHS:
<code>
bimap f id
</code>
<br>
RHS:
<code>
first f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant first</td>
<td>
LHS:
<code>
first id
</code>
<br>
RHS:
<code>
id
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant second</td>
<td>
LHS:
<code>
second id
</code>
<br>
RHS:
<code>
id
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant bimap</td>
<td>
LHS:
<code>
bimap id id
</code>
<br>
RHS:
<code>
id
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use bimap</td>
<td>
LHS:
<code>
first f (second g x)
</code>
<br>
RHS:
<code>
bimap f g x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use bimap</td>
<td>
LHS:
<code>
second g (first f x)
</code>
<br>
RHS:
<code>
bimap f g x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant first</td>
<td>
LHS:
<code>
first f (first g x)
</code>
<br>
RHS:
<code>
first (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant second</td>
<td>
LHS:
<code>
second f (second g x)
</code>
<br>
RHS:
<code>
second (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant bimap</td>
<td>
LHS:
<code>
bimap f h (bimap g i x)
</code>
<br>
RHS:
<code>
bimap (f . g) (h . i) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant first</td>
<td>
LHS:
<code>
first f (bimap g h x)
</code>
<br>
RHS:
<code>
bimap (f . g) h x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant second</td>
<td>
LHS:
<code>
second g (bimap f h x)
</code>
<br>
RHS:
<code>
bimap f (g . h) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant first</td>
<td>
LHS:
<code>
bimap f h (first g x)
</code>
<br>
RHS:
<code>
bimap (f . g) h x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant second</td>
<td>
LHS:
<code>
bimap f g (second h x)
</code>
<br>
RHS:
<code>
bimap f (g . h) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use bimap</td>
<td>
LHS:
<code>
\ (x, y) -> (f x, g y)
</code>
<br>
RHS:
<code>
Data.Bifunctor.bimap f g
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use first</td>
<td>
LHS:
<code>
\ (x, y) -> (f x, y)
</code>
<br>
RHS:
<code>
Data.Bifunctor.first f
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use second</td>
<td>
LHS:
<code>
\ (x, y) -> (x, f y)
</code>
<br>
RHS:
<code>
Data.Bifunctor.second f
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use bimap</td>
<td>
LHS:
<code>
(f (fst x), g (snd x))
</code>
<br>
RHS:
<code>
Data.Bifunctor.bimap f g x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use first</td>
<td>
LHS:
<code>
(f (fst x), snd x)
</code>
<br>
RHS:
<code>
Data.Bifunctor.first f x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use second</td>
<td>
LHS:
<code>
(fst x, g (snd x))
</code>
<br>
RHS:
<code>
Data.Bifunctor.second g x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Functor law</td>
<td>
LHS:
<code>
fmap f (fmap g x)
</code>
<br>
RHS:
<code>
fmap (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Functor law</td>
<td>
LHS:
<code>
f <$> g <$> x
</code>
<br>
RHS:
<code>
f . g <$> x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Functor law</td>
<td>
LHS:
<code>
fmap id
</code>
<br>
RHS:
<code>
id
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Functor law</td>
<td>
LHS:
<code>
id <$> x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <$></td>
<td>
LHS:
<code>
fmap f $ x
</code>
<br>
RHS:
<code>
f <$> x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use fmap</td>
<td>
LHS:
<code>
\ x -> a <$> b x
</code>
<br>
RHS:
<code>
fmap a . b
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use $></td>
<td>
LHS:
<code>
x *> pure y
</code>
<br>
RHS:
<code>
x Data.Functor.$> y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use $></td>
<td>
LHS:
<code>
x *> return y
</code>
<br>
RHS:
<code>
x Data.Functor.$> y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <$</td>
<td>
LHS:
<code>
pure x <* y
</code>
<br>
RHS:
<code>
x Data.Functor.<$ y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <$</td>
<td>
LHS:
<code>
return x <* y
</code>
<br>
RHS:
<code>
x Data.Functor.<$ y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <$</td>
<td>
LHS:
<code>
const x <$> y
</code>
<br>
RHS:
<code>
x <$ y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <$</td>
<td>
LHS:
<code>
pure x <$> y
</code>
<br>
RHS:
<code>
x <$ y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <$</td>
<td>
LHS:
<code>
return x <$> y
</code>
<br>
RHS:
<code>
x <$ y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use $></td>
<td>
LHS:
<code>
x <&> const y
</code>
<br>
RHS:
<code>
x Data.Functor.$> y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use $></td>
<td>
LHS:
<code>
x <&> pure y
</code>
<br>
RHS:
<code>
x Data.Functor.$> y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use $></td>
<td>
LHS:
<code>
x <&> return y
</code>
<br>
RHS:
<code>
x Data.Functor.$> y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <$></td>
<td>
LHS:
<code>
return x <*> y
</code>
<br>
RHS:
<code>
x <$> y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <$></td>
<td>
LHS:
<code>
pure x <*> y
</code>
<br>
RHS:
<code>
x <$> y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant <*</td>
<td>
LHS:
<code>
x <* pure y
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant pure</td>
<td>
LHS:
<code>
pure x *> y
</code>
<br>
RHS:
<code>
y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Monad law, left identity</td>
<td>
LHS:
<code>
return a >>= f
</code>
<br>
RHS:
<code>
f a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Monad law, left identity</td>
<td>
LHS:
<code>
f =<< return a
</code>
<br>
RHS:
<code>
f a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Monad law, right identity</td>
<td>
LHS:
<code>
m >>= return
</code>
<br>
RHS:
<code>
m
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Monad law, right identity</td>
<td>
LHS:
<code>
return =<< m
</code>
<br>
RHS:
<code>
m
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fmap</td>
<td>
LHS:
<code>
liftM
</code>
<br>
RHS:
<code>
fmap
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fmap</td>
<td>
LHS:
<code>
liftA
</code>
<br>
RHS:
<code>
fmap
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <&></td>
<td>
LHS:
<code>
m >>= return . f
</code>
<br>
RHS:
<code>
m Data.Functor.<&> f
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <$></td>
<td>
LHS:
<code>
return . f =<< m
</code>
<br>
RHS:
<code>
f <$> m
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
fmap f x >>= g
</code>
<br>
RHS:
<code>
x >>= g . f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <$></td>
<td>
LHS:
<code>
f <$> x >>= g
</code>
<br>
RHS:
<code>
x >>= g . f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <&></td>
<td>
LHS:
<code>
x Data.Functor.<&> f >>= g
</code>
<br>
RHS:
<code>
x >>= g . f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
g =<< fmap f x
</code>
<br>
RHS:
<code>
g . f =<< x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <$></td>
<td>
LHS:
<code>
g =<< f <$> x
</code>
<br>
RHS:
<code>
g . f =<< x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <&></td>
<td>
LHS:
<code>
g =<< (x Data.Functor.<&> f)
</code>
<br>
RHS:
<code>
g . f =<< x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use when</td>
<td>
LHS:
<code>
if x then y else return ()
</code>
<br>
RHS:
<code>
Control.Monad.when x $ _noParen_ y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use when</td>
<td>
LHS:
<code>
if x then y else return ()
</code>
<br>
RHS:
<code>
Control.Monad.when x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use unless</td>
<td>
LHS:
<code>
if x then return () else y
</code>
<br>
RHS:
<code>
Control.Monad.unless x $ _noParen_ y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use unless</td>
<td>
LHS:
<code>
if x then return () else y
</code>
<br>
RHS:
<code>
Control.Monad.unless x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapM</td>
<td>
LHS:
<code>
sequence (map f x)
</code>
<br>
RHS:
<code>
mapM f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapM_</td>
<td>
LHS:
<code>
sequence_ (map f x)
</code>
<br>
RHS:
<code>
mapM_ f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapM</td>
<td>
LHS:
<code>
sequence (f <$> x)
</code>
<br>
RHS:
<code>
mapM f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapM</td>
<td>
LHS:
<code>
sequence (fmap f x)
</code>
<br>
RHS:
<code>
mapM f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapM_</td>
<td>
LHS:
<code>
sequence_ (f <$> x)
</code>
<br>
RHS:
<code>
mapM_ f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapM_</td>
<td>
LHS:
<code>
sequence_ (fmap f x)
</code>
<br>
RHS:
<code>
mapM_ f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use forM</td>
<td>
LHS:
<code>
flip mapM
</code>
<br>
RHS:
<code>
Control.Monad.forM
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use forM_</td>
<td>
LHS:
<code>
flip mapM_
</code>
<br>
RHS:
<code>
Control.Monad.forM_
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use mapM</td>
<td>
LHS:
<code>
flip forM
</code>
<br>
RHS:
<code>
mapM
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use mapM_</td>
<td>
LHS:
<code>
flip forM_
</code>
<br>
RHS:
<code>
mapM_
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use unless</td>
<td>
LHS:
<code>
when (not x)
</code>
<br>
RHS:
<code>
unless x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use when</td>
<td>
LHS:
<code>
unless (not x)
</code>
<br>
RHS:
<code>
when x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use join</td>
<td>
LHS:
<code>
x >>= id
</code>
<br>
RHS:
<code>
Control.Monad.join x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use join</td>
<td>
LHS:
<code>
id =<< x
</code>
<br>
RHS:
<code>
Control.Monad.join x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use join</td>
<td>
LHS:
<code>
id =<< x
</code>
<br>
RHS:
<code>
Control.Monad.join x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use join</td>
<td>
LHS:
<code>
id =<< x
</code>
<br>
RHS:
<code>
Control.Monad.join x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use =<<</td>
<td>
LHS:
<code>
join (f <$> x)
</code>
<br>
RHS:
<code>
f =<< x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use =<<</td>
<td>
LHS:
<code>
join (fmap f x)
</code>
<br>
RHS:
<code>
f =<< x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use void</td>
<td>
LHS:
<code>
a >> return ()
</code>
<br>
RHS:
<code>
Control.Monad.void a
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use void</td>
<td>
LHS:
<code>
fmap (const ())
</code>
<br>
RHS:
<code>
Control.Monad.void
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use void</td>
<td>
LHS:
<code>
const () <$> x
</code>
<br>
RHS:
<code>
Control.Monad.void x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use void</td>
<td>
LHS:
<code>
() <$ x
</code>
<br>
RHS:
<code>
Control.Monad.void x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <=<</td>
<td>
LHS:
<code>
flip (>=>)
</code>
<br>
RHS:
<code>
(<=<)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use >=></td>
<td>
LHS:
<code>
flip (<=<)
</code>
<br>
RHS:
<code>
(>=>)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use =<<</td>
<td>
LHS:
<code>
flip (>>=)
</code>
<br>
RHS:
<code>
(=<<)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use >>=</td>
<td>
LHS:
<code>
flip (=<<)
</code>
<br>
RHS:
<code>
(>>=)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use >=></td>
<td>
LHS:
<code>
\ x -> f x >>= g
</code>
<br>
RHS:
<code>
f Control.Monad.>=> g
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <=<</td>
<td>
LHS:
<code>
\ x -> f =<< g x
</code>
<br>
RHS:
<code>
f Control.Monad.<=< g
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <=<</td>
<td>
LHS:
<code>
(>>= f) . g
</code>
<br>
RHS:
<code>
f Control.Monad.<=< g
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <=<</td>
<td>
LHS:
<code>
(f =<<) . g
</code>
<br>
RHS:
<code>
f Control.Monad.<=< g
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant >></td>
<td>
LHS:
<code>
a >> forever a
</code>
<br>
RHS:
<code>
forever a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use ap</td>
<td>
LHS:
<code>
liftM2 id
</code>
<br>
RHS:
<code>
ap
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use fmap</td>
<td>
LHS:
<code>
liftA2 f (return x)
</code>
<br>
RHS:
<code>
fmap (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fmap</td>
<td>
LHS:
<code>
liftM2 f (pure x)
</code>
<br>
RHS:
<code>
fmap (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fmap</td>
<td>
LHS:
<code>
liftM2 f (return x)
</code>
<br>
RHS:
<code>
fmap (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
fmap f (return x)
</code>
<br>
RHS:
<code>
return (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <$></td>
<td>
LHS:
<code>
f <$> return x
</code>
<br>
RHS:
<code>
return (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use zipWithM</td>
<td>
LHS:
<code>
mapM (uncurry f) (zip l m)
</code>
<br>
RHS:
<code>
zipWithM f l m
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant void</td>
<td>
LHS:
<code>
mapM_ (void . f)
</code>
<br>
RHS:
<code>
mapM_ f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant void</td>
<td>
LHS:
<code>
forM_ x (void . f)
</code>
<br>
RHS:
<code>
forM_ x f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use >></td>
<td>
LHS:
<code>
a >>= \ _ -> b
</code>
<br>
RHS:
<code>
a >> b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <*</td>
<td>
LHS:
<code>
m <* return x
</code>
<br>
RHS:
<code>
m
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant return</td>
<td>
LHS:
<code>
return x *> m
</code>
<br>
RHS:
<code>
m
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant pure</td>
<td>
LHS:
<code>
pure x >> m
</code>
<br>
RHS:
<code>
m
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant return</td>
<td>
LHS:
<code>
return x >> m
</code>
<br>
RHS:
<code>
m
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use evalState</td>
<td>
LHS:
<code>
fst (runState x y)
</code>
<br>
RHS:
<code>
evalState x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use execState</td>
<td>
LHS:
<code>
snd (runState x y)
</code>
<br>
RHS:
<code>
execState x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapAndUnzipM</td>
<td>
LHS:
<code>
unzip <$> mapM f x
</code>
<br>
RHS:
<code>
Control.Monad.mapAndUnzipM f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapAndUnzipM</td>
<td>
LHS:
<code>
fmap unzip (mapM f x)
</code>
<br>
RHS:
<code>
Control.Monad.mapAndUnzipM f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use zipWithM</td>
<td>
LHS:
<code>
sequence (zipWith f x y)
</code>
<br>
RHS:
<code>
Control.Monad.zipWithM f x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use zipWithM_</td>
<td>
LHS:
<code>
sequence_ (zipWith f x y)
</code>
<br>
RHS:
<code>
Control.Monad.zipWithM_ f x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use replicateM</td>
<td>
LHS:
<code>
sequence (replicate n x)
</code>
<br>
RHS:
<code>
Control.Monad.replicateM n x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use replicateM_</td>
<td>
LHS:
<code>
sequence_ (replicate n x)
</code>
<br>
RHS:
<code>
Control.Monad.replicateM_ n x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use zipWithM</td>
<td>
LHS:
<code>
sequenceA (zipWith f x y)
</code>
<br>
RHS:
<code>
Control.Monad.zipWithM f x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use zipWithM_</td>
<td>
LHS:
<code>
sequenceA_ (zipWith f x y)
</code>
<br>
RHS:
<code>
Control.Monad.zipWithM_ f x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use replicateM</td>
<td>
LHS:
<code>
sequenceA (replicate n x)
</code>
<br>
RHS:
<code>
Control.Monad.replicateM n x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use replicateM_</td>
<td>
LHS:
<code>
sequenceA_ (replicate n x)
</code>
<br>
RHS:
<code>
Control.Monad.replicateM_ n x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use replicateM</td>
<td>
LHS:
<code>
mapM f (replicate n x)
</code>
<br>
RHS:
<code>
Control.Monad.replicateM n (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use replicateM_</td>
<td>
LHS:
<code>
mapM_ f (replicate n x)
</code>
<br>
RHS:
<code>
Control.Monad.replicateM_ n (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Fuse mapM/map</td>
<td>
LHS:
<code>
mapM f (map g x)
</code>
<br>
RHS:
<code>
mapM (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Fuse mapM_/map</td>
<td>
LHS:
<code>
mapM_ f (map g x)
</code>
<br>
RHS:
<code>
mapM_ (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Fuse traverse/map</td>
<td>
LHS:
<code>
traverse f (map g x)
</code>
<br>
RHS:
<code>
traverse (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Fuse traverse_/map</td>
<td>
LHS:
<code>
traverse_ f (map g x)
</code>
<br>
RHS:
<code>
traverse_ (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sequence</td>
<td>
LHS:
<code>
mapM id
</code>
<br>
RHS:
<code>
sequence
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sequence_</td>
<td>
LHS:
<code>
mapM_ id
</code>
<br>
RHS:
<code>
sequence_
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use for</td>
<td>
LHS:
<code>
flip traverse
</code>
<br>
RHS:
<code>
for
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use traverse</td>
<td>
LHS:
<code>
flip for
</code>
<br>
RHS:
<code>
traverse
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use for_</td>
<td>
LHS:
<code>
flip traverse_
</code>
<br>
RHS:
<code>
for_
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use traverse_</td>
<td>
LHS:
<code>
flip for_
</code>
<br>
RHS:
<code>
traverse_
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sequenceA_</td>
<td>
LHS:
<code>
foldr (*>) (pure ())
</code>
<br>
RHS:
<code>
sequenceA_
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use asum</td>
<td>
LHS:
<code>
foldr (<|>) empty
</code>
<br>
RHS:
<code>
asum
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <**></td>
<td>
LHS:
<code>
liftA2 (flip ($))
</code>
<br>
RHS:
<code>
(<**>)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fmap</td>
<td>
LHS:
<code>
liftA2 f (pure x)
</code>
<br>
RHS:
<code>
fmap (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
fmap f (pure x)
</code>
<br>
RHS:
<code>
pure (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <$></td>
<td>
LHS:
<code>
f <$> pure x
</code>
<br>
RHS:
<code>
pure (f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use optional</td>
<td>
LHS:
<code>
Just <$> a <|> pure Nothing
</code>
<br>
RHS:
<code>
optional a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <&></td>
<td>
LHS:
<code>
m >>= pure . f
</code>
<br>
RHS:
<code>
m Data.Functor.<&> f
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use <$></td>
<td>
LHS:
<code>
pure . f =<< m
</code>
<br>
RHS:
<code>
f <$> m
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Alternative law, left identity</td>
<td>
LHS:
<code>
empty <|> x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Alternative law, right identity</td>
<td>
LHS:
<code>
x <|> empty
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sequenceA</td>
<td>
LHS:
<code>
traverse id
</code>
<br>
RHS:
<code>
sequenceA
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use sequenceA_</td>
<td>
LHS:
<code>
traverse_ id
</code>
<br>
RHS:
<code>
sequenceA_
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use list comprehension</td>
<td>
LHS:
<code>
if b then [x] else []
</code>
<br>
RHS:
<code>
[x | b]
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use list comprehension</td>
<td>
LHS:
<code>
if b then [] else [x]
</code>
<br>
RHS:
<code>
[x | not b]
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant list comprehension</td>
<td>
LHS:
<code>
[x | x <- y]
</code>
<br>
RHS:
<code>
y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant seq</td>
<td>
LHS:
<code>
seq x x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant seq</td>
<td>
LHS:
<code>
join seq
</code>
<br>
RHS:
<code>
id
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant $!</td>
<td>
LHS:
<code>
id $! x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant seq</td>
<td>
LHS:
<code>
seq x y
</code>
<br>
RHS:
<code>
y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant $!</td>
<td>
LHS:
<code>
f $! x
</code>
<br>
RHS:
<code>
f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant evaluate</td>
<td>
LHS:
<code>
evaluate x
</code>
<br>
RHS:
<code>
return x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant seq</td>
<td>
LHS:
<code>
seq (rnf x) ()
</code>
<br>
RHS:
<code>
rnf x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use map</td>
<td>
LHS:
<code>
fst (unzip x)
</code>
<br>
RHS:
<code>
map fst x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use map</td>
<td>
LHS:
<code>
snd (unzip x)
</code>
<br>
RHS:
<code>
map snd x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use (,)</td>
<td>
LHS:
<code>
\ x y -> (x, y)
</code>
<br>
RHS:
<code>
(,)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use (,,)</td>
<td>
LHS:
<code>
\ x y z -> (x, y, z)
</code>
<br>
RHS:
<code>
(,,)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
(, b) a
</code>
<br>
RHS:
<code>
(a, b)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
(a,) b
</code>
<br>
RHS:
<code>
(a, b)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use fromMaybe</td>
<td>
LHS:
<code>
maybe x id
</code>
<br>
RHS:
<code>
Data.Maybe.fromMaybe x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant maybe</td>
<td>
LHS:
<code>
maybe Nothing Just
</code>
<br>
RHS:
<code>
id
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isJust</td>
<td>
LHS:
<code>
maybe False (const True)
</code>
<br>
RHS:
<code>
Data.Maybe.isJust
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isNothing</td>
<td>
LHS:
<code>
maybe True (const False)
</code>
<br>
RHS:
<code>
Data.Maybe.isNothing
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use Just</td>
<td>
LHS:
<code>
maybe False (x ==)
</code>
<br>
RHS:
<code>
(Just x ==)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use Just</td>
<td>
LHS:
<code>
maybe True (x /=)
</code>
<br>
RHS:
<code>
(Just x /=)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use Just</td>
<td>
LHS:
<code>
maybe False (== x)
</code>
<br>
RHS:
<code>
(Just x ==)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use Just</td>
<td>
LHS:
<code>
maybe True (/= x)
</code>
<br>
RHS:
<code>
(Just x /=)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use Just</td>
<td>
LHS:
<code>
fromMaybe False x
</code>
<br>
RHS:
<code>
Just True == x
</code>
<br>
</td>
<td>Ignore</td>
</tr>
<tr>
<td>Use Just</td>
<td>
LHS:
<code>
fromMaybe True x
</code>
<br>
RHS:
<code>
Just False /= x
</code>
<br>
</td>
<td>Ignore</td>
</tr>
<tr>
<td>Use isJust</td>
<td>
LHS:
<code>
not (isNothing x)
</code>
<br>
RHS:
<code>
isJust x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isNothing</td>
<td>
LHS:
<code>
not (isJust x)
</code>
<br>
RHS:
<code>
isNothing x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use maybeToList</td>
<td>
LHS:
<code>
maybe [] (: [])
</code>
<br>
RHS:
<code>
maybeToList
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapMaybe</td>
<td>
LHS:
<code>
catMaybes (map f x)
</code>
<br>
RHS:
<code>
mapMaybe f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapMaybe</td>
<td>
LHS:
<code>
catMaybes (f <$> x)
</code>
<br>
RHS:
<code>
mapMaybe f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapMaybe</td>
<td>
LHS:
<code>
catMaybes (fmap f x)
</code>
<br>
RHS:
<code>
mapMaybe f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Replace case with fromMaybe</td>
<td>
LHS:
<pre>
case x of
  Nothing -> y
  Just a -> a
</pre>
RHS:
<code>
Data.Maybe.fromMaybe y x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Replace case with fromMaybe</td>
<td>
LHS:
<pre>
case x of
  Just a -> a
  Nothing -> y
</pre>
RHS:
<code>
Data.Maybe.fromMaybe y x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Replace case with maybe</td>
<td>
LHS:
<pre>
case x of
  Nothing -> y
  Just a -> f a
</pre>
RHS:
<code>
maybe y f x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Replace case with maybe</td>
<td>
LHS:
<pre>
case x of
  Just a -> f a
  Nothing -> y
</pre>
RHS:
<code>
maybe y f x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use maybe</td>
<td>
LHS:
<code>
if isNothing x then y else f (fromJust x)
</code>
<br>
RHS:
<code>
maybe y f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use maybe</td>
<td>
LHS:
<code>
if isJust x then f (fromJust x) else y
</code>
<br>
RHS:
<code>
maybe y f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fmap</td>
<td>
LHS:
<code>
maybe Nothing (Just . f)
</code>
<br>
RHS:
<code>
fmap f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use catMaybes</td>
<td>
LHS:
<code>
map fromJust (filter isJust x)
</code>
<br>
RHS:
<code>
Data.Maybe.catMaybes x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use isNothing</td>
<td>
LHS:
<code>
x == Nothing
</code>
<br>
RHS:
<code>
isNothing x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isNothing</td>
<td>
LHS:
<code>
Nothing == x
</code>
<br>
RHS:
<code>
isNothing x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isJust</td>
<td>
LHS:
<code>
x /= Nothing
</code>
<br>
RHS:
<code>
Data.Maybe.isJust x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use isJust</td>
<td>
LHS:
<code>
Nothing /= x
</code>
<br>
RHS:
<code>
Data.Maybe.isJust x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mapMaybe</td>
<td>
LHS:
<code>
concatMap (maybeToList . f)
</code>
<br>
RHS:
<code>
Data.Maybe.mapMaybe f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use catMaybes</td>
<td>
LHS:
<code>
concatMap maybeToList
</code>
<br>
RHS:
<code>
catMaybes
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <|></td>
<td>
LHS:
<code>
maybe n Just x
</code>
<br>
RHS:
<code>
x Control.Applicative.<|> n
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fromMaybe</td>
<td>
LHS:
<code>
if isNothing x then y else fromJust x
</code>
<br>
RHS:
<code>
fromMaybe y x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fromMaybe</td>
<td>
LHS:
<code>
if isJust x then fromJust x else y
</code>
<br>
RHS:
<code>
fromMaybe y x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use Just</td>
<td>
LHS:
<code>
isJust x && (fromJust x == y)
</code>
<br>
RHS:
<code>
x == Just y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Fuse mapMaybe/map</td>
<td>
LHS:
<code>
mapMaybe f (map g x)
</code>
<br>
RHS:
<code>
mapMaybe (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use maybe</td>
<td>
LHS:
<code>
fromMaybe a (fmap f x)
</code>
<br>
RHS:
<code>
maybe a f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use maybe</td>
<td>
LHS:
<code>
fromMaybe a (f <$> x)
</code>
<br>
RHS:
<code>
maybe a f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use catMaybes</td>
<td>
LHS:
<code>
mapMaybe id
</code>
<br>
RHS:
<code>
catMaybes
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use catMaybes</td>
<td>
LHS:
<code>
[x | Just x <- a]
</code>
<br>
RHS:
<code>
Data.Maybe.catMaybes a
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use join</td>
<td>
LHS:
<pre>
case m of
  Nothing -> Nothing
  Just x -> x
</pre>
RHS:
<code>
Control.Monad.join m
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use join</td>
<td>
LHS:
<code>
maybe Nothing id
</code>
<br>
RHS:
<code>
join
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use =<<</td>
<td>
LHS:
<code>
maybe Nothing f x
</code>
<br>
RHS:
<code>
f =<< x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
maybe x f (g <$> y)
</code>
<br>
RHS:
<code>
maybe x (f . g) y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
maybe x f (fmap g y)
</code>
<br>
RHS:
<code>
maybe x (f . g) y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <$></td>
<td>
LHS:
<code>
isJust (f <$> x)
</code>
<br>
RHS:
<code>
isJust x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
isJust (fmap f x)
</code>
<br>
RHS:
<code>
isJust x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <$></td>
<td>
LHS:
<code>
isNothing (f <$> x)
</code>
<br>
RHS:
<code>
isNothing x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
isNothing (fmap f x)
</code>
<br>
RHS:
<code>
isNothing x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <$></td>
<td>
LHS:
<code>
fromJust (f <$> x)
</code>
<br>
RHS:
<code>
f (fromJust x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
fromJust (fmap f x)
</code>
<br>
RHS:
<code>
f (fromJust x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
mapMaybe f (g <$> x)
</code>
<br>
RHS:
<code>
mapMaybe (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
mapMaybe f (fmap g x)
</code>
<br>
RHS:
<code>
mapMaybe (f . g) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move nub out</td>
<td>
LHS:
<code>
catMaybes (nub x)
</code>
<br>
RHS:
<code>
nub (catMaybes x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move nub out</td>
<td>
LHS:
<code>
lefts (nub x)
</code>
<br>
RHS:
<code>
nub (lefts x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move nub out</td>
<td>
LHS:
<code>
rights (nub x)
</code>
<br>
RHS:
<code>
nub (rights x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move reverse out</td>
<td>
LHS:
<code>
catMaybes (reverse x)
</code>
<br>
RHS:
<code>
reverse (catMaybes x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move reverse out</td>
<td>
LHS:
<code>
lefts (reverse x)
</code>
<br>
RHS:
<code>
reverse (lefts x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move reverse out</td>
<td>
LHS:
<code>
rights (reverse x)
</code>
<br>
RHS:
<code>
reverse (rights x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move sort out</td>
<td>
LHS:
<code>
catMaybes (sort x)
</code>
<br>
RHS:
<code>
sort (catMaybes x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move sort out</td>
<td>
LHS:
<code>
lefts (sort x)
</code>
<br>
RHS:
<code>
sort (lefts x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move sort out</td>
<td>
LHS:
<code>
rights (sort x)
</code>
<br>
RHS:
<code>
sort (rights x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move nubOrd out</td>
<td>
LHS:
<code>
catMaybes (nubOrd x)
</code>
<br>
RHS:
<code>
nubOrd (catMaybes x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move nubOrd out</td>
<td>
LHS:
<code>
lefts (nubOrd x)
</code>
<br>
RHS:
<code>
nubOrd (lefts x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move nubOrd out</td>
<td>
LHS:
<code>
rights (nubOrd x)
</code>
<br>
RHS:
<code>
nubOrd (rights x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Move reverse out</td>
<td>
LHS:
<code>
filter f (reverse x)
</code>
<br>
RHS:
<code>
reverse (filter f x)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use lefts</td>
<td>
LHS:
<code>
[a | Left a <- a]
</code>
<br>
RHS:
<code>
lefts a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use rights</td>
<td>
LHS:
<code>
[a | Right a <- a]
</code>
<br>
RHS:
<code>
rights a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fmap</td>
<td>
LHS:
<code>
either Left (Right . f)
</code>
<br>
RHS:
<code>
fmap f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
either f g (fmap h x)
</code>
<br>
RHS:
<code>
either f (g . h) x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
isLeft (fmap f x)
</code>
<br>
RHS:
<code>
isLeft x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
isRight (fmap f x)
</code>
<br>
RHS:
<code>
isRight x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
fromLeft x (fmap f y)
</code>
<br>
RHS:
<code>
fromLeft x y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use either</td>
<td>
LHS:
<code>
fromRight x (fmap f y)
</code>
<br>
RHS:
<code>
either (const x) f y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fromRight</td>
<td>
LHS:
<code>
either (const x) id
</code>
<br>
RHS:
<code>
fromRight x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use fromLeft</td>
<td>
LHS:
<code>
either id (const x)
</code>
<br>
RHS:
<code>
fromLeft x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use =<<</td>
<td>
LHS:
<code>
either Left f x
</code>
<br>
RHS:
<code>
f =<< x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use infix</td>
<td>
LHS:
<code>
elem x y
</code>
<br>
RHS:
<code>
x `elem` y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use infix</td>
<td>
LHS:
<code>
notElem x y
</code>
<br>
RHS:
<code>
x `notElem` y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use infix</td>
<td>
LHS:
<code>
isInfixOf x y
</code>
<br>
RHS:
<code>
x `isInfixOf` y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use infix</td>
<td>
LHS:
<code>
isSuffixOf x y
</code>
<br>
RHS:
<code>
x `isSuffixOf` y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use infix</td>
<td>
LHS:
<code>
isPrefixOf x y
</code>
<br>
RHS:
<code>
x `isPrefixOf` y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use infix</td>
<td>
LHS:
<code>
union x y
</code>
<br>
RHS:
<code>
x `union` y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use infix</td>
<td>
LHS:
<code>
intersect x y
</code>
<br>
RHS:
<code>
x `intersect` y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant fromIntegral</td>
<td>
LHS:
<code>
fromIntegral x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fromInteger</td>
<td>
LHS:
<code>
fromInteger x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use -</td>
<td>
LHS:
<code>
x + negate y
</code>
<br>
RHS:
<code>
x - y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use negate</td>
<td>
LHS:
<code>
0 - x
</code>
<br>
RHS:
<code>
negate x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant negate</td>
<td>
LHS:
<code>
negate (negate x)
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use logBase</td>
<td>
LHS:
<code>
log y / log x
</code>
<br>
RHS:
<code>
logBase x y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use tan</td>
<td>
LHS:
<code>
sin x / cos x
</code>
<br>
RHS:
<code>
tan x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use even</td>
<td>
LHS:
<code>
rem n 2 == 0
</code>
<br>
RHS:
<code>
even n
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use even</td>
<td>
LHS:
<code>
0 == rem n 2
</code>
<br>
RHS:
<code>
even n
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use odd</td>
<td>
LHS:
<code>
rem n 2 /= 0
</code>
<br>
RHS:
<code>
odd n
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use odd</td>
<td>
LHS:
<code>
0 /= rem n 2
</code>
<br>
RHS:
<code>
odd n
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use even</td>
<td>
LHS:
<code>
mod n 2 == 0
</code>
<br>
RHS:
<code>
even n
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use even</td>
<td>
LHS:
<code>
0 == mod n 2
</code>
<br>
RHS:
<code>
even n
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use odd</td>
<td>
LHS:
<code>
mod n 2 /= 0
</code>
<br>
RHS:
<code>
odd n
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use odd</td>
<td>
LHS:
<code>
0 /= mod n 2
</code>
<br>
RHS:
<code>
odd n
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use odd</td>
<td>
LHS:
<code>
not (even x)
</code>
<br>
RHS:
<code>
odd x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use even</td>
<td>
LHS:
<code>
not (odd x)
</code>
<br>
RHS:
<code>
even x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use sqrt</td>
<td>
LHS:
<code>
x ** 0.5
</code>
<br>
RHS:
<code>
sqrt x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use 1</td>
<td>
LHS:
<code>
x ^ 0
</code>
<br>
RHS:
<code>
1
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use floor</td>
<td>
LHS:
<code>
round (x - 0.5)
</code>
<br>
RHS:
<code>
floor x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use writeList2Chan</td>
<td>
LHS:
<code>
mapM_ (writeChan a)
</code>
<br>
RHS:
<code>
writeList2Chan a
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use readTVarIO</td>
<td>
LHS:
<code>
atomically (readTVar x)
</code>
<br>
RHS:
<code>
readTVarIO x
</code>
<br>
</td>
<td>Error</td>
</tr>
<tr>
<td>Use newTVarIO</td>
<td>
LHS:
<code>
atomically (newTVar x)
</code>
<br>
RHS:
<code>
newTVarIO x
</code>
<br>
</td>
<td>Error</td>
</tr>
<tr>
<td>Use newTMVarIO</td>
<td>
LHS:
<code>
atomically (newTMVar x)
</code>
<br>
RHS:
<code>
newTMVarIO x
</code>
<br>
</td>
<td>Error</td>
</tr>
<tr>
<td>Use newEmptyTMVarIO</td>
<td>
LHS:
<code>
atomically newEmptyTMVar
</code>
<br>
RHS:
<code>
newEmptyTMVarIO
</code>
<br>
</td>
<td>Error</td>
</tr>
<tr>
<td>Use typeRep</td>
<td>
LHS:
<code>
typeOf (a :: b)
</code>
<br>
RHS:
<code>
typeRep (Proxy :: Proxy b)
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use handle</td>
<td>
LHS:
<code>
flip Control.Exception.catch
</code>
<br>
RHS:
<code>
handle
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use catch</td>
<td>
LHS:
<code>
flip handle
</code>
<br>
RHS:
<code>
Control.Exception.catch
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use handleJust</td>
<td>
LHS:
<code>
flip (catchJust p)
</code>
<br>
RHS:
<code>
handleJust p
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use catchJust</td>
<td>
LHS:
<code>
flip (handleJust p)
</code>
<br>
RHS:
<code>
catchJust p
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use bracket_</td>
<td>
LHS:
<code>
Control.Exception.bracket b (const a) (const t)
</code>
<br>
RHS:
<code>
Control.Exception.bracket_ b a t
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use withFile</td>
<td>
LHS:
<code>
Control.Exception.bracket (openFile x y) hClose
</code>
<br>
RHS:
<code>
withFile x y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use withBinaryFile</td>
<td>
LHS:
<code>
Control.Exception.bracket (openBinaryFile x y) hClose
</code>
<br>
RHS:
<code>
withBinaryFile x y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use error</td>
<td>
LHS:
<code>
throw (ErrorCall a)
</code>
<br>
RHS:
<code>
error a
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use nonTermination</td>
<td>
LHS:
<code>
toException NonTermination
</code>
<br>
RHS:
<code>
nonTermination
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use nestedAtomically</td>
<td>
LHS:
<code>
toException NestedAtomically
</code>
<br>
RHS:
<code>
nestedAtomically
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use writeIORef</td>
<td>
LHS:
<code>
modifyIORef r (const x)
</code>
<br>
RHS:
<code>
writeIORef r x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use writeIORef</td>
<td>
LHS:
<code>
modifyIORef r (\ v -> x)
</code>
<br>
RHS:
<code>
writeIORef r x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant castPtr</td>
<td>
LHS:
<code>
castPtr nullPtr
</code>
<br>
RHS:
<code>
nullPtr
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant castPtr</td>
<td>
LHS:
<code>
castPtr (castPtr x)
</code>
<br>
RHS:
<code>
castPtr x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant castPtr</td>
<td>
LHS:
<code>
plusPtr (castPtr x)
</code>
<br>
RHS:
<code>
plusPtr x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant castPtr</td>
<td>
LHS:
<code>
minusPtr (castPtr x)
</code>
<br>
RHS:
<code>
minusPtr x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant castPtr</td>
<td>
LHS:
<code>
minusPtr x (castPtr y)
</code>
<br>
RHS:
<code>
minusPtr x y
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant castPtr</td>
<td>
LHS:
<code>
peekByteOff (castPtr x)
</code>
<br>
RHS:
<code>
peekByteOff x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Redundant castPtr</td>
<td>
LHS:
<code>
pokeByteOff (castPtr x)
</code>
<br>
RHS:
<code>
pokeByteOff x
</code>
<br>
</td>
<td>Suggestion</td>
</tr>
<tr>
<td>Use mkWeakPtr</td>
<td>
LHS:
<code>
mkWeak a a b
</code>
<br>
RHS:
<code>
mkWeakPtr a b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use mkWeakPair</td>
<td>
LHS:
<code>
mkWeak a (a, b) c
</code>
<br>
RHS:
<code>
mkWeakPair a b c
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use forM_</td>
<td>
LHS:
<pre>
case m of
  Nothing -> return ()
  Just x -> f x
</pre>
RHS:
<code>
Data.Foldable.forM_ m f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use forM_</td>
<td>
LHS:
<pre>
case m of
  Just x -> f x
  Nothing -> return ()
</pre>
RHS:
<code>
Data.Foldable.forM_ m f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use forM_</td>
<td>
LHS:
<pre>
case m of
  Just x -> f x
  _ -> return ()
</pre>
RHS:
<code>
Data.Foldable.forM_ m f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use forM_</td>
<td>
LHS:
<code>
when (isJust m) (f (fromJust m))
</code>
<br>
RHS:
<code>
Data.Foldable.forM_ m f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use gets</td>
<td>
LHS:
<code>
f <$> Control.Monad.State.get
</code>
<br>
RHS:
<code>
gets f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use gets</td>
<td>
LHS:
<code>
fmap f Control.Monad.State.get
</code>
<br>
RHS:
<code>
gets f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <$></td>
<td>
LHS:
<code>
f <$> Control.Monad.State.gets g
</code>
<br>
RHS:
<code>
gets (f . g)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
fmap f (Control.Monad.State.gets g)
</code>
<br>
RHS:
<code>
gets (f . g)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use asks</td>
<td>
LHS:
<code>
f <$> Control.Monad.Reader.ask
</code>
<br>
RHS:
<code>
asks f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use asks</td>
<td>
LHS:
<code>
fmap f Control.Monad.Reader.ask
</code>
<br>
RHS:
<code>
asks f
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <$></td>
<td>
LHS:
<code>
f <$> Control.Monad.Reader.asks g
</code>
<br>
RHS:
<code>
asks (f . g)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant fmap</td>
<td>
LHS:
<code>
fmap f (Control.Monad.Reader.asks g)
</code>
<br>
RHS:
<code>
asks (f . g)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use evalState</td>
<td>
LHS:
<code>
fst (runState m s)
</code>
<br>
RHS:
<code>
evalState m s
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use execState</td>
<td>
LHS:
<code>
snd (runState m s)
</code>
<br>
RHS:
<code>
execState m s
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
True && x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
False && x
</code>
<br>
RHS:
<code>
False
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
True || x
</code>
<br>
RHS:
<code>
True
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
False || x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
not True
</code>
<br>
RHS:
<code>
False
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
not False
</code>
<br>
RHS:
<code>
True
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
Nothing >>= k
</code>
<br>
RHS:
<code>
Nothing
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
k =<< Nothing
</code>
<br>
RHS:
<code>
Nothing
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
either f g (Left x)
</code>
<br>
RHS:
<code>
f x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
either f g (Right y)
</code>
<br>
RHS:
<code>
g y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
fst (x, y)
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
snd (x, y)
</code>
<br>
RHS:
<code>
y
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
init [x]
</code>
<br>
RHS:
<code>
[]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
null [x]
</code>
<br>
RHS:
<code>
False
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
null []
</code>
<br>
RHS:
<code>
True
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
length []
</code>
<br>
RHS:
<code>
0
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
foldl f z []
</code>
<br>
RHS:
<code>
z
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
foldr f z []
</code>
<br>
RHS:
<code>
z
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
foldr1 f [x]
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
scanr f z []
</code>
<br>
RHS:
<code>
[z]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
scanr1 f []
</code>
<br>
RHS:
<code>
[]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
scanr1 f [x]
</code>
<br>
RHS:
<code>
[x]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
take n []
</code>
<br>
RHS:
<code>
[]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
drop n []
</code>
<br>
RHS:
<code>
[]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
takeWhile p []
</code>
<br>
RHS:
<code>
[]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
dropWhile p []
</code>
<br>
RHS:
<code>
[]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
span p []
</code>
<br>
RHS:
<code>
([], [])
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
lines ""
</code>
<br>
RHS:
<code>
[]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
unwords []
</code>
<br>
RHS:
<code>
""
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
x - 0
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
x * 1
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
x / 1
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
concat [a]
</code>
<br>
RHS:
<code>
a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
concat []
</code>
<br>
RHS:
<code>
[]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
zip [] []
</code>
<br>
RHS:
<code>
[]
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
const x y
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
any (const False)
</code>
<br>
RHS:
<code>
const False
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
all (const True)
</code>
<br>
RHS:
<code>
const True
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
[] ++ x
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
x ++ []
</code>
<br>
RHS:
<code>
x
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
all f [a]
</code>
<br>
RHS:
<code>
f a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
all f []
</code>
<br>
RHS:
<code>
True
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
any f [a]
</code>
<br>
RHS:
<code>
f a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Evaluate</td>
<td>
LHS:
<code>
any f []
</code>
<br>
RHS:
<code>
False
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldr on tuple</td>
<td>
LHS:
<code>
foldr f z (x, b)
</code>
<br>
RHS:
<code>
f b z
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldr' on tuple</td>
<td>
LHS:
<code>
foldr' f z (x, b)
</code>
<br>
RHS:
<code>
f b z
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldl on tuple</td>
<td>
LHS:
<code>
foldl f z (x, b)
</code>
<br>
RHS:
<code>
f z b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldl' on tuple</td>
<td>
LHS:
<code>
foldl' f z (x, b)
</code>
<br>
RHS:
<code>
f z b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldMap on tuple</td>
<td>
LHS:
<code>
foldMap f (x, b)
</code>
<br>
RHS:
<code>
f b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldr1 on tuple</td>
<td>
LHS:
<code>
foldr1 f (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldl1 on tuple</td>
<td>
LHS:
<code>
foldl1 f (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using elem on tuple</td>
<td>
LHS:
<code>
elem e (x, b)
</code>
<br>
RHS:
<code>
e == b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using fold on tuple</td>
<td>
LHS:
<code>
fold (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using toList on tuple</td>
<td>
LHS:
<code>
toList (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using maximum on tuple</td>
<td>
LHS:
<code>
maximum (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using minimum on tuple</td>
<td>
LHS:
<code>
minimum (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using sum on tuple</td>
<td>
LHS:
<code>
sum (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using product on tuple</td>
<td>
LHS:
<code>
product (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using concat on tuple</td>
<td>
LHS:
<code>
concat (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using and on tuple</td>
<td>
LHS:
<code>
and (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using or on tuple</td>
<td>
LHS:
<code>
or (x, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using any on tuple</td>
<td>
LHS:
<code>
any f (x, b)
</code>
<br>
RHS:
<code>
f b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using all on tuple</td>
<td>
LHS:
<code>
all f (x, b)
</code>
<br>
RHS:
<code>
f b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldr on tuple</td>
<td>
LHS:
<code>
foldr f z (x, y, b)
</code>
<br>
RHS:
<code>
f b z
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldr' on tuple</td>
<td>
LHS:
<code>
foldr' f z (x, y, b)
</code>
<br>
RHS:
<code>
f b z
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldl on tuple</td>
<td>
LHS:
<code>
foldl f z (x, y, b)
</code>
<br>
RHS:
<code>
f z b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldl' on tuple</td>
<td>
LHS:
<code>
foldl' f z (x, y, b)
</code>
<br>
RHS:
<code>
f z b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldMap on tuple</td>
<td>
LHS:
<code>
foldMap f (x, y, b)
</code>
<br>
RHS:
<code>
f b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldr1 on tuple</td>
<td>
LHS:
<code>
foldr1 f (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using foldl1 on tuple</td>
<td>
LHS:
<code>
foldl1 f (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using elem on tuple</td>
<td>
LHS:
<code>
elem e (x, y, b)
</code>
<br>
RHS:
<code>
e == b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using fold on tuple</td>
<td>
LHS:
<code>
fold (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using toList on tuple</td>
<td>
LHS:
<code>
toList (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using maximum on tuple</td>
<td>
LHS:
<code>
maximum (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using minimum on tuple</td>
<td>
LHS:
<code>
minimum (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using sum on tuple</td>
<td>
LHS:
<code>
sum (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using product on tuple</td>
<td>
LHS:
<code>
product (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using concat on tuple</td>
<td>
LHS:
<code>
concat (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using and on tuple</td>
<td>
LHS:
<code>
and (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using or on tuple</td>
<td>
LHS:
<code>
or (x, y, b)
</code>
<br>
RHS:
<code>
b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using any on tuple</td>
<td>
LHS:
<code>
any f (x, y, b)
</code>
<br>
RHS:
<code>
f b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using all on tuple</td>
<td>
LHS:
<code>
all f (x, y, b)
</code>
<br>
RHS:
<code>
f b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using null on tuple</td>
<td>
LHS:
<code>
null x
</code>
<br>
RHS:
<code>
False
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Using length on tuple</td>
<td>
LHS:
<code>
length x
</code>
<br>
RHS:
<code>
1
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use empty</td>
<td>
LHS:
<code>
Data.Map.fromList []
</code>
<br>
RHS:
<code>
Data.Map.empty
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use empty</td>
<td>
LHS:
<code>
Data.Map.Lazy.fromList []
</code>
<br>
RHS:
<code>
Data.Map.Lazy.empty
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use empty</td>
<td>
LHS:
<code>
Data.Map.Strict.fromList []
</code>
<br>
RHS:
<code>
Data.Map.Strict.empty
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant ^.</td>
<td>
LHS:
<code>
(a ^. b) ^. c
</code>
<br>
RHS:
<code>
a ^. (b . c)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use ^?!</td>
<td>
LHS:
<code>
fromJust (a ^? b)
</code>
<br>
RHS:
<code>
a ^?! b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use ?~</td>
<td>
LHS:
<code>
a .~ Just b
</code>
<br>
RHS:
<code>
a ?~ b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <&></td>
<td>
LHS:
<code>
(mapped %~ b) a
</code>
<br>
RHS:
<code>
a <&> b
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <&></td>
<td>
LHS:
<code>
((mapped . b) %~ c) a
</code>
<br>
RHS:
<code>
a <&> b %~ c
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use <$</td>
<td>
LHS:
<code>
(mapped .~ b) a
</code>
<br>
RHS:
<code>
b <$ a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use view</td>
<td>
LHS:
<code>
ask <&> (^. a)
</code>
<br>
RHS:
<code>
view a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Redundant <&></td>
<td>
LHS:
<code>
view a <&> (^. b)
</code>
<br>
RHS:
<code>
view (a . b)
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use ix</td>
<td>
LHS:
<code>
Control.Lens.at a . Control.Lens._Just
</code>
<br>
RHS:
<code>
Control.Lens.ix a
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use True</td>
<td>
LHS:
<code>
Control.Lens.has (Control.Lens.at a)
</code>
<br>
RHS:
<code>
True
</code>
<br>
</td>
<td>Error</td>
</tr>
<tr>
<td>Redundant at</td>
<td>
LHS:
<code>
Control.Lens.has (a . Control.Lens.at b)
</code>
<br>
RHS:
<code>
Control.Lens.has a
</code>
<br>
</td>
<td>Error</td>
</tr>
<tr>
<td>Use False</td>
<td>
LHS:
<code>
Control.Lens.nullOf (Control.Lens.at a)
</code>
<br>
RHS:
<code>
False
</code>
<br>
</td>
<td>Error</td>
</tr>
<tr>
<td>Redundant at</td>
<td>
LHS:
<code>
Control.Lens.nullOf (a . Control.Lens.at b)
</code>
<br>
RHS:
<code>
Control.Lens.nullOf a
</code>
<br>
</td>
<td>Error</td>
</tr>
<tr>
<td>Use optional</td>
<td>
LHS:
<code>
Data.Attoparsec.Text.option Nothing (Just <$> p)
</code>
<br>
RHS:
<code>
optional p
</code>
<br>
</td>
<td>Warning</td>
</tr>
<tr>
<td>Use optional</td>
<td>
LHS:
<code>
Data.Attoparsec.ByteString.option Nothing (Just <$> p)
</code>
<br>
RHS:
<code>
optional p
</code>
<br>
</td>
<td>Warning</td>
</tr>
</table>

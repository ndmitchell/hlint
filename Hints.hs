
module Hints where

import Data.List


concat_map f x = concat (map f x)

map_map f g x = map f (map g x)

box_append x y = [x] ++ y

head_index x = x !! 0

tail_drop x = drop 1 x

use_replicate n x = take n (repeat x)

use_unwords1 x xs = x ++ concatMap (' ':) xs

use_unwords2 xs = concat (intersperse " " xs)

no_if a = if a then True else False

use_not a = if a then False else True

use_neq a b = not (a == b)

use_eq a b = not (a /= b)

use_or a b t f = if a then t else (if b then t else f)

use_and a b t f = if a then (if b then t else f) else f

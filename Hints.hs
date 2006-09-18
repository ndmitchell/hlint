
module Hints where

concat_map f x = concat (map f x)

map_map f g x = map f (map g x)

box_append x y = [x] ++ y



module Hints where

concat_map f x = concat (map f x)

map_map f g x = map f (map g x)

box_append x y = [x] ++ y

head_index x = x !! 0

tail_drop x = drop 1 x

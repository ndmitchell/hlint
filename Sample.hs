
module Sample where

myfunc f = concat . map f

myfunc2 f g x = map f (map g x)

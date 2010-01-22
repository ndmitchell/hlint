
module HSE.Eq(eqExpShell) where

import HSE.Type
import HSE.Util
import Data.Function


eqExpShell :: Exp_ -> Exp_ -> Bool
eqExpShell x y = eqExpShellFast x y && eqExpShellSlow x y


eqExpShellFast x y = f (show x) (show y)
    where f (x:xs) (y:ys) | x == y = x == ' ' || f xs ys
          f [] [] = True
          f _ _ = False


eqExpShellSlow = (==) `on` descend (const $ Do an [])

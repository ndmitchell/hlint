
module HSE.Eq(eqExpShell) where

import HSE.Type
import HSE.Util
import Data.Function
import Data.Data


eqExpShell :: Exp_ -> Exp_ -> Bool
eqExpShell x y = eqExpShellFast x y && eqExpShellSlow x y

eqExpShellFast x y = toConstr x == toConstr y

eqExpShellSlow = (==) `on` descend (const $ Do an [])

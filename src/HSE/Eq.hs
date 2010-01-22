
module HSE.Eq(eqExpShell) where

import HSE.Type
import HSE.Util
import Data.Function
import Data.Data


-- A small expression
ex :: Exp_
ex = Do an []


eqExpShell :: Exp_ -> Exp_ -> Bool
eqExpShell x y = eqExpShellFast x y && eqExpShellSlow x y

eqExpShellFast x y = toConstr x == toConstr y

eqExpShellSlow = (==) `on` descend (const ex)


{-
-- The following eqExpShell method is based on the SYB functions.
-- It performs roughly the same as eqExpShell above, but is more
-- complex so is not used by default.
-- It is possible some overhead could be removed by optimising the
-- SYB calls.

data Box = forall a . Data a => Box a

eqExpShellSYB :: Exp_ -> Exp_ -> Bool
eqExpShellSYB = f
    where
        f :: (Data a, Data b) => a -> b -> Bool
        f x y =
            toConstr x == toConstr y &&
            andZipWith g (gmapQ Box x) (gmapQ Box y)

        g (Box x) (Box y)
            | let tx = typeOf x in tx == typeAnn || tx == typeExp = True
            | otherwise = f x y


typeAnn = typeOf an
typeExp = typeOf ex


andZipWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
andZipWith op = f
    where
        f (x:xs) (y:ys) = op x y && f xs ys
        f [] [] = True
        f _ _ = error "Internal error: andZipWith on unequal lengths"
-}

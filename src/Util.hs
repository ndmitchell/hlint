{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Util(
    forceList,
    gzip, universeParentBi,
    exitMessage, exitMessageImpure,
    getContentsUTF8
    ) where

import System.Exit
import System.IO
import System.IO.Unsafe
import Unsafe.Coerce
import Data.Data
import Data.Generics.Uniplate.Operations


---------------------------------------------------------------------
-- CONTROL.DEEPSEQ

forceList :: [a] -> [a]
forceList xs = length xs `seq` xs


---------------------------------------------------------------------
-- SYSTEM.IO

exitMessage :: String -> IO a
exitMessage msg = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1

exitMessageImpure :: String -> a
exitMessageImpure = unsafePerformIO . exitMessage


getContentsUTF8 :: IO String
getContentsUTF8 = do
    hSetEncoding stdin utf8
    getContents


---------------------------------------------------------------------
-- DATA.GENERICS

data Box = forall a . Data a => Box a

gzip :: Data a => (forall b . Data b => b -> b -> c) -> a -> a -> Maybe [c]
gzip f x y | toConstr x /= toConstr y = Nothing
           | otherwise = Just $ zipWith op (gmapQ Box x) (gmapQ Box y)
         -- unsafeCoerce is safe because gmapQ on the same constr gives the same fields
         -- in the same order
    where op (Box x) (Box y) = f x (unsafeCoerce y)


---------------------------------------------------------------------
-- DATA.GENERICS.UNIPLATE.OPERATIONS

universeParent :: Uniplate a => a -> [(Maybe a, a)]
universeParent x = (Nothing,x) : f x
    where
        f :: Uniplate a => a -> [(Maybe a, a)]
        f x = concat [(Just x, y) : f y | y <- children x]

universeParentBi :: Biplate a b => a -> [(Maybe b, b)]
universeParentBi = concatMap universeParent . childrenBi

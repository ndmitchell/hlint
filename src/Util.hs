{-# LANGUAGE ExistentialQuantification, Rank2Types, PatternGuards #-}

module Util(
    defaultExtensions,
    Encoding, defaultEncoding, readFileEncoding',
    gzip, universeParentBi, descendIndex,
    exitMessage
    ) where

import Control.Monad.Trans.State
import Control.Exception
import Data.List
import System.Exit
import System.IO.Extra hiding (readFileEncoding')
import System.IO.Unsafe
import Unsafe.Coerce
import Data.Data
import Data.Generics.Uniplate.Operations
import Language.Haskell.Exts.Extension


---------------------------------------------------------------------
-- SYSTEM.IO

-- | An 'Encoding' represents how characters are stored in a file. Created with
--   'defaultEncoding' or 'readEncoding' and used with 'useEncoding'.
type Encoding = TextEncoding

-- | The system default encoding.
defaultEncoding :: Encoding
defaultEncoding = utf8

-- | Apply an encoding to a 'Handle'.
useEncoding :: Handle -> Encoding -> IO ()
useEncoding = hSetEncoding

readFileEncoding' :: Encoding -> FilePath -> IO String
readFileEncoding' enc "-" = do
    useEncoding stdin enc
    getContents
readFileEncoding' enc file = withFile file ReadMode $ \h -> do
    useEncoding h enc
    s <- hGetContents h
    evaluate $ length s
    return s


exitMessage :: String -> a
exitMessage msg = unsafePerformIO $ do
    putStrLn msg
    exitWith $ ExitFailure 1


---------------------------------------------------------------------
-- DATA.GENERICS

data Box = forall a . Data a => Box a

gzip :: Data a => (forall b . Data b => b -> b -> c) -> a -> a -> Maybe [c]
gzip f x y | toConstr x /= toConstr y = Nothing
           | otherwise = Just $ zipWith op (gmapQ Box x) (gmapQ Box y)
    where op (Box x) (Box y) = f x (unsafeCoerce y)


---------------------------------------------------------------------
-- DATA.GENERICS.UNIPLATE.OPERATIONS

descendIndex :: Uniplate a => (Int -> a -> a) -> a -> a
descendIndex f x = flip evalState 0 $ flip descendM x $ \y -> do
    i <- get
    modify (+1)
    return $ f i y

universeParent :: Uniplate a => a -> [(Maybe a, a)]
universeParent x = (Nothing,x) : f x
    where
        f :: Uniplate a => a -> [(Maybe a, a)]
        f x = concat [(Just x, y) : f y | y <- children x]

universeParentBi :: Biplate a b => a -> [(Maybe b, b)]
universeParentBi = concatMap universeParent . childrenBi


---------------------------------------------------------------------
-- LANGUAGE.HASKELL.EXTS.EXTENSION

defaultExtensions :: [Extension]
defaultExtensions = [e | e@EnableExtension{} <- knownExtensions] \\ map EnableExtension badExtensions

badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ,DoRec, RecursiveDo -- breaks rec
    ,TypeApplications -- HSE fails on @ patterns
    ]

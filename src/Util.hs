{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Util(
    parseExtensions,
    configExtensions,
    forceList,
    gzip, universeParentBi,
    exitMessage, exitMessageImpure,
    getContentsUTF8
    ) where

import Data.List
import System.Exit
import System.IO
import System.IO.Unsafe
import Unsafe.Coerce
import Data.Data
import Data.Generics.Uniplate.Operations
import Language.Haskell.Exts.Extension


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


---------------------------------------------------------------------
-- LANGUAGE.HASKELL.EXTS.EXTENSION

-- | Extensions we turn on by default when parsing. Aim to parse as many files as we can.
parseExtensions :: [Extension]
parseExtensions = [e | e@EnableExtension{} <- knownExtensions] \\ map EnableExtension badExtensions

-- | Extensions we turn on when reading config files, don't have to deal with the whole world
--   of variations - in particular, we might require spaces in some places.
configExtensions :: [Extension]
configExtensions = [e | e@EnableExtension{} <- knownExtensions] \\ map EnableExtension reallyBadExtensions

badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples, UnboxedSums -- breaks (#) lens operator
    ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ,DoRec, RecursiveDo -- breaks rec
    ,TypeApplications -- HSE fails on @ patterns
    ]

reallyBadExtensions =
    [TransformListComp -- steals the group keyword
    ]

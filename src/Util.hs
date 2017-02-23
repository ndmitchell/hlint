{-# LANGUAGE ExistentialQuantification, Rank2Types, PatternGuards #-}

module Util(
    defaultExtensions,
    Encoding, defaultEncoding, readFileEncoding', readEncoding, useEncoding,
    gzip, universeParentBi, descendIndex,
    exitMessage,
    isUnboxedTyCon
    ) where

import Control.Monad.Trans.State
import Control.Exception
import Data.Char
import Data.List
import System.Exit
import System.IO.Extra hiding (readFileEncoding')
import System.IO.Unsafe
import Unsafe.Coerce
import Data.Data
import Data.Generics.Uniplate.Operations
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Syntax


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


-- | Create an encoding from a string, or throw an error if the encoding is not known.
--   Accepts many encodings including @locale@, @utf-8@ and all those supported by the
--   GHC @mkTextEncoding@ function.
readEncoding :: String -> IO Encoding
-- GHC's mkTextEncoding function is fairly poor - it doesn't support lots of fun things,
-- so we fake them up, and then try mkTextEncoding last
readEncoding "" = return defaultEncoding
readEncoding enc
        | Just e <- lookup (f enc) [(f a, b) | (as,b) <- encs, a <- as] = return e
        | otherwise = do
            res <- try $ mkTextEncoding enc :: IO (Either SomeException TextEncoding)
            case res of
                Right e -> return e
                Left _ -> do
                    let (a,b) = splitAt 2 $ map (head . fst) encs
                    putStr $ unlines
                        ["Error: Unknown text encoding argument, " ++ enc
                        ,"Possible values:"
                        ,"  " ++ unwords a
                        ,"  " ++ unwords b
                        ,"  and anything accepted by System.IO.mkTextEncoding"]
                    exitWith $ ExitFailure 1
    where
        f = map toLower . filter (`notElem` "-_ ")

        encs = let a*b = (words a, b)
               in ["ISO8859-1 8859-1 ISO8859 8859 LATIN LATIN1" * latin1
                  ,"LOCALE" * localeEncoding
                  ,"UTF-8" * utf8
                  ,"UTF-8BOM" * utf8_bom
                  ,"UTF-16" * utf16
                  ,"UTF-16LE" * utf16le
                  ,"UTF-16BE" * utf16be
                  ,"UTF-32" * utf16
                  ,"UTF-32LE" * utf16le
                  ,"UTF-32BE" * utf16be]


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

---------------------------------------------------------------------
-- LANGUAGE.HASKELL.EXTS.SYNTAX

isUnboxedTyCon :: Type l -> Bool
isUnboxedTyCon (TyParen _ t) = isUnboxedTyCon t
isUnboxedTyCon (TyApp _ (TyCon _ qname) _) = not (null name) || last name == '#'
  where name = case qname of
          Qual _ _ n -> nameString n
          UnQual _ n -> nameString n
          _ -> ""

        nameString :: Name l -> String
        nameString (Ident _ n) = n
        nameString (Symbol _ n) = n
isUnboxedTyCon _ = False


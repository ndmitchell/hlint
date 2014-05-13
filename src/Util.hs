{-# LANGUAGE ExistentialQuantification, Rank2Types, PatternGuards #-}

module Util where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State
import Control.Exception
import Data.Char
import Data.Function
import Data.List
import Data.Ord
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import Unsafe.Coerce
import Data.Data
import Data.Generics.Uniplate.Operations
import Language.Haskell.Exts.Extension
import GHC.IO.Handle(hDuplicate,hDuplicateTo)


---------------------------------------------------------------------
-- SYSTEM.DIRECTORY

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    xs <- getDirectoryContents dir
    (dirs,files) <- partitionM doesDirectoryExist [dir </> x | x <- xs, not $ isBadDir x]
    rest <- concatMapM getDirectoryContentsRecursive $ sort dirs
    return $ sort files ++ rest
    where
        isBadDir x = "." `isPrefixOf` x || "_" `isPrefixOf` x


---------------------------------------------------------------------
-- CONTROL.MONAD

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

concatM :: Monad m => [m [a]] -> m [a]
concatM = liftM concat . sequence

concatZipWithM :: Monad m => (a -> b -> m [c]) -> [a] -> [b] -> m [c]
concatZipWithM f xs ys = liftM concat $ zipWithM f xs ys

listM' :: Monad m => [a] -> m [a]
listM' x = length x `seq` return x


---------------------------------------------------------------------
-- PRELUDE

notNull = not . null

headDef :: a -> [a] -> a
headDef x [] = x
headDef x (y:ys) = y

isLeft_ Left{} = True; isLeft_ _ = False
isRight_ = not . isLeft_

unzipEither :: [Either a b] -> ([a], [b])
unzipEither (x:xs) = case x of
    Left y -> (y:a,b)
    Right y -> (a,y:b)
    where (a,b) = unzipEither xs
unzipEither [] = ([], [])


for = flip map

---------------------------------------------------------------------
-- DATA.STRING

limit :: Int -> String -> String
limit n s = if null post then s else pre ++ "..."
    where (pre,post) = splitAt n s

ltrim :: String -> String
ltrim = dropWhile isSpace

rtrim :: String -> String
rtrim = reverse . ltrim . reverse

trimBy :: (a -> Bool) -> [a] -> [a]
trimBy f = reverse . dropWhile f . reverse . dropWhile f


---------------------------------------------------------------------
-- DATA.LIST

groupSortFst :: Ord a => [(a,b)] -> [(a,[b])]
groupSortFst = map (fst . head &&& map snd) . groupBy ((==) `on` fst) . sortBy (comparing fst)

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint xs = null . intersect xs

unsnoc :: [a] -> ([a],a)
unsnoc [] = error "Unsnoc on empty list"
unsnoc xs = (init xs, last xs)

revTake :: Int -> [a] -> [a]
revTake i = reverse . take i . reverse

concatUnzip :: [([a], [b])] -> ([a], [b])
concatUnzip = (concat *** concat) . unzip


---------------------------------------------------------------------
-- DATA.TUPLE

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

concat3 :: [([a],[b],[c])] -> ([a],[b],[c])
concat3 xs = (concat a, concat b, concat c)
    where (a,b,c) = unzip3 xs

concat2 :: [([a],[b])] -> ([a],[b])
concat2 xs = (concat a, concat b)
    where (a,b) = unzip xs


---------------------------------------------------------------------
-- SYSTEM.IO

-- | An 'Encoding' represents how characters are stored in a file. Created with
--   'defaultEncoding' or 'readEncoding' and used with 'useEncoding'.
data Encoding = Encoding_Internal (Maybe (Handle -> IO ()))

-- | The system default encoding.
defaultEncoding :: Encoding
defaultEncoding = Encoding_Internal Nothing

-- | Apply an encoding to a 'Handle'.
useEncoding :: Handle -> Encoding -> IO ()
useEncoding h (Encoding_Internal x) = maybe (return ()) ($ h) x

readFileEncoding :: Encoding -> FilePath -> IO String
readFileEncoding enc file = do
    h <- if file == "-" then return stdin else openFile file ReadMode
    useEncoding h enc
    hGetContents h


-- | Create an encoding from a string, or throw an error if the encoding is not known.
--   Accepts many encodings including @locale@, @utf-8@ and all those supported by the
--   GHC @mkTextEncoding@ function.
readEncoding :: String -> IO Encoding
-- GHC's mkTextEncoding function is fairly poor - it doesn't support lots of fun things,
-- so we fake them up, and then try mkTextEncoding last
readEncoding "" = return defaultEncoding
readEncoding enc
        | Just e <- lookup (f enc) [(f a, b) | (as,b) <- encs, a <- as] = return $ wrap e
        | otherwise = do
            res <- try $ mkTextEncoding enc :: IO (Either SomeException TextEncoding)
            case res of
                Right e -> return $ wrap e
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
        wrap = Encoding_Internal . Just . flip hSetEncoding 

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


withTemporaryFile :: String -> (FilePath -> IO a) -> IO a
withTemporaryFile pat act = do
    tmp <- getTemporaryDirectory
    bracket (openTempFile tmp pat) (removeFile . fst) $
        \(file,h) -> hClose h >> act file


captureOutput :: IO () -> IO String
captureOutput act = withTemporaryFile "hlint_capture_output.txt" $ \file -> do
    h <- openFile file ReadWriteMode
    bout <- hGetBuffering stdout
    berr <- hGetBuffering stderr
    sto <- hDuplicate stdout
    ste <- hDuplicate stderr
    hDuplicateTo h stdout
    hDuplicateTo h stderr
    hClose h
    act
    hDuplicateTo sto stdout
    hDuplicateTo ste stderr
    hSetBuffering stdout bout
    hSetBuffering stderr berr
    readFile' file


withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering h m act = bracket (hGetBuffering h) (hSetBuffering h) $ const $ do
    hSetBuffering h m
    act


-- FIXME: Should use strict ByteString
readFile' :: FilePath -> IO String
readFile' x = listM' =<< readFile x


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
    ]

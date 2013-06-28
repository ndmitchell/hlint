{-# LANGUAGE CPP, ExistentialQuantification, Rank2Types, PatternGuards #-}

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

#if __GLASGOW_HASKELL__ >= 612
import GHC.IO.Handle(hDuplicate,hDuplicateTo)
#endif


---------------------------------------------------------------------
-- SYSTEM.DIRECTORY

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    xs <- getDirectoryContents dir
    (dirs,files) <- partitionM doesDirectoryExist [dir </> x | x <- xs, not $ isBadDir x]
    rest <- concatMapM getDirectoryContentsRecursive dirs
    return $ files++rest
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

#if !MIN_VERSION_base(4,7,0)
isLeft Left{} = True; isLeft _ = False
isRight = not . isLeft
#endif

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


---------------------------------------------------------------------
-- DATA.TUPLE

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)


---------------------------------------------------------------------
-- SYSTEM.IO

-- | An encoding is a function to change a handle to a particular encoding
data Encoding = Encoding_Internal (Maybe (Handle -> IO ()))

defaultEncoding :: Encoding
defaultEncoding = Encoding_Internal Nothing


readFileEncoding :: Encoding -> FilePath -> IO String
readFileEncoding (Encoding_Internal x) file = case x of
    Nothing -> if file == "-" then getContents else readFile file
    Just set -> do
        h <- if file == "-" then return stdin else openFile file ReadMode
        set h
        hGetContents h


-- GHC's mkTextEncoding function is fairly poor - it doesn't support lots of fun things,
-- so we fake them up, and then try mkTextEncoding last
newEncoding :: String -> IO Encoding
newEncoding "" = return defaultEncoding
#if __GLASGOW_HASKELL__ >= 612
newEncoding enc
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
#else
newEncoding enc = do
    putStrLn "Warning: Text encodings are not supported with HLint compiled by GHC 6.10"
    return defaultEncoding
#endif


exitMessage :: String -> a
exitMessage msg = unsafePerformIO $ do
    putStrLn msg
    exitWith $ ExitFailure 1


-- FIXME: This could use a lot more bracket calls!
captureOutput :: IO () -> IO (Maybe String)
#if __GLASGOW_HASKELL__ < 612
captureOutput act = return Nothing
#else
captureOutput act = do
    tmp <- getTemporaryDirectory
    (f,h) <- openTempFile tmp "hlint"
    sto <- hDuplicate stdout
    ste <- hDuplicate stderr
    hDuplicateTo h stdout
    hDuplicateTo h stderr
    hClose h
    act
    hDuplicateTo sto stdout
    hDuplicateTo ste stderr
    res <- readFile' f
    removeFile f
    return $ Just res
#endif


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
defaultExtensions = knownExtensions \\ badExtensions

badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ]

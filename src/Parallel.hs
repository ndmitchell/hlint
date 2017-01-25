{-
The parallel function (specialised to lists) is equivalent to:

import Control.Parallel.Strategies
parallel :: [IO [a]] -> IO [[a]]
parallel = return . withStrategy (parList $ seqList r0) . map unsafePerformIO

However, this version performs about 10% slower with 2 processors in GHC 6.12.1
-}

module Parallel(parallel) where

import System.IO.Unsafe
import Control.Concurrent
import Control.Exception
import Control.Monad


parallel :: Int -> [IO a] -> IO [a]
parallel j = if j <= 1 then parallel1 else parallelN j


parallel1 :: [IO a] -> IO [a]
parallel1 [] = return []
parallel1 (x:xs) = do
    x2 <- x
    xs2 <- unsafeInterleaveIO $ parallel1 xs
    return $ x2:xs2


parallelN :: Int -> [IO a] -> IO [a]
parallelN j xs = do
    ms <- mapM (const newEmptyMVar) xs
    chan <- newChan
    mapM_ (writeChan chan . Just) $ zip ms xs
    replicateM_ j (writeChan chan Nothing >> forkIO (f chan))
    let throwE x = throw (x :: SomeException)
    parallel1 $ map (fmap (either throwE id) . takeMVar) ms
    where
        f chan = do
            v <- readChan chan
            case v of
                Nothing -> return ()
                Just (m,x) -> do
                    putMVar m =<< try x
                    f chan


module Parallel(parallel) where

import System.IO.Unsafe(unsafeInterleaveIO)
import GHC.Conc(numCapabilities)
import Control.Concurrent
import Control.Monad
import Control.Parallel


parallel :: [IO a] -> IO [a]
parallel = if numCapabilities <= 1 then parallel1 else parallelN


parallel1 :: [IO a] -> IO [a]
parallel1 [] = return []
parallel1 (x:xs) = do
    x2 <- x
    xs2 <- unsafeInterleaveIO $ parallel1 xs
    return $ x2:xs2


parallelN :: [IO a] -> IO [a]
parallelN xs = do
    ms <- mapM (const newEmptyMVar) xs
    chan <- newChan
    mapM (writeChan chan . Just) $ zip ms xs
    replicateM_ numCapabilities (writeChan chan Nothing >> forkIO (f chan))
    parallel1 $ map takeMVar ms
    where
        f chan = do
            v <- readChan chan
            case v of
                Nothing -> return ()
                Just (m,x) -> do
                    x' <- x
                    putMVar m x'
                    f chan

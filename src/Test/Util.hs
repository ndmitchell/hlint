{-# LANGUAGE RecordWildCards #-}

module Test.Util(
    withTests, tested, passed, failed, progress
    ) where

import Data.IORef
import System.IO.Unsafe
import Control.Monad


data Result = Result {failures :: Int, total :: Int} deriving Show

{-# NOINLINE ref #-}
ref :: IORef [Result]
ref = unsafePerformIO $ newIORef []


-- | Returns the number of failing tests.
--   Warning: Not multithread safe, but is reenterant
withTests :: IO () -> IO Int
withTests act = do
    atomicModifyIORef ref $ \r -> (Result 0 0 : r, ())
    act
    Result{..} <- atomicModifyIORef ref $ \(r:rs) -> (rs, r)
    putStrLn ""
    putStrLn $ if failures == 0
        then "Tests passed (" ++ show total ++ ")"
        else "Tests failed (" ++ show failures ++ " of " ++ show total ++ ")"
    return failures

progress :: IO ()
progress = putChar '.'

passed :: IO ()
passed = atomicModifyIORef ref $ \(r:rs) -> (r{total=total r+1}:rs, ())

failed :: [String] -> IO ()
failed xs = do
    unless (null xs) $ putStrLn $ unlines $ "" : xs
    atomicModifyIORef ref $ \(r:rs) -> (r{total=total r+1, failures=failures r+1}:rs, ())

tested :: Bool -> IO ()
tested b = if b then passed else failed []

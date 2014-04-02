{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

module Test.Util(
    Result(..),
    pass, failure, result, results,
    progress, failed
    ) where

import Data.Monoid


data Result = Result {resultFailures :: Int, resultTotal :: Int}

pass :: Result
pass = Result 0 1

failure :: Result
failure = Result 1 1

result :: Bool -> Result
result x = if x then pass else failure

results :: IO [Result] -> IO Result
results = fmap mconcat

instance Monoid Result where
    mempty = Result 0 0
    mappend (Result f1 t1) (Result f2 t2) = Result (f1+f2) (t1+t2)

progress :: IO ()
progress = putChar '.'

failed :: [String] -> IO ()
failed xs = putStrLn $ unlines $ "" : xs


module Timing(
    timed, timedIO,
    startTimings,
    printTimings
    ) where

import qualified Data.HashMap.Strict as Map
import Control.Exception
import Data.IORef.Extra
import Data.Tuple.Extra
import Data.List.Extra
import Control.Monad
import System.Console.CmdArgs.Verbosity
import System.Time.Extra
import System.IO.Unsafe
import System.IO


type Category = String
type Item = String

{-# NOINLINE useTimingsRef #-}
useTimingsRef :: IORef Bool
useTimingsRef = unsafePerformIO $ newIORef False

{-# NOINLINE useTimings #-}
useTimings :: Bool
useTimings = unsafePerformIO $ readIORef useTimingsRef

{-# NOINLINE timings #-}
timings :: IORef (Map.HashMap (Category, Item) Seconds)
timings = unsafePerformIO $ newIORef Map.empty

{-# NOINLINE timed #-}
timed :: Category -> Item -> a -> a
timed c i x = if not useTimings then x else unsafePerformIO $ timedIO c i $ evaluate x


timedIO :: Category -> Item -> IO a -> IO a
timedIO c i x = if not useTimings then x else do
    let quiet = c == "Hint"
    unless quiet $ whenLoud $ do
        putStr $ "# " ++ c ++ " of " ++ i ++ "... "
        hFlush stdout
    (time, x) <- duration x
    atomicModifyIORef'_ timings $ Map.insertWith (+) (c, i) time
    unless quiet $ whenLoud $ putStrLn $ "took " ++ showDuration time
    pure x

startTimings :: IO ()
startTimings = do
    writeIORef useTimingsRef True
    writeIORef timings Map.empty

printTimings :: IO ()
printTimings = do
    mp <- readIORef timings
    let items = sortOn (sumSnd . snd) $
                groupSort $ map (\((a,b),c) -> (a,(b,c))) $ Map.toList mp
    putStrLn $ unlines $ intercalate [""] $ map disp $ items ++ [("TOTAL", map (second sumSnd) items)]
    where
        sumSnd = sum . map snd

        disp (cat,xs) =
                ("Timing " ++ cat) :
                ["  " ++ showDuration b ++ " " ++ a | (a,b) <- xs2] ++
                ["  " ++ showDuration (sumSnd xs2) ++ " TOTAL"]
            where
                xs2 = f $ splitAt 9 $ sortOn (negate . snd) xs
                f (xs,ys)
                    | length ys <= 1 = xs ++ ys
                    | otherwise = xs ++ [("Other items (" ++ show (length ys) ++ ")", sumSnd ys)]

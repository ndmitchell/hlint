{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

module Test.Util(
    Test, withTests, tested, passed, failed, progress
    ) where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class


data S = S
    {failures :: !Int
    ,total :: !Int
    }

newtype Test a = Test (StateT S IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Returns the number of failing tests.
withTests :: Test () -> IO Int
withTests (Test act) = do
    S{..} <- execStateT act $ S 0 0
    putStrLn ""
    putStrLn $ if failures == 0
        then "Tests passed (" ++ show total ++ ")"
        else "Tests failed (" ++ show failures ++ " of " ++ show total ++ ")"
    return failures

progress :: Test ()
progress = liftIO $ putChar '.'

passed :: Test ()
passed = Test $ modify $ \s -> s{total=total s+1}

failed :: [String] -> Test ()
failed xs = do
    unless (null xs) $ liftIO $ putStrLn $ unlines $ "" : xs
    Test $ modify $ \s -> s{total=total s+1, failures=failures s+1}

tested :: Bool -> Test ()
tested b = if b then passed else failed []

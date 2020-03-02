{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

module Test.Util(
    Test, withTests,
    passed, failed, progress,
    addIdeas, getIdeas
    ) where

import Idea
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef


data S = S
    {failures :: !Int
    ,total :: !Int
    ,ideas :: [[Idea]]
    }

newtype Test a = Test (ReaderT (IORef S) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

-- | Returns the number of failing tests.
withTests :: Test a -> IO (Int, a)
withTests (Test act) = do
    ref <- newIORef $ S 0 0 []
    res <- runReaderT act ref
    S{..} <- readIORef ref
    putStrLn ""
    putStrLn $ if failures == 0
        then "Tests passed (" ++ show total ++ ")"
        else "Tests failed (" ++ show failures ++ " of " ++ show total ++ ")"
    pure (failures, res)

addIdeas :: [Idea] -> Test ()
addIdeas xs = do
    ref <- Test ask
    liftIO $ modifyIORef' ref $ \s -> s{ideas = xs : ideas s}

getIdeas :: Test [Idea]
getIdeas = do
    ref <- Test ask
    liftIO $ concat . reverse . ideas <$> readIORef ref

progress :: Test ()
progress = liftIO $ putChar '.'

passed :: Test ()
passed = do
    ref <- Test ask
    liftIO $ modifyIORef' ref $ \s -> s{total=total s+1}

failed :: [String] -> Test ()
failed xs = do
    unless (null xs) $ liftIO $ putStrLn $ unlines $ "" : xs
    ref <- Test ask
    liftIO $ modifyIORef' ref $ \s -> s{total=total s+1, failures=failures s+1}

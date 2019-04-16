{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

module Test.Util(
    Test, withTests,
    tested, passed, failed, progress,
    addIdeas, getIdeas
    ) where

import Idea
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class


data S = S
    {failures :: !Int
    ,total :: !Int
    ,ideas :: [Idea]
    }

newtype Test a = Test (StateT S IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Returns the number of failing tests.
withTests :: Test a -> IO (Int, a)
withTests (Test act) = do
    (res, S{..}) <- runStateT act $ S 0 0 []
    putStrLn ""
    putStrLn $ if failures == 0
        then "Tests passed (" ++ show total ++ ")"
        else "Tests failed (" ++ show failures ++ " of " ++ show total ++ ")"
    return (failures, res)

addIdeas :: [Idea] -> Test ()
addIdeas xs = Test $ modify $ \s -> s{ideas = xs ++ ideas s}

getIdeas :: Test [Idea]
getIdeas = Test $ gets ideas

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

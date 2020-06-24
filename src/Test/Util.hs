{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}

module Test.Util(
    Test, withTests,
    passed, failed, progress,
    addIdeas, getIdeas,
    BuiltinSummary, BuiltinEx(..), addBuiltin, getBuiltins,
    ) where

import Idea
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map.Strict as Map

-- | A map from (hint name, hint severity, does hint support refactoring) to an example.
type BuiltinSummary = Map (String, Severity, Bool) BuiltinEx

data BuiltinEx = BuiltinEx
    { builtinInp :: !String
    , builtinFrom :: !String
    , builtinTo :: !(Maybe String)
    }

data S = S
    {failures :: !Int
    ,total :: !Int
    ,ideas :: [[Idea]]
    ,builtinHints :: BuiltinSummary
    -- ^ A summary of builtin hints
    }

newtype Test a = Test (ReaderT (IORef S) IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Returns the number of failing tests.
withTests :: Test a -> IO (Int, a)
withTests (Test act) = do
    ref <- newIORef $ S 0 0 [] Map.empty
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

addBuiltin :: String -> Idea -> Test ()
addBuiltin inp idea@Idea{..} = unless ("Parse error" `isPrefixOf` ideaHint) $ do
    ref <- Test ask
    liftIO $ modifyIORef' ref $ \s ->
        let k = (ideaHint, ideaSeverity, notNull ideaRefactoring)
            v = BuiltinEx inp ideaFrom ideaTo
         -- Do not insert if the key already exists in the map. This has the effect
         -- of picking the first test case of a hint as the example in the summary.
         in s{builtinHints = Map.insertWith (curry snd) k v (builtinHints s)}

getBuiltins :: Test BuiltinSummary
getBuiltins = do
    ref <- Test ask
    liftIO $ builtinHints <$> readIORef ref

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

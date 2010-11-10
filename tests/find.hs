{-# LANGUAGE CPP, ExistentialQuantification, Rank2Types #-}

module Util where

import Control.Arrow
import Control.Monad.Trans.State
import Data.Char
import Data.Function

listM' :: Monad m => [a] -> m [a]
listM' x = length x `seq` return x

notNull = not . null

headDef :: a -> [a] -> a
headDef x [] = x
headDef x (y:ys) = y

isLeft Left{} = True; isLeft _ = False
isRight = not . isLeft

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

exitMessage :: String -> a
exitMessage msg = unsafePerformIO $ do
    putStrLn msg
    exitWith $ ExitFailure 1

defaultExtensions = knownExtensions \\ badExtensions

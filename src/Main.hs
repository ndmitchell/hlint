
module Main(main) where

import Language.Haskell.HLint3
import Control.Monad
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    errs <- hlint args
    unless (null errs) $
        exitWith $ ExitFailure 1


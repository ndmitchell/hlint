
module Main where

import Language.Haskell.HLint
import Control.Monad
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    errs <- hlint args
    when (length errs > 0) $
        exitWith $ ExitFailure 1


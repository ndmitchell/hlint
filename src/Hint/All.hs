
module Hint.All(readHints) where

import Control.Monad
import Util
import Type

import Hint.Match
import Hint.List
import Hint.Monad
import Hint.Lambda
import Hint.Bracket


readHints :: [FilePath] -> IO Hint
readHints = liftM (concatHints . concat) . mapM readHint


readHint :: FilePath -> IO [Hint]
readHint file = do
    modu <- parseHsModule file
    return [readMatch modu, listHint, monadHint, lambdaHint, bracketHint]

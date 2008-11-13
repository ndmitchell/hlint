
module Hint.Read(readHints) where

import Control.Monad
import Hint.Util
import Hint.Type

import Hint.Match
import Hint.List
import Hint.Monad
import Hint.Lambda


readHints :: [FilePath] -> IO Hint
readHints = liftM (concatHints . concat) . mapM readHint


readHint :: FilePath -> IO [Hint]
readHint file = do
    modu <- parseHsModule file
    return [readMatch modu, listHint, monadHint, lambdaHint]

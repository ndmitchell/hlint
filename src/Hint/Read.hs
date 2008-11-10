
module Hint.Read(readHints) where

import Control.Monad
import Hint.Util
import Hint.Type
import Hint.Match


readHints :: [FilePath] -> IO Hint
readHints = liftM concatHints . mapM readHint


readHint :: FilePath -> IO Hint
readHint = liftM readMatch . parseHsModule

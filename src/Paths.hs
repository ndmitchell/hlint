
module Paths (P.version, getDataDir, P.getDataFileName) where

import qualified Paths_hlint as P
import System.FilePath

getDataDir :: IO FilePath
getDataDir = (</> "data") <$> P.getDataDir

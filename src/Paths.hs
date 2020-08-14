
module Paths (P.version, getDataDir, P.getDataFileName) where

import qualified Paths_hlint as P
import System.FilePath

-- | The data-dir declared in the cabal file is ./, so P.getDataDir returns ./
-- and this function returns ./data.
getDataDir :: IO FilePath
getDataDir = (</> "data") <$> P.getDataDir

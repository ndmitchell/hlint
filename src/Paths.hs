
module Paths_hlint(version, getDataDir) where

import Data.Version.Extra

version :: Version
version = makeVersion [0,0]

getDataDir :: IO FilePath
getDataDir = pure "data"

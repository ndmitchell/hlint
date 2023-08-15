
module Paths_hlint(version, getDataDir) where

import Data.Version.Extra

version :: Version
version = makeVersion [0,0]

{-# ANN module "HLint: ignore Unnecessarily monadic" #-}
getDataDir :: IO FilePath
getDataDir = pure "data"

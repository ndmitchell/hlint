{-|
    I would much rather put this module hidden away in src
    but then Cabal will always use it in preference to the
    auto-generated one.
-}

module Paths_hlint where

import Data.Version.Extra

version :: Version
version =  makeVersion [0,0]

getDataDir :: IO FilePath
getDataDir = return "data"

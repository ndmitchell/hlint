{-|
    I would much rather put this module hidden away in src
    but then Cabal will always use it in preference to the
    auto-generated one.
-}

module Paths_drhaskell where

import Data.Version(Version(..))

version :: Version
version = Version {versionBranch = [0,0], versionTags = []}

getDataDir :: IO FilePath
getDataDir = return "../data"

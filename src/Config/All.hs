
module Config.All(readFileConfig) where

import Config.Type
import Config.Haskell


readFileConfig :: FilePath -> Maybe String -> IO [Setting]
readFileConfig = readFileConfigHaskell

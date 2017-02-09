
module Config.All(readFileConfig) where

import Config.Type
import Config.Haskell
import Config.Yaml
import Data.List.Extra
import System.FilePath


readFileConfig :: FilePath -> Maybe String -> IO [Setting]
readFileConfig file
    | ext `elem` [".yml",".yaml"] = readFileConfigYaml file
    | otherwise = readFileConfigHaskell file
    where ext = lower $ takeExtension file

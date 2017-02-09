
module Config.Yaml(readFileConfigYaml) where

import Config.Type

readFileConfigYaml :: FilePath -> Maybe String -> IO [Setting]
readFileConfigYaml _ _ = return []

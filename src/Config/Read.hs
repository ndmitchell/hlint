
module Config.Read(readFilesConfig) where

import Config.Type
import Config.Haskell
import Config.Yaml
import Data.List.Extra
import System.FilePath


readFilesConfig :: [(FilePath, Maybe String)] -> IO [Setting]
readFilesConfig files = do
        yaml <- mapM (uncurry readFileConfigYaml) yaml
        haskell <- mapM (uncurry readFileConfigHaskell) haskell
        return $ concat haskell ++ settingsFromConfigYaml yaml
    where
        (yaml, haskell) = partition (\(x,_) -> lower (takeExtension x) `elem` [".yml",".yaml"]) files

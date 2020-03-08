
module Config.Read(readFilesConfig) where

import Config.Type
import Control.Monad
import Control.Exception.Extra
import Config.Yaml
import Data.List.Extra
import System.FilePath


readFilesConfig :: [(FilePath, Maybe String)] -> IO [Setting]
readFilesConfig files = do
    let (yaml, haskell) = partition (\(x,_) -> lower (takeExtension x) `elem` [".yml",".yaml"]) files
    unless (null haskell) $
        errorIO $ "HLint 2.3 and beyond cannot use Haskell configuration files.\n" ++
                  "Tried to use: " ++ show haskell ++ "\n" ++
                  "Convert it to .yaml file format, following the example at\n" ++
                  "  <https://github.com/ndmitchell/hlint/blob/master/data/hlint.yaml>"
    yaml <- mapM (uncurry readFileConfigYaml) yaml
    pure $ settingsFromConfigYaml yaml

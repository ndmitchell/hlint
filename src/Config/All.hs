
module Config.All(readFileConfig) where

import Config.Type
import Config.Haskell
import HSE.All


readFileConfig :: FilePath -> Maybe String -> IO [Setting]
readFileConfig file contents = do
    let flags = addInfix defaultParseFlags
    res <- parseModuleEx flags file contents
    case res of
        Left (ParseError sl msg err) ->
            error $ "Config parse failure at " ++ showSrcLoc sl ++ ": " ++ msg ++ "\n" ++ err
        Right (m, _) -> return $ readSettings m

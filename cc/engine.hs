{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import System.Process (callProcess)
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy.Char8 as C8

data Config = Config
    { cIncludePaths :: [FilePath]
    , cExtraFlags :: [String]
    }

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> Config
        <$> o .: "include_paths"
        <*> do
            mc <- o .:? "config"
            mf <- maybe (pure Nothing) (.:? "flags") mc
            pure $ fromMaybe [] mf

main :: IO ()
main = do
    ec <- eitherDecode <$> C8.readFile "/config.json"
    options <- case ec of
        Left ex -> do
            hPutStrLn stderr $ "Ignoring invalid /config.json: " ++ ex
            return ["."]
        Right c -> return $ cExtraFlags c ++ filter include (cIncludePaths c)

    callProcess "hlint" $
        ["--datadir"
        , "/opt/hlint"
        , "--json-cc"
        , "--no-exit-code"
        ] ++ options

  where
    -- Return true for directories as well, so they are included
    include :: FilePath -> Bool
    include p = any (`isSuffixOf` p) ["/", ".hs", ".lhs"]

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
    -- Given a source tree:
    --
    -- > ./
    -- >   a.hs
    -- >   b.hs
    -- >   c/
    -- >     d.hs
    --
    -- And .codeclimate.yml:
    --
    -- > engines:
    -- >   hlint:
    -- >     enabled: true
    -- >     config:
    -- >       flags:
    -- >         - --foo
    -- >         - --bar
    -- >
    -- > exclude_paths:
    -- >   - b.hs
    --
    -- We will find a /config.json like:
    --
    -- > {
    -- >   "include_paths": [
    -- >     "a.hs",
    -- >     "c/"
    -- >   ],
    -- >   "config": {
    -- >     "flags": [
    -- >       "--foo",
    -- >       "--bar",
    -- >     ]
    -- >   }
    -- > }
    --
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
        [ "--datadir"
        , "/opt/hlint"
        , "--json-cc"
        , "--no-exit-code"
        ] ++ options

  where
    include :: FilePath -> Bool
    include p = any (`isSuffixOf` p) ["/", ".hs", ".lhs"]

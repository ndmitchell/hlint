{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

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
    c <- readConfig "/config.json"

    callProcess "hlint" $
        [ "--cc"
        , "--datadir", "/opt/hlint"
        , "--no-exit-code"
        ]
        ++ cExtraFlags c
        ++ filter include (cIncludePaths c)

  where
    include :: FilePath -> Bool
    include p = any (`isSuffixOf` p) ["/", ".hs", ".lhs"]

readConfig :: FilePath -> IO Config
readConfig fp = either err return . eitherDecode =<< C8.readFile fp
  where
    err e = do
        hPutStrLn stderr $ "Ignoring invalid /config.json: " ++ e
        return $ Config ["."] []

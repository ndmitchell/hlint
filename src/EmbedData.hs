{-# LANGUAGE TemplateHaskell #-}

module EmbedData
  ( hlintYaml,
    defaultYaml,
    reportTemplate,
  )
where

import Data.ByteString.UTF8
import Data.FileEmbed

hlintYaml :: (FilePath, Maybe String)
hlintYaml = ("data/hlint.yaml", Just $ toString $(embedFile "data/hlint.yaml"))

defaultYaml :: String
defaultYaml = toString $(embedFile "data/default.yaml")

reportTemplate :: String
reportTemplate = toString $(embedFile "data/report_template.html")

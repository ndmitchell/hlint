{-# LANGUAGE TemplateHaskell #-}

module EmbedData
  ( hlintYaml,
    defaultYaml,
    reportTemplate,
  )
where

import Data.ByteString.UTF8
import Data.FileEmbed

-- Use NOINLINE below to avoid dirtying too much when these files change

{-# NOINLINE hlintYaml #-}
hlintYaml :: (FilePath, Maybe String)
hlintYaml = ("data/hlint.yaml", Just $ toString $(embedFile "data/hlint.yaml"))

{-# NOINLINE defaultYaml #-}
defaultYaml :: String
defaultYaml = toString $(embedFile "data/default.yaml")

{-# NOINLINE reportTemplate #-}
reportTemplate :: String
reportTemplate = toString $(embedFile "data/report_template.html")

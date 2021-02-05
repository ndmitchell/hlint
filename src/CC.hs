{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
--
-- Utility for formatting @'Idea'@ data in accordance with the Code Climate
-- spec: <https://github.com/codeclimate/spec>
--
module CC
    ( printIssue
    , fromIdea
    ) where

import Data.Aeson (ToJSON(..), (.=), encode, object)
import Data.Char (toUpper)
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C8

import Idea (Idea(..), Severity(..))

import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Util as GHC

data Issue = Issue
    { issueType :: Text
    , issueCheckName :: Text
    , issueDescription :: Text
    , issueContent :: Text
    , issueCategories :: [Text]
    , issueLocation :: Location
    , issueRemediationPoints :: Int
    }

data Location = Location FilePath Position Position
data Position = Position Int Int

instance ToJSON Issue where
    toJSON Issue{..} = object
        [ "type" .= issueType
        , "check_name" .= issueCheckName
        , "description" .= issueDescription
        , "content" .= object
            [ "body" .= issueContent
            ]
        , "categories" .= issueCategories
        , "location" .= issueLocation
        , "remediation_points" .= issueRemediationPoints
        ]

instance ToJSON Location where
    toJSON (Location path begin end) = object
        [ "path" .= path
        , "positions" .= object
            [ "begin" .= begin
            , "end" .= end
            ]
        ]

instance ToJSON Position where
    toJSON (Position line column) = object
        [ "line" .= line
        , "column" .= column
        ]

-- | Print an @'Issue'@ with trailing null-terminator and newline
--
-- The trailing newline will be ignored, but makes the output more readable
--
printIssue :: Issue -> IO ()
printIssue = C8.putStrLn . (<> "\0") . encode

-- | Convert an hlint @'Idea'@ to a datatype more easily serialized for CC
fromIdea :: Idea -> Issue
fromIdea Idea{..} = Issue
    { issueType = "issue"
    , issueCheckName = "HLint/" <> T.pack (camelize ideaHint)
    , issueDescription = T.pack ideaHint
    , issueContent = content ideaFrom ideaTo <> listNotes ideaNote
    , issueCategories = categories ideaHint
    , issueLocation = fromSrcSpan ideaSpan
    , issueRemediationPoints = points ideaSeverity
    }

  where
    content from Nothing = T.unlines
        [ "Found"
        , ""
        , "```"
        , T.pack from
        , "```"
        , ""
        , "remove it."
        ]

    content from (Just to) = T.unlines
        [ "Found"
        , ""
        , "```"
        , T.pack from
        , "```"
        , ""
        , "Perhaps"
        , ""
        , "```"
        , T.pack to
        , "```"
        ]

    listNotes [] = ""
    listNotes notes = T.unlines $
        [ ""
        , "Applying this change:"
        , ""
        ] ++ map (("* " <>) . T.pack . show) notes

    categories _ = ["Style"]

    points Ignore = 0
    points Suggestion = basePoints
    points Warning = 5 * basePoints
    points Error = 10 * basePoints

fromSrcSpan :: GHC.SrcSpan -> Location
fromSrcSpan GHC.SrcSpan{..} = Location
    (locationFileName srcSpanFilename)
    (Position srcSpanStartLine' srcSpanStartColumn)
    (Position srcSpanEndLine' srcSpanEndColumn)
  where
    locationFileName ('.':'/':x) = x
    locationFileName x = x

camelize :: String -> String
camelize = concatMap capitalize . words

capitalize :: String -> String
capitalize [] = []
capitalize (c:rest) = toUpper c : rest

-- "The baseline remediation points value is 50,000, which is the time it takes
-- to fix a trivial code style issue like a missing semicolon on a single line,
-- including the time for the developer to open the code, make the change, and
-- confidently commit the fix. All other remediation points values are expressed
-- in multiples of that Basic Remediation Point Value."
basePoints :: Int
basePoints = 50000

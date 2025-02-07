{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Description: Formats hlint ideas in the Statis Analysis Results Interchange Format (SARIF).
License: BSD-3-Clause

Supports the conversion of a list of HLint 'Idea's into SARIF.

SARIF (Static Analysis Results Interchange Format) is an open interchange format
for storing results from static analyses.
-}
module SARIF ( printIdeas
             , showIdeas
             , toJSONEncoding
             -- * See also
             --
             -- $references
             ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Encoding
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Text.Lazy (Text)
import Data.Version (showVersion)
import GHC.Util
import Idea
import Paths_hlint (version)

-- | Print the given ideas to standard output.
--
-- For example:
--
-- >>> hlint ["src"] >>= printIdeas
--
-- For printing ideas in SARIF without dependent modules
-- having to import "Data.Aeson" or "Data.ByteString.Lazy".
printIdeas :: [Idea] -> IO ()
printIdeas = B.putStr . showIdeas

-- | Format the given ideas in SARIF.
--
-- For converting ideas to SARIF without dependent modules
-- having to import "Data.Aeson".
showIdeas :: [Idea] -> ByteString
showIdeas = encodingToLazyByteString . toJSONEncoding

-- | Converts the given ideas to a "Data.Aeson" encoding in SARIF.
toJSONEncoding :: [Idea] -> Encoding
toJSONEncoding = pairs . sarif

-- | Converts the given object to a top-level @sarifLog@ object.
--
-- See section 3.13 "sarifLog object", SARIF specification.
sarif :: [Idea] -> Series
sarif ideas =
  pair "version" (lazyText "2.1.0") <>
  pair "$schema" (lazyText schemaURI) <>
  pair "runs" runs
  where runs = list pairs [ pair "tool" (pairs tool) <>
                            pair "results" (list (pairs . toResult) ideas) ]

-- | A @tool@ object describing what created the output.
--
-- Obviously, it will describe that HLint created the output.
--
-- See section 3.18 "tool object", SARIF specification.
tool :: Series
tool = pair "driver" $ pairs $
  pair "name" (lazyText "hlint") <>
  pair "version" (string $ showVersion version) <>
  pair "informationUri" (lazyText hlintURI)

-- | Converts a given idea into a @result@ object.
--
-- It will describe the hint, the severity, suggestions for fixes, etc.
--
-- See section 3.27 "result object", SARIF specification.
toResult :: Idea -> Series
toResult idea@Idea{..} =
  pair "message" (pairs $ pair "text" $ string $ show idea) <>
  pair "level" (lazyText $ showSeverity ideaSeverity) <>
  pair "locations" (list (pairs . toLocation) [idea]) <>
  pair "fixes" (list (pairs . toFix) [idea]) <>
  -- Use 'ideaHint' as the rule identifier.
  --
  -- "ruleId" is supposed to a stable, opaque identifier.
  -- 'ideaHint' is not opaque, nor is it quite guaranteed to be stable,
  -- but they will usually be stable enough, and disabling a hint is
  -- based on the name in 'ideaHint'.
  --
  -- Most importantly, there is no requirement that "ruleId"
  -- be a /unique/ identifier.
  pair "ruleId" (string ideaHint)

-- | Convert HLint severity to SARIF level.
--
-- See section 3.58.6 "level property", SARIF specification.
showSeverity :: Severity -> Text
showSeverity Error = "error"
showSeverity Warning = "warning"
showSeverity Suggestion = "note"
showSeverity Ignore = "none"

-- | Converts the location information in a given idea to a @location@ object.
--
-- See section 3.28 "location object", SARIF specification.
toLocation :: Idea -> Series
toLocation idea@Idea{ideaSpan=SrcSpan{..}, ..} =
  physicalLocation <> logicalLocations ideaModule ideaDecl
  where physicalLocation = pair "physicalLocation" $ pairs $
          pair "artifactLocation"
              (pairs $ pair "uri" (string srcSpanFilename)) <>
          pair "region" (pairs $ toRegion idea)

        logicalLocations [mod] [decl] = pair "logicalLocations" $
          list pairs [ pair "name" (string decl) <>
                       pair "fullyQualifiedName" (string $ mod ++ "." ++ decl) ]
          -- It would be nice to include whether it is a function or type
          -- in the "kind" field, but we do not have that information.

        -- If the lists are empty, then there is obviously no logical location.
        -- Logical location is still omitted when the lists are not singleton,
        -- because the associations between modules and declarations are
        -- not clear.
        logicalLocations _ _ = mempty

-- | Converts a given idea to a @fix@ object.
--
-- It will suggest how code can be improved to deal with an issue.
-- This includes the file to be changed and how to change it.
--
-- See section 3.55 "fix object", SARIF specification.
toFix :: Idea -> Series
toFix idea@Idea{..} =
  pair "description" (pairs $ pair "text" $ string ideaHint) <>
  pair "artifactChanges" (list (pairs . toChange) [idea])

-- | Converts a given idea to a @artifactChange@ object.
--
-- It will describe the details as to how the code can be changed.
-- I.e., the text to remove and what it should be replaced with.
--
-- See section 3.56 "artifactChange object", SARIF specification.
toChange :: Idea -> Series
toChange idea@Idea{ideaSpan=SrcSpan{..}, ..} =
  pair "artifactLocation" (pairs uri) <>
  pair "replacements" (list pairs [deleted <> inserted])
  where uri  = pair "uri" $ string srcSpanFilename
        deleted = pair "deletedRegion" $ pairs $ toRegion idea
        inserted = maybe mempty insertedContent ideaTo
        insertedContent = pair "insertedContent" . pairs . pair "text" . string

-- | Converts the source span in an idea to a SARIF region.
--
-- See 3.30 "region object", SARIF specification.
toRegion :: Idea -> Series
toRegion Idea{ideaSpan=SrcSpan{..}, ..} =
  pair "startLine" (int srcSpanStartLine') <>
  pair "startColumn" (int srcSpanStartColumn) <>
  pair "endLine" (int srcSpanEndLine') <>
  pair "endColumn" (int srcSpanEndColumn)

-- | URI to SARIF schema definition.
schemaURI :: Text
schemaURI = "https://raw.githubusercontent.com/" <>
            "oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json"

-- | URI to HLint home page.
hlintURI :: Text
hlintURI = "https://github.com/ndmitchell/hlint"

-- $references
--
-- * [SARIF Tutorials](https://github.com/microsoft/sarif-tutorials)
-- * [Static Analysis Results Interchange Format](https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html), version 2.1.0

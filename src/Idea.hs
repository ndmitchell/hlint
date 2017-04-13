{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction #-}

module Idea(
    Idea(..),
    rawIdea, idea, suggest, warn, ignore,
    rawIdeaN, suggestN,
    showIdeasJson, showANSI,
    Note(..), showNotes,
    Severity(..)
    ) where

import Data.List.Extra
import Data.Char
import Numeric
import HSE.All
import Config.Type
import HsColour
import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R


-- | An idea suggest by a 'Hint'.
data Idea = Idea
    {ideaModule :: String -- ^ The module the idea applies to, may be @\"\"@ if the module cannot be determined or is a result of cross-module hints.
    ,ideaDecl :: String -- ^ The declaration the idea applies to, typically the function name, but may be a type name.
    ,ideaSeverity :: Severity -- ^ The severity of the idea, e.g. 'Warning'.
    ,ideaHint :: String -- ^ The name of the hint that generated the idea, e.g. @\"Use reverse\"@.
    ,ideaSpan :: SrcSpan -- ^ The source code the idea relates to.
    ,ideaFrom :: String -- ^ The contents of the source code the idea relates to.
    ,ideaTo :: Maybe String -- ^ The suggested replacement, or 'Nothing' for no replacement (e.g. on parse errors).
    ,ideaNote :: [Note] -- ^ Notes about the effect of applying the replacement.
    ,ideaRefactoring :: [Refactoring R.SrcSpan] -- ^ How to perform this idea
    }
    deriving (Eq,Ord)

-- I don't use aeson here for 2 reasons:
-- 1) Aeson doesn't esape unicode characters, and I want to (allows me to ignore encoding)
-- 2) I want to control the format so it's slightly human readable as well
showIdeaJson :: Idea -> String
showIdeaJson idea@Idea{ideaSpan=srcSpan@SrcSpan{..}, ..} = wrap . intercalate "," . map mkPair $
    [("module", str ideaModule)
    ,("decl", str ideaDecl)
    ,("severity", str $ show ideaSeverity)
    ,("hint", str ideaHint)
    ,("file", str srcSpanFilename)
    ,("startLine", show srcSpanStartLine)
    ,("startColumn", show srcSpanStartColumn)
    ,("endLine", show srcSpanEndLine)
    ,("endColumn", show srcSpanEndColumn)
    ,("from", str ideaFrom)
    ,("to", maybe "null" str ideaTo)
    ,("note", "[" ++ intercalate "," (map (str . show) ideaNote) ++ "]")
    ,("refactorings", str $ show ideaRefactoring)
    ]
  where
    str x = "\"" ++ concatMap f x ++ "\""
        where f '\"' = "\\\""
              f '\\' = "\\\\"
              f '\n' = "\\n"
              f '\r' = "\\r"
              f x | isControl x || not (isAscii x) = "\\u" ++ takeEnd 4 ("0000" ++ showHex (ord x) "")
              f x = [x]
    mkPair (k, v) = show k ++ ":" ++ v
    wrap x = "{" ++ x ++ "}"

showIdeasJson :: [Idea] -> String
showIdeasJson ideas = "[" ++ intercalate "\n," (map showIdeaJson ideas) ++ "]"

instance Show Idea where
    show = showEx id


showANSI :: IO (Idea -> String)
showANSI = do
    f <- hsColourConsole
    return $ showEx f

showEx :: (String -> String) -> Idea -> String
showEx tt Idea{..} = unlines $
    [showSrcLoc (getPointLoc ideaSpan) ++ ": " ++ (if ideaHint == "" then "" else show ideaSeverity ++ ": " ++ ideaHint)] ++
    f "Found" (Just ideaFrom) ++ f "Why not" ideaTo ++
    ["Note: " ++ n | let n = showNotes ideaNote, n /= ""]
    where
        f msg Nothing = []
        f msg (Just x) | null xs = [msg ++ " remove it."]
                       | otherwise = (msg ++ ":") : map ("  "++) xs
            where xs = lines $ tt x


rawIdea = Idea "" ""
rawIdeaN a b c d e f = Idea "" "" a b c d e f []

idea severity hint from to = rawIdea severity hint (srcInfoSpan $ ann from) (f from) (Just $ f to) []
    where f = trimStart . prettyPrint

suggest = idea Suggestion
warn = idea Warning
ignore = idea Ignore

ideaN severity hint from to = idea severity hint from to []

suggestN = ideaN Suggestion

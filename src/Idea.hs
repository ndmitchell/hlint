{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction #-}

module Idea(
    Idea(..),
    rawIdea, idea, suggest, suggestRemove, ideaRemove, warn, ignore,
    rawIdeaN, suggestN, ignoreNoSuggestion,
    showIdeasJson, showIdeaANSI,
    Note(..), showNotes,
    Severity(..),
    ) where

import Data.List.Extra
import Config.Type
import HsColour
import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R
import Prelude
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Util

import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

-- | An idea suggest by a 'Hint'.
data Idea = Idea
    {ideaModule :: [String] -- ^ The modules the idea is for, usually a singleton.
    ,ideaDecl :: [String] -- ^ The declarations the idea is for, usually a singleton, typically the function name, but may be a type name.
    ,ideaSeverity :: Severity -- ^ The severity of the idea, e.g. 'Warning'.
    ,ideaHint :: String -- ^ The name of the hint that generated the idea, e.g. @\"Use reverse\"@.
    ,ideaSpan :: SrcSpan -- ^ The source code the idea relates to.
    ,ideaFrom :: String -- ^ The contents of the source code the idea relates to.
    ,ideaTo :: Maybe String -- ^ The suggested replacement, or 'Nothing' for no replacement (e.g. on parse errors).
    ,ideaNote :: [Note] -- ^ Notes about the effect of applying the replacement.
    ,ideaRefactoring :: [Refactoring R.SrcSpan] -- ^ How to perform this idea
    }
    deriving Eq

-- I don't use aeson here for 2 reasons:
-- 1) Aeson doesn't esape unicode characters, and I want to (allows me to ignore encoding)
-- 2) I want to control the format so it's slightly human readable as well
showIdeaJson :: Idea -> String
showIdeaJson idea@Idea{ideaSpan=srcSpan@SrcSpan{..}, ..} = dict
    [("module", list $ map str ideaModule)
    ,("decl", list $ map str ideaDecl)
    ,("severity", str $ show ideaSeverity)
    ,("hint", str ideaHint)
    ,("file", str srcSpanFilename)
    ,("startLine", show srcSpanStartLine')
    ,("startColumn", show srcSpanStartColumn)
    ,("endLine", show srcSpanEndLine')
    ,("endColumn", show srcSpanEndColumn)
    ,("from", str ideaFrom)
    ,("to", maybe "null" str ideaTo)
    ,("note", list (map (str . show) ideaNote))
    ,("refactorings", str $ show ideaRefactoring)
    ]
  where
    str x = "\"" ++ escapeJSON x ++ "\""
    dict xs = "{" ++ intercalate "," [show k ++ ":" ++ v | (k,v) <- xs] ++ "}"
    list xs = "[" ++ intercalate "," xs ++ "]"

-- | Show a list of 'Idea' values as a JSON string.
showIdeasJson :: [Idea] -> String
showIdeasJson ideas = "[" ++ intercalate "\n," (map showIdeaJson ideas) ++ "]"

instance Show Idea where
    show = showEx id


-- | Show an 'Idea' with ANSI color codes to give syntax coloring to the Haskell code.
showIdeaANSI :: Idea -> String
showIdeaANSI = showEx hsColourConsole

showEx :: (String -> String) -> Idea -> String
showEx tt Idea{..} = unlines $
    [showSrcSpan ideaSpan ++ ": " ++ (if ideaHint == "" then "" else show ideaSeverity ++ ": " ++ ideaHint)] ++
    f "Found" (Just ideaFrom) ++ f "Perhaps" ideaTo ++
    ["Note: " ++ n | let n = showNotes ideaNote, n /= ""]
    where
        f msg Nothing = []
        f msg (Just x) | null xs = [msg ++ " you should remove it."]
                       | otherwise = (msg ++ ":") : map ("  "++) xs
            where xs = lines $ tt x


rawIdea :: Severity -> String -> SrcSpan -> String -> Maybe String -> [Note]-> [Refactoring R.SrcSpan] -> Idea
rawIdea = Idea [] []

rawIdeaN :: Severity -> String -> SrcSpan -> String -> Maybe String -> [Note] -> Idea
rawIdeaN a b c d e f = Idea [] [] a b c d e f []

idea :: (GHC.Utils.Outputable.Outputable a, GHC.Utils.Outputable.Outputable b) =>
         Severity -> String -> Located a -> Located b -> [Refactoring R.SrcSpan] -> Idea
idea severity hint from to =
  rawIdea severity hint (getLoc from) (unsafePrettyPrint from) (Just $ unsafePrettyPrint to) []

-- Construct an Idea that suggests "Perhaps you should remove it."
ideaRemove :: Severity -> String -> SrcSpan -> String -> [Refactoring R.SrcSpan] -> Idea
ideaRemove severity hint span from = rawIdea severity hint span from (Just "") []

suggest :: (GHC.Utils.Outputable.Outputable a, GHC.Utils.Outputable.Outputable b) =>
            String -> Located a -> Located b -> [Refactoring R.SrcSpan] -> Idea
suggest = idea Suggestion

suggestRemove :: String -> SrcSpan -> String -> [Refactoring R.SrcSpan] -> Idea
suggestRemove = ideaRemove Suggestion

warn :: (GHC.Utils.Outputable.Outputable a, GHC.Utils.Outputable.Outputable b) =>
         String -> Located a -> Located b -> [Refactoring R.SrcSpan] -> Idea
warn = idea Warning

ignoreNoSuggestion :: (GHC.Utils.Outputable.Outputable a)
                    => String -> Located a -> Idea
ignoreNoSuggestion hint x = rawIdeaN Ignore hint (getLoc x) (unsafePrettyPrint x) Nothing []

ignore :: (GHC.Utils.Outputable.Outputable a) =>
           String -> Located a -> Located a -> [Refactoring R.SrcSpan] -> Idea
ignore = idea Ignore

ideaN :: (GHC.Utils.Outputable.Outputable a) =>
          Severity -> String -> Located a -> Located a -> Idea
ideaN severity hint from to = idea severity hint from to []

suggestN :: (GHC.Utils.Outputable.Outputable a) =>
             String -> Located a -> Located a -> Idea
suggestN = ideaN Suggestion

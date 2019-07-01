{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports #-}

module Idea(
    Idea(..),
    rawIdea, idea, idea', suggest, warn, ignore, ignore',
    rawIdeaN, suggestN, suggestN', ignoreN,
    showIdeasJson, showANSI,
    Note(..), showNotes,
    Severity(..)
    ) where

import Data.Functor
import Data.List.Extra
import HSE.All
import Config.Type
import HsColour
import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R
import Prelude
import qualified "ghc-lib-parser" SrcLoc as GHC
import qualified "ghc-lib-parser" Outputable

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
    deriving (Eq,Ord)

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
    ,("startLine", show srcSpanStartLine)
    ,("startColumn", show srcSpanStartColumn)
    ,("endLine", show srcSpanEndLine)
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

showIdeasJson :: [Idea] -> String
showIdeasJson ideas = "[" ++ intercalate "\n," (map showIdeaJson ideas) ++ "]"

instance Show Idea where
    show = showEx id


showANSI :: IO (Idea -> String)
showANSI = showEx <$> hsColourConsole

showEx :: (String -> String) -> Idea -> String
showEx tt Idea{..} = unlines $
    [showSrcLoc (getPointLoc ideaSpan) ++ ": " ++ (if ideaHint == "" then "" else show ideaSeverity ++ ": " ++ ideaHint)] ++
    f "Found" (Just ideaFrom) ++ f "Perhaps" ideaTo ++
    ["Note: " ++ n | let n = showNotes ideaNote, n /= ""]
    where
        f msg Nothing = []
        f msg (Just x) | null xs = [msg ++ " you should remove it."]
                       | otherwise = (msg ++ ":") : map ("  "++) xs
            where xs = lines $ tt x


rawIdea = Idea [] []
rawIdeaN a b c d e f = Idea [] [] a b c d e f []

idea severity hint from to = rawIdea severity hint (srcInfoSpan $ ann from) (f from) (Just $ f to) []
    where f = trimStart . prettyPrint

idea' :: (GHC.HasSrcSpan a, Outputable.Outputable a) =>
         Severity -> String -> a -> a -> [Refactoring R.SrcSpan] -> Idea
idea' severity hint from to =
  rawIdea severity hint (ghcSpanToHSE (GHC.getLoc from)) (f from) (Just $ f to) []
  where
    f = Outputable.showSDocUnsafe . Outputable.ppr

suggest = idea Suggestion
warn = idea Warning
ignore = idea Ignore
ignore' :: (GHC.HasSrcSpan a, Outputable.Outputable a) =>
           String -> a -> a -> [Refactoring R.SrcSpan] -> Idea
ignore' = idea' Ignore

ideaN severity hint from to = idea severity hint from to []
ideaN' :: (GHC.HasSrcSpan a, Outputable.Outputable a) =>
          Severity -> String -> a -> a -> Idea
ideaN' severity hint from to = idea' severity hint from to []

suggestN = ideaN Suggestion
suggestN' :: (GHC.HasSrcSpan a, Outputable.Outputable a) =>
             String -> a -> a -> Idea
suggestN' = ideaN' Suggestion
ignoreN = ideaN Ignore

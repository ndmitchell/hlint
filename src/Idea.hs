{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction #-}

module Idea(module Idea, Note(..), showNotes, Severity(..)) where

import Data.List
import HSE.All
import Settings
import Language.Haskell.HsColour.TTY
import Language.Haskell.HsColour.Colourise
import Util


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
    }
    deriving (Eq,Ord)

showIdeaJson :: Idea -> String
showIdeaJson idea@Idea{ideaSpan=srcSpan@SrcSpan{..}, ..} = wrap . intercalate "," . map mkPair $
    [("module", show ideaModule)
    ,("decl", show ideaDecl)
    ,("severity", show . show $ ideaSeverity)
    ,("hint", show ideaHint)
    ,("file", show srcSpanFilename)
    ,("startLine", show srcSpanStartLine)
    ,("startColumn", show srcSpanStartColumn)
    ,("endLine", show srcSpanEndLine)
    ,("endColumn", show srcSpanEndColumn)
    ,("from", show ideaFrom)
    ,("to", maybe "null" show ideaTo)
    ,("note", show $ map (show . show) ideaNote)
    ]
  where
    mkPair (k, v) = show k ++ ":" ++ v
    wrap x = "{" ++ x ++ "}"

showIdeasJson :: [Idea] -> String
showIdeasJson ideas = "[" ++ intercalate "," (map showIdeaJson ideas) ++ "]"

instance Show Idea where
    show = showEx id


showANSI :: IO (Idea -> String)
showANSI = do
    prefs <- readColourPrefs
    return $ showEx (hscolour prefs)

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
idea severity hint from to = rawIdea severity hint (toSrcSpan $ ann from) (f from) (Just $ f to) []
    where f = ltrim . prettyPrint
warn = idea Warning
err = idea Error

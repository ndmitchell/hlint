{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction #-}

module Idea(module Idea, Note(..), showNotes, Severity(..)) where

import Data.List.Extra
import HSE.All
import Settings
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
    , ideaRefactoring :: Maybe (Refactoring R.SrcSpan) -- ^ How to perform this idea
--    , ideaPreciseSpan :: SrcSpan -- ^ The source code that needs to be replaced
--    , ideaSubst :: Maybe [(String, SrcSpan)] -- ^ The performed substitution
--    , ideaTemplate :: Maybe String -- ^ The template used to generate the output
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


rawIdea a b c d e f = Idea "" "" a b c d e f Nothing
rawRefactorIdea = Idea "" ""
idea' severity hint from to subst fromE = (idea severity hint from to)
                                            {
                                              ideaRefactoring = Just (refactReplace Expr from subst fromE)
                                            }
idea severity hint from to = rawIdea severity hint (toSrcSpan $ ann from) (f from) (Just $ f to) []
    where f = trimStart . prettyPrint
warn = idea Warning
err = idea Error

warn' = idea' Warning
err' = idea' Error

preciseIdea severity hint from to subst template replexp typ =
  (idea severity hint from to)
    { ideaRefactoring = Just (refactReplace typ replexp subst template ) }

preciseWarn = preciseIdea Warning
preciseErr  = preciseIdea Error


refactReplace :: Annotated a => RType -> a S -> [(String, a S)] -> String -> Refactoring R.SrcSpan
refactReplace typ ss subt template =
  Replace typ (f ss) (map (fmap f) subt) template
  where
    f = toRefactSrcSpan . toSrcSpan . ann

toRefactSrcSpan :: SrcSpan -> R.SrcSpan
toRefactSrcSpan ss = R.SrcSpan (srcSpanStart ss) (srcSpanEnd ss)

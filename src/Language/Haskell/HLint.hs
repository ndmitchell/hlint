{-|
/WARNING: This module represents the old version of the HLint API./
/It will be deleted in favour of "Language.Haskell.HLint3" in the next major version./

This module provides a library interface to HLint, strongly modelled on the command line interface.
-}

module Language.Haskell.HLint(hlint, Suggestion, suggestionLocation, suggestionSeverity, Severity(..)) where

import qualified HLint
import Config.Type
import Idea
import HSE.All


-- | This function takes a list of command line arguments, and returns the given suggestions.
--   To see a list of arguments type @hlint --help@ at the console.
--   This function writes to the stdout/stderr streams, unless @--quiet@ is specified.
--
--   As an example:
--
-- > do hints <- hlint ["src", "--ignore=Use map","--quiet"]
-- >    when (length hints > 3) $ error "Too many hints!"
hlint :: [String] -> IO [Suggestion]
hlint = fmap (map Suggestion_) . HLint.hlint



-- | A suggestion - the @Show@ instance is of particular use.
newtype Suggestion = Suggestion_ {fromSuggestion :: Idea}
                     deriving (Eq,Ord)

instance Show Suggestion where
    show = show . fromSuggestion

-- | From a suggestion, extract the file location it refers to.
suggestionLocation :: Suggestion -> SrcLoc
suggestionLocation = getPointLoc . ideaSpan . fromSuggestion


-- | From a suggestion, determine how severe it is.
suggestionSeverity :: Suggestion -> Severity
suggestionSeverity = ideaSeverity . fromSuggestion

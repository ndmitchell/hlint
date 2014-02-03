{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction #-}

module Idea(module Idea, Note(..), showNotes, Severity(..)) where

import HSE.All
import Settings
import Language.Haskell.HsColour.TTY
import Language.Haskell.HsColour.Colourise
import Util


data Idea
    = Idea {ideaModule :: String, ideaDecl :: String, severity :: Severity, hint :: String, loc :: SrcLoc, from :: String, to :: Maybe String, note :: [Note]}
      deriving (Eq,Ord)


instance Show Idea where
    show = showEx id


showANSI :: IO (Idea -> String)
showANSI = do
    prefs <- readColourPrefs
    return $ showEx (hscolour prefs)

showEx :: (String -> String) -> Idea -> String
showEx tt Idea{..} = unlines $
    [showSrcLoc loc ++ ": " ++ show severity ++ ": " ++ hint] ++
    f "Found" (Just from) ++ f "Why not" to ++
    ["Note: " ++ n | let n = showNotes note, n /= ""]
    where
        f msg Nothing = []
        f msg (Just x) | null xs = [msg ++ " remove it."]
                       | otherwise = (msg ++ ":") : map ("  "++) xs
            where xs = lines $ tt x


rawIdea = Idea "" ""
idea severity hint from to = rawIdea severity hint (toSrcLoc $ ann from) (f from) (Just $ f to) []
    where f = ltrim . prettyPrint
warn = idea Warning
err = idea Error

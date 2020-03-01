{-# LANGUAGE CPP #-}
module HsColour(hsColourHTML, hsColourConsole) where

#ifdef GPL_SCARES_ME

hsColourConsole :: IO (String -> String)
hsColourConsole = pure id

hsColourHTML :: String -> String
hsColourHTML = id

#else

import Data.Functor
import Prelude

import Language.Haskell.HsColour.TTY as TTY
import Language.Haskell.HsColour.Colourise
import Language.Haskell.HsColour.CSS as CSS


hsColourConsole :: IO (String -> String)
hsColourConsole = TTY.hscolour <$> readColourPrefs

hsColourHTML :: String -> String
hsColourHTML = CSS.hscolour False 1

#endif

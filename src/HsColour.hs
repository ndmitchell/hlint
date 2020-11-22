{-# LANGUAGE CPP #-}
module HsColour(hsColourHTML, hsColourConsole) where

#ifdef GPL_SCARES_ME

hsColourConsole :: String -> String
hsColourConsole = id

hsColourHTML :: String -> String
hsColourHTML = id

#else

import Prelude

import Language.Haskell.HsColour.TTY as TTY
import Language.Haskell.HsColour.Colourise
import Language.Haskell.HsColour.CSS as CSS

hsColourConsole :: String -> String
hsColourConsole = TTY.hscolour defaultColourPrefs

hsColourHTML :: String -> String
hsColourHTML = CSS.hscolour False 1

#endif

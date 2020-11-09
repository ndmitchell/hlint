{-# LANGUAGE CPP #-}
module HsColour(hsColourHTML, hsColourConsole, hsColourConsolePure) where

#ifdef GPL_SCARES_ME

hsColourConsole :: IO (String -> String)
hsColourConsole = pure id

hsColourConsolePure :: String -> String
hsColourConsolePure = id

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

hsColourConsolePure :: String -> String
hsColourConsolePure = TTY.hscolour defaultColourPrefs

hsColourHTML :: String -> String
hsColourHTML = CSS.hscolour False 1

#endif

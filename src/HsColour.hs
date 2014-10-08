
module HsColour(hsColourHTML, hsColourConsole) where

import Language.Haskell.HsColour.TTY as TTY
import Language.Haskell.HsColour.Colourise
import Language.Haskell.HsColour.CSS as CSS


hsColourConsole :: IO (String -> String)
hsColourConsole = do
    prefs <- readColourPrefs
    return $ TTY.hscolour prefs

hsColourHTML :: String -> String
hsColourHTML = CSS.hscolour False

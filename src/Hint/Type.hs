
module Hint.Type where

import Language.Haskell.Exts


data Hint = Hint {hint :: String, loc :: SrcLoc}
            deriving Eq

instance Show Hint where
    show x = showSrcLoc (loc x) ++ " " ++ hint x


showSrcLoc :: SrcLoc -> String
showSrcLoc (SrcLoc file line col) = file ++ ":" ++ show line ++ ":" ++ show col ++ ":"



module Hint.Type where

import Language.Haskell.Exts


data Idea = Idea {idea :: String, loc :: SrcLoc}
            deriving Eq

instance Show Idea where
    show x = showSrcLoc (loc x) ++ " " ++ idea x


showSrcLoc :: SrcLoc -> String
showSrcLoc (SrcLoc file line col) = file ++ ":" ++ show line ++ ":" ++ show col ++ ":"



type Hint = HsDecl -> [Idea]

concatHints :: [Hint] -> Hint
concatHints hs x = concatMap ($x) hs

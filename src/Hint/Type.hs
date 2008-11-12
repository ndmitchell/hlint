
module Hint.Type where

import Language.Haskell.Exts
import Hint.Util


data Idea = Idea {idea :: String, loc :: SrcLoc}
            deriving Eq

instance Show Idea where
    show x = showSrcLoc (loc x) ++ " " ++ idea x


type Hint = HsDecl -> [Idea]


concatHints :: [Hint] -> Hint
concatHints hs x = concatMap ($x) hs


applyHint :: Hint -> HsModule -> [Idea]
applyHint h (HsModule _ _ _ _ xs) = concatMap h xs

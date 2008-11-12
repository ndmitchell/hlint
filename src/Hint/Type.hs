
module Hint.Type where

import Language.Haskell.Exts
import Hint.Util


data Idea = Idea {idea :: String, loc :: SrcLoc, from :: Maybe String, to :: Maybe String}
            deriving Eq

nullIdea = Idea "" nullSrcLoc Nothing Nothing


instance Show Idea where
    show x = unlines $
        [showSrcLoc (loc x) ++ " " ++ idea x] ++ f "Found" from ++ f "Why not" to
        where
            f msg sel = maybe [] (\y -> (msg ++ ":") : map ("  "++) (lines y)) (sel x)



type Hint = HsDecl -> [Idea]


concatHints :: [Hint] -> Hint
concatHints hs x = concatMap ($x) hs


applyHint :: Hint -> HsModule -> [Idea]
applyHint h (HsModule _ _ _ _ xs) = concatMap h xs

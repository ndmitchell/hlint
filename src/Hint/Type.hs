
module Hint.Type where

import Language.Haskell.Exts
import Hint.Util


data Idea = Idea {text :: String, loc :: SrcLoc, from :: Maybe String, to :: Maybe String}
            deriving Eq

idea s loc from to = Idea s loc (Just $ prettyPrint from) (Just $ prettyPrint to)


instance Show Idea where
    show x = unlines $
        [showSrcLoc (loc x) ++ " " ++ text x] ++ f "Found" from ++ f "Why not" to
        where
            f msg sel = maybe [] (\y -> (msg ++ ":") : map ("  "++) (lines y)) (sel x)



type Hint = HsDecl -> [Idea]


concatHints :: [Hint] -> Hint
concatHints hs x = concatMap ($x) hs


applyHint :: Hint -> HsModule -> [Idea]
applyHint h (HsModule _ _ _ _ xs) = concatMap h xs

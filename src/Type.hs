
module Type where

import HSE.All
import Data.List
import Data.Maybe
import Data.Ord


---------------------------------------------------------------------
-- SETTINGS

data Rank = Skip | Warn | Error
            deriving (Eq,Ord,Show)

-- (modulename,functionname)
-- either being blank implies universal matching
type FuncName = (String,String)

data Setting = Classify Rank String FuncName
             | Hint {message :: String, rank :: Rank, lhs :: Exp, rhs :: Exp, side :: Maybe Exp} -- lhs rhs side-cond


isClassify Classify{} = True; isClassify _ = False


instance Show Setting where
    show (Hint x _ y z q) = unlines $ ("Hint " ++ show x) :
        map (\x -> "  " ++ prettyPrint x) ([y,z] ++ maybeToList q)

    showList = showString . concatMap show


---------------------------------------------------------------------
-- IDEAS

-- Key is Data.List.split, for example
data Idea = Idea {key :: String, text :: String, loc :: SrcLoc, from :: String, to :: String}
            deriving Eq

idea s loc from to = Idea "" s loc (prettyPrint from) (prettyPrint to)


instance Show Idea where
    show x = unlines $
        [showSrcLoc (loc x) ++ " " ++ text x] ++ f "Found" from ++ f "Why not" to
        where f msg sel = (msg ++ ":") : map ("  "++) (lines $ sel x)


---------------------------------------------------------------------
-- HINTS

type Hint = Decl -> [Idea]


concatHints :: [Hint] -> Hint
concatHints hs x = concatMap ($x) hs


applyHint :: Hint -> Module -> [Idea]
applyHint h m = [i{key = name ++ ['.'|name/=""] ++ declName d}
                | d <- moduleDecls m, i <- sortBy (comparing loc) $ h d]
    where name = moduleName m

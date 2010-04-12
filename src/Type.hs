{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction #-}

module Type where

import HSE.All
import Data.Char
import Language.Haskell.HsColour.TTY
import Language.Haskell.HsColour.Colourise
import Util


---------------------------------------------------------------------
-- GENERAL DATA TYPES

data Rank = Ignore | Warning | Error
            deriving (Eq,Ord,Show)

-- (modulename,functionname)
-- either being blank implies universal matching
type FuncName = (String,String)


-- Any 1-letter variable names are assumed to be unification variables
isUnifyVar :: String -> Bool
isUnifyVar [x] = x == '?' || isAlpha x
isUnifyVar _ = False


---------------------------------------------------------------------
-- SETTINGS - things read from settings files/command line

data Setting
    = Classify {rankS :: Rank, hintS :: String, funcS :: FuncName}
    | MatchExp {rankS :: Rank, hintS :: String, lhs :: Exp_, rhs :: Exp_, side :: Maybe Exp_}
    | Builtin String -- use a builtin hint set
    | Infix Fixity
      deriving Show

isClassify Classify{} = True; isClassify _ = False
isMatchExp MatchExp{} = True; isMatchExp _ = False


---------------------------------------------------------------------
-- IDEAS - generated hints

data Idea
    = Idea {func :: FuncName, rank :: Rank, hint :: String, loc :: SrcLoc, from :: String, to :: String}
    | ParseError {rank :: Rank, hint :: String, loc :: SrcLoc, msg :: String, from :: String}
      deriving Eq


isParseError ParseError{} = True; isParseError _ = False


instance Show Idea where
    show = showEx id


showANSI :: IO (Idea -> String)
showANSI = do
    prefs <- readColourPrefs
    return $ showEx (hscolour prefs)

showEx :: (String -> String) -> Idea -> String
showEx tt Idea{..} = unlines $
    [showSrcLoc loc ++ " " ++ show rank ++ ": " ++ hint] ++ f "Found" from ++ f "Why not" to
    where f msg x = (msg ++ ":") : map ("  "++) (lines $ tt x)

showEx tt ParseError{..} = unlines $
    [showSrcLoc loc ++ " Parse error","Error message:","  " ++ msg,"Code:"] ++ map ("  "++) (lines $ tt from)


rawIdea = Idea ("","")
idea rank hint from to = rawIdea rank hint (toSrcLoc $ ann from) (f from) (f to)
    where f = ltrim . prettyPrint
warn = idea Warning
err = idea Error

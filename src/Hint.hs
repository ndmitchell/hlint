
module Hint where

import HSE.All
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Settings
import Type
import Util


type DeclHint = NameMatch -> Module_ -> Decl_ -> [Idea]
type ModuHint = NameMatch -> Module_          -> [Idea]

data Hint = DeclHint {declHint :: DeclHint} | ModuHint {moduHint :: ModuHint}


applyHint :: ParseFlags -> [Hint] -> [Setting] -> FilePath -> IO [Idea]
applyHint flags h s file = do
    src <- readFileEncoding (encoding flags) file
    applyHintStr flags h s file src


applyHintStr :: ParseFlags -> [Hint] -> [Setting] -> FilePath -> String -> IO [Idea]
applyHintStr flags h s file src = do
    res <- parseString flags file src
    case snd res of
        ParseFailed sl msg -> map (classify s) `fmap` parseFailed flags sl msg src
        ParseOk m -> return $
            let settings = mapMaybe readPragma $ moduleDecls m
            in map (classify $ s ++ settings) $ parseOk h m


parseFailed :: ParseFlags -> SrcLoc -> String -> String -> IO [Idea]
parseFailed flags sl msg src = do
    -- figure out the best line number to grab context from, by reparsing
    (str2,pr2) <- parseString (parseFlagsNoLocations flags) "" src
    let ctxt = case pr2 of
            ParseFailed sl2 _ -> context (srcLine sl2) str2
            _ -> context (srcLine sl) src
    return [ParseError Warning "Parse error" sl msg ctxt]


context :: Int -> String -> String
context lineNo src =
    unlines $ trimBy (all isSpace) $
    zipWith (++) ticks $ take 5 $ drop (lineNo - 3) $ lines src ++ [""]
    where ticks = ["  ","  ","> ","  ","  "]


parseOk :: [Hint] -> Module_ -> [Idea]
parseOk h m =
        order "" [i | ModuHint h <- h, i <- h nm m] ++
        concat [order (fromNamed d) [i | DeclHint h <- h, i <- h nm m d] | d <- moduleDecls m]
    where
        order n = map (\i -> i{func = (moduleName m,n)}) . sortBy (comparing loc)
        nm = nameMatch $ moduleImports m

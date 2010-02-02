
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
    case res of
        ParseFailed sl msg -> return $ map (classify s) $ parseFailed sl msg src
        ParseOk m -> return $
            let settings = mapMaybe readPragma $ moduleDecls m
            in map (classify $ s ++ settings) $ parseOk h m


parseFailed :: SrcLoc -> String -> String -> [Idea]
parseFailed sl msg src =
    let ticks = ["  ","  ","> ","  ","  "]
        bad = zipWith (++) ticks $ take 5 $ drop (srcLine sl - 3) $ lines src ++ [""]
        bad2 = reverse $ dropWhile (all isSpace) $ reverse $ dropWhile (all isSpace) bad
    in [ParseError Warning "Parse error" sl msg (unlines bad2)]


parseOk :: [Hint] -> Module_ -> [Idea]
parseOk h m =
        order "" [i | ModuHint h <- h, i <- h nm m] ++
        concat [order (fromNamed d) [i | DeclHint h <- h, i <- h nm m d] | d <- moduleDecls m]
    where
        order n = map (\i -> i{func = (moduleName m,n)}) . sortBy (comparing loc)
        nm = nameMatch $ moduleImports m

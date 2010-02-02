
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
    return $ applyHintStr flags h s file src


applyHintStr :: ParseFlags -> [Hint] -> [Setting] -> FilePath -> String -> [Idea]
applyHintStr flags h s file src =
    case parseString flags file src of
        ParseFailed sl msg ->
            let ticks = ["  ","  ","> ","  ","  "]
                bad = zipWith (++) ticks $ take 5 $ drop (srcLine sl - 3) $ lines src ++ [""]
                bad2 = reverse $ dropWhile (all isSpace) $ reverse $ dropWhile (all isSpace) bad
            in [classify s $ ParseError Warning "Parse error" sl msg (unlines bad2)]
        ParseOk m ->
            let name = moduleName m
                nm = nameMatch $ moduleImports m
                order n = map (\i -> i{func = (name,n)}) . sortBy (comparing loc)
                settings = mapMaybe readPragma $ moduleDecls m
            in map (classify $ s ++ settings) $
               order "" [i | ModuHint h <- h, i <- h nm m] ++
               concat [order (fromNamed d) [i | DeclHint h <- h, i <- h nm m d] | d <- moduleDecls m]

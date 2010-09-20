
module Apply(applyHint, applyHintStr) where

import HSE.All
import Hint.All
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Settings
import Idea
import Util


applyHint :: ParseFlags -> [Setting] -> FilePath -> IO [Idea]
applyHint flags s file = do
    src <- readFileEncoding (encoding flags) file
    applyHintStr flags s file src


applyHintStr :: ParseFlags -> [Setting] -> FilePath -> String -> IO [Idea]
applyHintStr flags s file src = do
    res <- parseString flags{infixes=[x | Infix x <- s]} file src
    case snd res of
        ParseFailed sl msg | length src `seq` True -> map (classify s) `fmap` parseFailed flags sl msg src
        ParseOk m -> return $
            let settings = mapMaybe readPragma $ moduleDecls m
            in map (classify $ s ++ settings) $ parseOk (allHints s) m


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
        nm = moduleScope m


allHints :: [Setting] -> [Hint]
allHints xs = dynamicHints xs : map f builtin
    where builtin = nub $ concat [if x == "All" then map fst staticHints else [x] | Builtin x <- xs]
          f x = fromMaybe (error $ "Unknown builtin hints: HLint.Builtin." ++ x) $ lookup x staticHints


classify :: [Setting] -> Idea -> Idea
classify xs i = i{rank = foldl' (rerank i) (rank i) $ filter isClassify xs}
    where
        -- figure out if we need to change the rank
        rerank :: Idea -> Rank -> Setting -> Rank
        rerank i r c | matchHint (hintS c) (hint i) && matchFunc (funcS c) (func_ i) = rankS c
                     | otherwise = r

        func_ x = if isParseError x then ("","") else func x
        matchHint = (~=)
        matchFunc (x1,x2) (y1,y2) = (x1~=y1) && (x2~=y2)
        x ~= y = null x || x == y

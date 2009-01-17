{-# LANGUAGE RecordWildCards #-}

module Report(writeReport) where

import Type
import Control.Arrow
import Language.Haskell.Exts
import Data.List
import Data.Maybe
import Data.Version
import System.FilePath
import HSE.All
import Paths_hlint
import Language.Haskell.HsColour.CSS


writeTemplate :: [(String,[String])] -> FilePath -> IO ()
writeTemplate content to = do
    dat <- getDataDir
    src <- readFile $ dat </> "report.html"
    writeFile to $ unlines $ concatMap f $ lines src
    where
        f ('$':xs) = fromMaybe ['$':xs] $ lookup xs content
        f x = [x]


writeReport :: FilePath -> [Idea] -> IO ()
writeReport file ideas = writeTemplate inner file
    where
        generateIds :: [String] -> [(String,Int)] -- sorted by name
        generateIds = map (head && length) . group . sort
        files = generateIds $ map (srcFilename . loc) ideas
        hints = generateIds $ map hintName ideas
        hintName x = show (rank x) ++ ": " ++ hint x

        inner = [("VERSION",['v' : showVersion version]),("CONTENT",content),
                 ("HINTS",list "hint" hints),("FILES",list "file" files)]

        content = concatMap (\i -> writeIdea (getClass i) i) ideas
        getClass i = "hint" ++ f hints (hintName i) ++ " file" ++ f files (srcFilename $ loc i)
            where f xs x = show $ fromJust $ findIndex ((==) x . fst) xs

        list mode xs = zipWith f [0..] xs
            where
                f i (name,n) = "<li><a id=" ++ show id ++ " href=\"javascript:show('" ++ id ++ "')\">" ++
                               escapeHTML name ++ " (" ++ show n ++ ")</a></li>"
                    where id = mode ++ show i


writeIdea :: String -> Idea -> [String]
writeIdea cls Idea{..} =
    ["<div class=" ++ show cls ++ ">"
    ,escapeHTML (showSrcLoc loc ++ " " ++ show rank ++ ": " ++ hint) ++ "<br/>"
    ,"Found<br/>"
    ,code from
    ,"Why not<br/>"
    ,code to
    ,"</div>"
    ,""]
    where
        code = hscolour False True ""


escapeHTML :: String -> String
escapeHTML = concatMap f
    where
        f '>' = "&gt;"
        f '<' = "&lt;"
        f '&' = "&amp;"
        f x = [x]

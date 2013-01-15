{-# LANGUAGE RecordWildCards #-}

module Report(writeReport) where

import Idea
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Version
import System.FilePath
import HSE.All
import Paths_hlint
import Language.Haskell.HsColour.CSS


writeTemplate :: FilePath -> [(String,[String])] -> FilePath -> IO ()
writeTemplate dataDir content to = do
    src <- readFile $ dataDir </> "report_template.html"
    writeFile to $ unlines $ concatMap f $ lines src
    where
        f ('$':xs) = fromMaybe ['$':xs] $ lookup xs content
        f x = [x]


writeReport :: FilePath -> FilePath -> [Idea] -> IO ()
writeReport dataDir file ideas = writeTemplate dataDir inner file
    where
        generateIds :: [String] -> [(String,Int)] -- sorted by name
        generateIds = map (head &&& length) . group . sort
        files = generateIds $ map (srcFilename . loc) ideas
        hints = generateIds $ map hintName ideas
        hintName x = show (severity x) ++ ": " ++ hint x

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


code = hscolour False


writeIdea :: String -> Idea -> [String]
writeIdea cls Idea{..} =
    ["<div class=" ++ show cls ++ ">"
    ,escapeHTML (showSrcLoc loc ++ ": " ++ show severity ++ ": " ++ hint) ++ "<br/>"
    ,"Found<br/>"
    ,code from
    ,"Why not" ++ (if to == "" then " remove it." else "") ++ "<br/>"
    ,code to
    ,let n = showNotes note in if n /= "" then "<span class='note'>Note: " ++ n ++ "</span>" else ""
    ,"</div>"
    ,""]


writeIdea cls ParseError{..} =
    ["<div class=" ++ show cls ++ ">"
    ,escapeHTML (showSrcLoc loc ++ ": " ++ show severity ++ ": " ++ hint) ++ "<br/>"
    ,"Error message<br/>"
    ,"<pre>" ++ escapeHTML msg ++ "</pre>"
    ,"Code<br/>"
    ,code from
    ,"</div>"
    ,""]


escapeHTML :: String -> String
escapeHTML = concatMap f
    where
        f '>' = "&gt;"
        f '<' = "&lt;"
        f '&' = "&amp;"
        f x = [x]

{-# LANGUAGE RecordWildCards #-}

module Report(writeReport) where

import Idea
import Data.Tuple.Extra
import Data.List.Extra
import Data.Maybe
import Data.Version
import System.FilePath
import System.IO.Extra
import HSE.All
import Paths_hlint
import HsColour


writeTemplate :: FilePath -> [(String,[String])] -> FilePath -> IO ()
writeTemplate dataDir content to = do
    src <- readFile' $ dataDir </> "report_template.html"
    writeFile to $ unlines $ concatMap f $ lines src
    where
        f ('$':xs) = fromMaybe ['$':xs] $ lookup xs content
        f x = [x]


writeReport :: FilePath -> FilePath -> [Idea] -> IO ()
writeReport dataDir file ideas = writeTemplate dataDir inner file
    where
        generateIds :: [String] -> [(String,Int)] -- sorted by name
        generateIds = map (head &&& length) . group -- must be already sorted
        files = generateIds $ sort $ map (srcSpanFilename . ideaSpan) ideas
        hints = generateIds $ map hintName $ sortOn (negate . fromEnum . ideaSeverity &&& hintName) ideas
        hintName x = show (ideaSeverity x) ++ ": " ++ ideaHint x

        inner = [("VERSION",['v' : showVersion version]),("CONTENT",content),
                 ("HINTS",list "hint" hints),("FILES",list "file" files)]

        content = concatMap (\i -> writeIdea (getClass i) i) ideas
        getClass i = "hint" ++ f hints (hintName i) ++ " file" ++ f files (srcSpanFilename $ ideaSpan i)
            where f xs x = show $ fromJust $ findIndex ((==) x . fst) xs

        list mode = zipWith f [0..]
            where
                f i (name,n) = "<li><a id=" ++ show id ++ " href=\"javascript:show('" ++ id ++ "')\">" ++
                               escapeHTML name ++ " (" ++ show n ++ ")</a></li>"
                    where id = mode ++ show i


writeIdea :: String -> Idea -> [String]
writeIdea cls Idea{..} =
    ["<div class=" ++ show cls ++ ">"
    ,escapeHTML (showSrcLoc (getPointLoc ideaSpan) ++ ": " ++ show ideaSeverity ++ ": " ++ ideaHint) ++ "<br/>"
    ,"Found<br/>"
    ,hsColourHTML ideaFrom] ++
    (case ideaTo of
        Nothing -> []
        Just to ->
            ["Why not" ++ (if to == "" then " remove it." else "") ++ "<br/>"
            ,hsColourHTML to]) ++
    [let n = showNotes ideaNote in if n /= "" then "<span class='note'>Note: " ++ writeNote n ++ "</span>" else ""
    ,"</div>"
    ,""]

-- Unescaped, but may have `backticks` for code
writeNote :: String -> String
writeNote = f . splitOn "`"
    where f (a:b:c) = escapeHTML a ++ "<tt>" ++ escapeHTML b ++ "</tt>" ++ f c
          f xs = concatMap escapeHTML xs

escapeHTML :: String -> String
escapeHTML = concatMap f
    where
        f '>' = "&gt;"
        f '<' = "&lt;"
        f '&' = "&amp;"
        f x = [x]

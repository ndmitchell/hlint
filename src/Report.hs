{-# LANGUAGE RecordWildCards #-}

module Report(writeReport) where

import Idea
import Data.Tuple.Extra
import Data.List.Extra
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Version
import Timing
import Paths_hlint
import HsColour
import EmbedData
import qualified GHC.Util as GHC


writeTemplate :: FilePath -> [(String,[String])] -> FilePath -> IO ()
writeTemplate dataDir content to =
    writeFile to $ unlines $ concatMap f $ lines reportTemplate
    where
        f ('$':xs) = fromMaybe ['$':xs] $ lookup xs content
        f x = [x]


writeReport :: FilePath -> FilePath -> [Idea] -> IO ()
writeReport dataDir file ideas = timedIO "Report" file $ writeTemplate dataDir inner file
    where
        generateIds :: [String] -> [(String,Int)] -- sorted by name
        generateIds = map (NE.head &&& length) . NE.group -- must be already sorted
        files = generateIds $ sort $ map (GHC.srcSpanFilename . ideaSpan) ideas
        hints = generateIds $ map hintName $ sortOn (negate . fromEnum . ideaSeverity &&& hintName) ideas
        hintName x = show (ideaSeverity x) ++ ": " ++ ideaHint x

        inner = if null ideas then emptyInner else nonEmptyInner

        emptyInner = [("VERSION",['v' : showVersion version]),("CONTENT", ["No hints"]),
                      ("HINTS", ["<li>No hints</li>"]),("FILES", ["<li>No files</li>"])]

        nonEmptyInner = [("VERSION",['v' : showVersion version]),("CONTENT",content),
                         ("HINTS",list "hint" hints),("FILES",list "file" files)]

        content = concatMap (\i -> writeIdea (getClass i) i) ideas
        getClass i = "hint" ++ f hints (hintName i) ++ " file" ++ f files (GHC.srcSpanFilename $ ideaSpan i)
            where f xs x = show $ fromJust $ findIndex ((==) x . fst) xs

        list mode = zipWithFrom f 0
            where
                f i (name,n) = "<li><a id=" ++ show id ++ " href=\"javascript:show('" ++ id ++ "')\">" ++
                               escapeHTML name ++ " (" ++ show n ++ ")</a></li>"
                    where id = mode ++ show i


writeIdea :: String -> Idea -> [String]
writeIdea cls Idea{..} =
    ["<div class=" ++ show cls ++ ">"
    ,escapeHTML (GHC.showSrcSpan ideaSpan ++ ": " ++ show ideaSeverity ++ ": " ++ ideaHint) ++ "<br/>"
    ,"Found<br/>"
    ,hsColourHTML ideaFrom] ++
    (case ideaTo of
        Nothing -> []
        Just to ->
            ["Perhaps" ++ (if to == "" then " you should remove it." else "") ++ "<br/>"
            ,hsColourHTML to]) ++
    [let n = showNotes ideaNote in if n /= "" then "<span class='note'>Note: " ++ writeNote n ++ "</span>" else ""
    ,"</div>"
    ,""]

-- Unescaped, but may have `backticks` for code
writeNote :: String -> String
writeNote = f . splitOn "`"
    where f (a:b:c) = escapeHTML a ++ "<tt>" ++ escapeHTML b ++ "</tt>" ++ f c
          f xs = concatMap escapeHTML xs

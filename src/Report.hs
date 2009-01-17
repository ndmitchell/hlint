{-# LANGUAGE RecordWildCards #-}

module Report(writeReport) where

import Type
import Language.Haskell.Exts
import Data.List
import Data.Maybe
import Data.Version
import System.FilePath
import HSE.All
import Paths_hlint


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
        generateIds = map (\x -> (head x, length x)) . group . sort
        files = generateIds $ map (srcFilename . loc) ideas
        hints = generateIds $ map hint ideas

        inner = [("VERSION",['v' : showVersion version]),("CONTENT",content),
                 ("HINTS",list "hint" hints),("FILES",list "file" files)]

        content = concatMap (\i -> writeIdea (getClass i) i) ideas
        getClass i = "hint" ++ f hints (hint i) ++ " file" ++ f files (srcFilename $ loc i)
            where f xs x = show $ fromJust $ findIndex ((==) x . fst) xs

        list mode xs = zipWith f [0..] xs
            where
                f i (name,n) = "<li><a id=" ++ show id ++ " href=\"javascript:show('" ++ id ++ "')\">" ++
                               name ++ " (" ++ show n ++ ")</a></li>"
                    where id = mode ++ show i


writeIdea :: String -> Idea -> [String]
writeIdea cls Idea{..} =
    ["<div class=" ++ show cls ++ ">"
    ,showSrcLoc loc ++ " " ++ show rank ++ ": " ++ hint ++ "<br/>"
    ,"Found<br/>"
    ,code from
    ,"Why not<br/>"
    ,code to
    ,"</div>"
    ,""]
    where
        code x = "<pre>" ++ concatMap f x ++ "</pre>"
        f '>' = "&gt;"
        f '<' = "&lt;"
        f '&' = "&amp;"
        f x = [x]

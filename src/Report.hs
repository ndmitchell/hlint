
module Report(writeReport) where

import Type
import Language.Haskell.Exts
import Data.List
import System.FilePath
import Paths_hlint


writeReport :: FilePath -> [Idea] -> IO ()
writeReport file ideas = do
    writeReport1 file ideas
    let file2 = dropExtension file ++ "_2" ++ takeExtension file
    writeReport2 file2 ideas


writeReport1 :: FilePath -> [Idea] -> IO ()
writeReport1 file ideas = writeTemplate "report.html" content file
    where
        content = map f ideas
        drp = filePrefix $ map (srcFilename . loc) ideas
        f x = "idea(" ++ concat (intersperse "," args) ++ ");"
            where args = [show $ show (rank x) ++ ": " ++ hint x
                         ,show $ drp $ srcFilename $ loc x, show $ srcLine $ loc x, show $ srcColumn $ loc x
                         ,show $ from x, show $ to x]


writeTemplate :: FilePath -> [String] -> FilePath -> IO ()
writeTemplate from content to = do
    dat <- getDataDir
    src <- readFile $ dat </> from
    writeFile to $ unlines $ repContent content $ lines src


repContent :: [String] -> [String] -> [String]
repContent content xs = pre ++ take 1 mid ++ content ++ post
    where
        (pre,mid) = break (isInfixOf "<CONTENT>") xs
        post = dropWhile (not . isInfixOf "</CONTENT>") mid


filePrefix :: [FilePath] -> (FilePath -> FilePath)
filePrefix xs | null xs = flipSlash
              | otherwise = flipSlash . drop n2
    where
        (mn,mx) = (minimum xs, maximum xs)
        n = length $ takeWhile id $ zipWith (==) mn mx
        n2 = length $ dropWhile (`notElem` "\\/") $ reverse $ take n mn

        flipSlash = map (\x -> if x == '\\' then '/' else x)




writeReport2 :: FilePath -> [Idea] -> IO ()
writeReport2 out ideas = return ()


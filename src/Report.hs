
module Report(writeReport) where

import Type
import Language.Haskell.Exts
import Data.List
import System.FilePath
import Paths_hlint


writeReport :: FilePath -> [Idea] -> IO ()
writeReport file ideas = do
        dat <- getDataDir
        src <- readFile (dat </> "report.html")
        writeFile file $ unlines $ repContent content $ lines src
    where
        content = map f ideas
        drp = filePrefix $ map (srcFilename . loc) ideas
        f x = "idea(" ++ concat (intersperse "," args) ++ ");"
            where args = [show $ hint x
                         ,show $ drp $ srcFilename $ loc x, show $ srcLine $ loc x, show $ srcColumn $ loc x
                         ,show $ from x, show $ to x]


repContent :: [String] -> [String] -> [String]
repContent content xs = pre ++ take 1 mid ++ content ++ post
    where
        (pre,mid) = break (isInfixOf_ "<CONTENT>") xs
        post = dropWhile (not . isInfixOf_ "</CONTENT>") mid


-- from GHC 6.10 is in the real libraries
isInfixOf_ find = any (isPrefixOf find) . tails


filePrefix :: [FilePath] -> (FilePath -> FilePath)
filePrefix xs | null xs = flipSlash
              | otherwise = flipSlash . drop n2
    where
        (mn,mx) = (minimum xs, maximum xs)
        n = length $ takeWhile id $ zipWith (==) mn mx
        n2 = length $ dropWhile (`notElem` "\\/") $ reverse $ take n mn

        flipSlash = map (\x -> if x == '\\' then '/' else x)

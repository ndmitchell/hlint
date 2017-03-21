{-# LANGUAGE PatternGuards, RecordWildCards #-}

-- | Check the <TEST> annotations within source and hint files.
module Test.Annotations(testAnnotations) where

import Data.Tuple.Extra
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Function

import Config.Type
import Idea
import Apply
import HSE.All
import Test.Util


-- Input, Output
-- Output = Nothing, should not match
-- Output = Just xs, should match xs
data Test = Test SrcLoc String (Maybe String)

testAnnotations :: [Setting] -> FilePath -> IO ()
testAnnotations setting file = do
    tests <- parseTestFile file
    mapM_ f tests
    where
        f (Test loc inp out) = do
            ideas <- applyHintFile defaultParseFlags setting file $ Just inp
            let good = case out of
                    Nothing -> null ideas
                    Just x -> length ideas == 1 &&
                              seq (length (show ideas)) True && -- force, mainly for hpc
                              match x (head ideas)
            let bad =
                    [failed $
                        ["TEST FAILURE (" ++ show (length ideas) ++ " hints generated)"
                        ,"SRC: " ++ showSrcLoc loc
                        ,"INPUT: " ++ inp] ++
                        map ((++) "OUTPUT: " . show) ideas ++
                        ["WANTED: " ++ fromMaybe "<failure>" out]
                        | not good] ++
                    [failed
                        ["TEST FAILURE (BAD LOCATION)"
                        ,"SRC: " ++ showSrcLoc loc
                        ,"INPUT: " ++ inp
                        ,"OUTPUT: " ++ show i]
                        | i@Idea{..} <- ideas, let SrcLoc{..} = getPointLoc ideaSpan, srcFilename == "" || srcLine == 0 || srcColumn == 0]
            if null bad then passed else sequence_ bad

        match "???" _ = True
        match x y | "@" `isPrefixOf` x = a == show (ideaSeverity y) && match (trimStart b) y
            where (a,b) = break isSpace $ tail x
        match x y = on (==) norm (fromMaybe "" $ ideaTo y) x

        -- FIXME: Should use a better check for expected results
        norm = filter $ \x -> not (isSpace x) && x /= ';'


parseTestFile :: FilePath -> IO [Test]
parseTestFile file = do
    src <- readFile file
    -- we remove all leading # symbols since Yaml only lets us do comments that way
    return $ f False $ zip [1..] $ map (\x -> fromMaybe x $ stripPrefix "# " x) $ lines src
    where
        open = isPrefixOf "<TEST>"
        shut = isPrefixOf "</TEST>"

        f False ((i,x):xs) = f (open x) xs
        f True  ((i,x):xs)
            | shut x = f False xs
            | null x || "-- " `isPrefixOf` x = f True xs
            | "\\" `isSuffixOf` x, (_,y):ys <- xs = f True $ (i,init x++"\n"++y):ys
            | otherwise = parseTest file i x : f True xs
        f _ [] = []


parseTest file i x = uncurry (Test (SrcLoc file i 0)) $ f x
    where
        f x | Just x <- stripPrefix "<COMMENT>" x = first ("--"++) $ f x
        f (' ':'-':'-':xs) | null xs || " " `isPrefixOf` xs = ("", Just $ dropWhile isSpace xs)
        f (x:xs) = first (x:) $ f xs
        f [] = ([], Nothing)

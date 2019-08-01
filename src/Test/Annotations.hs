{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}

-- | Check the <TEST> annotations within source and hint files.
module Test.Annotations(testAnnotations) where

import Control.Exception.Extra
import Data.Tuple.Extra
import Data.Char
import Data.Either.Extra
import Data.List.Extra
import Data.Maybe
import Control.Monad
import System.FilePath
import Control.Monad.IO.Class
import Data.Function
import Data.Yaml
import qualified Data.ByteString.Char8 as BS

import Config.Type
import Idea
import Apply
import HSE.All
import Test.Util
import Data.Functor
import Prelude
import Config.Yaml


-- Input, Output
-- Output = Nothing, should not match
-- Output = Just xs, should match xs
data TestCase = TestCase SrcLoc String (Maybe String) [Setting] deriving (Show)

testAnnotations :: [Setting] -> FilePath -> Test ()
testAnnotations setting file = do
    tests <- liftIO $ parseTestFile file
    mapM_ f tests
    where
        f (TestCase loc inp out additionalSettings) = do
            ideas <- liftIO $ try_ $ do
                res <- applyHintFile defaultParseFlags (setting ++ additionalSettings) file $ Just inp
                evaluate $ length $ show res
                return res

            -- the hints from data/Test.hs are really fake hints we don't actually deploy
            -- so don't record them
            when (takeFileName file /= "Test.hs") $
                either (const $ return ()) addIdeas ideas

            let good = case (out, ideas) of
                    (Nothing, Right []) -> True
                    (Just x, Right [idea]) | match x idea -> True
                    _ -> False
            let bad =
                    [failed $
                        ["TEST FAILURE (" ++ show (either (const 1) length ideas) ++ " hints generated)"
                        ,"SRC: " ++ showSrcLoc loc
                        ,"INPUT: " ++ inp] ++
                        map ("OUTPUT: " ++) (either (return . show) (map show) ideas) ++
                        ["WANTED: " ++ fromMaybe "<failure>" out]
                        | not good] ++
                    [failed
                        ["TEST FAILURE (BAD LOCATION)"
                        ,"SRC: " ++ showSrcLoc loc
                        ,"INPUT: " ++ inp
                        ,"OUTPUT: " ++ show i]
                        | i@Idea{..} <- fromRight [] ideas, let SrcLoc{..} = getPointLoc ideaSpan, srcFilename == "" || srcLine == 0 || srcColumn == 0]
            if null bad then passed else sequence_ bad

        match "???" _ = True
        match (word1 -> ("@Message",msg)) i = ideaHint i == msg
        match (word1 -> ("@Note",note)) i = map show (ideaNote i) == [note]
        match "@NoNote" i = null (ideaNote i)
        match (word1 -> ('@':sev, msg)) i = sev == show (ideaSeverity i) && match msg i
        match msg i = on (==) norm (fromMaybe "" $ ideaTo i) msg

        -- FIXME: Should use a better check for expected results
        norm = filter $ \x -> not (isSpace x) && x /= ';'


parseTestFile :: FilePath -> IO [TestCase]
parseTestFile file =
    -- we remove all leading # symbols since Yaml only lets us do comments that way
    f Nothing . zip [1..] . map (\x -> fromMaybe x $ stripPrefix "# " x) . lines <$> readFile file
    where
        open :: String -> Maybe [Setting]
        open line
          |  "<TEST>" `isPrefixOf` line =
             let suffix = dropPrefix "<TEST>" line
                 config = decodeEither'  $ BS.pack suffix
             in case config of
                  Left err -> Just []
                  Right config -> Just $ settingsFromConfigYaml [config]
          | otherwise = Nothing

        shut :: String -> Bool
        shut = isPrefixOf "</TEST>"

        f :: Maybe [Setting] -> [(Int, String)] -> [TestCase]
        f Nothing ((i,x):xs) = f (open x) xs
        f (Just s)  ((i,x):xs)
            | shut x = f Nothing xs
            | null x || "-- " `isPrefixOf` x = f (Just s) xs
            | "\\" `isSuffixOf` x, (_,y):ys <- xs = f (Just s) $ (i,init x++"\n"++y):ys
            | otherwise = parseTest file i x s : f (Just s) xs
        f _ [] = []


parseTest :: String -> Int -> String -> [Setting] -> TestCase
parseTest file i x = uncurry (TestCase (SrcLoc file i 0)) $ f x
    where
        f x | Just x <- stripPrefix "<COMMENT>" x = first ("--"++) $ f x
        f (' ':'-':'-':xs) | null xs || " " `isPrefixOf` xs = ("", Just $ dropWhile isSpace xs)
        f (x:xs) = first (x:) $ f xs
        f [] = ([], Nothing)

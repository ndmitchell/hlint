{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

module Test.InputOutput(testInputOutput) where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Verbosity
import System.Exit

import Util
import Test.Util


testInputOutput :: ([String] -> IO ()) -> IO Result
testInputOutput main = do
    xs <- getDirectoryContents "tests"
    xs <- return $ filter ((==) ".test" . takeExtension) xs
    results $ fmap concat $ forM xs $ \file -> do
        ios <- parseInputOutputs <$> readFile ("tests" </> file)
        res <- forM (zip [1..] ios) $ \(i,io@InputOutput{..}) -> do
            forM_ files $ \(name,contents) -> do
                createDirectoryIfMissing True $ takeDirectory name
                writeFile name contents
            checkInputOutput main io{name= "_" ++ takeBaseName file ++ "_" ++ show i}
        mapM_ (removeFile . fst) $ concatMap files ios
        return res

data InputOutput = InputOutput
    {name :: String
    ,files :: [(FilePath, String)]
    ,run :: [String]
    ,output :: String
    ,exit :: Int -- FIXME: Not currently checked
    } deriving Eq

parseInputOutputs :: String -> [InputOutput]
parseInputOutputs = f z . lines
    where
        z = InputOutput "unknown" [] [] "" 0
        interest x = any (`isPrefixOf` x) ["----","FILE","RUN","OUTPUT","EXIT"]

        f io ((stripPrefix "RUN " -> Just flags):xs) = f io{run = splitArgs flags} xs
        f io ((stripPrefix "EXIT " -> Just code):xs) = f io{exit = read code} xs
        f io ((stripPrefix "FILE " -> Just file):xs) | (str,xs) <- g xs = f io{files = files io ++ [(file,unlines str)]} xs
        f io ("OUTPUT":xs) | (str,xs) <- g xs = f io{output = unlines str} xs
        f io ((isPrefixOf "----" -> True):xs) = [io | io /= z] ++ f z xs
        f io [] = [io | io /= z]
        f io (x:xs) = error $ "Unknown test item, " ++ x

        g = first (reverse . dropWhile null . reverse) . break interest


---------------------------------------------------------------------
-- CHECK INPUT/OUTPUT PAIRS

checkInputOutput :: ([String] -> IO ()) -> InputOutput -> IO Result
checkInputOutput main InputOutput{..} = do
    got <- fmap (reverse . dropWhile null . reverse . map rtrim . lines) $ captureOutput $
        handle (\(e::SomeException) -> print e) $
        handle (\(e::ExitCode) -> return ()) $ do
        bracket getVerbosity setVerbosity $ const $ setVerbosity Normal >> main run
    (want,got) <- return $ matchStarStar (lines output) got

    if length got == length want && and (zipWith matchStar want got) then
        return pass
     else do
        let trail = replicate (max (length got) (length want)) "<EOF>"
        let (i,g,w):_ = [(i,g,w) | (i,g,w) <- zip3 [1..] (got++trail) (want++trail), not $ matchStar w g]
        putStrLn $ unlines
            ["TEST FAILURE IN tests/" ++ name
            ,"DIFFER ON LINE: " ++ show i
            ,"GOT : " ++ g
            ,"WANT: " ++ w]
        v <- getVerbosity
        when (null want || v >= Loud) $ putStrLn $ unlines $ "FULL OUTPUT FOR GOT:" : got
        return failure


-- | First string may have stars in it (the want)
matchStar :: String -> String -> Bool
matchStar ('*':xs) ys = any (matchStar xs) $ tails ys
matchStar (x:xs) (y:ys) = x == y && matchStar xs ys
matchStar [] [] = True
matchStar _ _ = False


matchStarStar :: [String] -> [String] -> ([String], [String])
matchStarStar want got = case break (== "**") want of
    (_, []) -> (want, got)
    (w1,_:w2) -> (w1++w2, g1 ++ revTake (length w2) g2)
        where (g1,g2) = splitAt (length w1) got

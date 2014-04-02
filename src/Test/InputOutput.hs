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
    files <- return $ filter ((==) ".test" . takeExtension) xs
    (add,sub) <- fmap (second concat . unzip . concat) $ forM files $ \file -> do
        ios <- parseInputOutputs <$> readFile ("tests" </> file)
        forM (zip [1..] ios) $ \(i,InputOutput{..}) -> do
            forM_ files $ \(name,contents) -> do
                createDirectoryIfMissing True $ takeDirectory name
                writeFile name contents
            let name = "_" ++ takeBaseName file ++ "_" ++ show i
            writeFile ("tests" </> name <.> "flags") $ unlines run
            writeFile ("tests" </> name <.> "output") output
            return (map (name <.>) ["flags","output"], map fst files)
    res <- results $ mapM (checkInputOutput main) add
    mapM_ removeFile sub
    forM_ (concat add) $ \x -> removeFile $ "tests" </> x
    return res

data InputOutput = InputOutput
    {files :: [(FilePath, String)]
    ,run :: [String]
    ,output :: String
    ,exit :: Int -- FIXME: Not currently checked
    } deriving Eq

parseInputOutputs :: String -> [InputOutput]
parseInputOutputs = f z . lines
    where
        z = InputOutput [] [] "" 0
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

checkInputOutput :: ([String] -> IO ()) -> [FilePath] -> IO Result
checkInputOutput main xs = do
    let pre = takeBaseName $ head xs
        has x = (pre <.> x) `elem` xs
        reader x = readFile' $ "tests" </> pre <.> x

    flags <-
        if has "flags" then lines <$> reader "flags"
        else if has "hs" then return ["tests/" ++ pre <.> "hs"]
        else if has "lhs" then return ["tests/" ++ pre <.> "lhs"]
        else error $ "checkInputOutput, couldn't find or figure out flags for " ++ pre

    got <- fmap (reverse . dropWhile null . reverse . map rtrim . lines) $ captureOutput $
        handle (\(e::SomeException) -> print e) $
        handle (\(e::ExitCode) -> return ()) $ do
        bracket getVerbosity setVerbosity $ const $ setVerbosity Normal >> main flags
    want <- lines <$> reader "output"
    (want,got) <- return $ matchStarStar want got

    if length got == length want && and (zipWith matchStar want got) then
        return pass
     else do
        let trail = replicate (max (length got) (length want)) "<EOF>"
        let (i,g,w):_ = [(i,g,w) | (i,g,w) <- zip3 [1..] (got++trail) (want++trail), not $ matchStar w g]
        putStrLn $ unlines
            ["TEST FAILURE IN tests/" ++ pre
            ,"DIFFER ON LINE: " ++ show i
            ,"GOT : " ++ g
            ,"WANT: " ++ w]
        when (null want) $ putStrLn $ unlines $ "FULL OUTPUT FOR GOT:" : got
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

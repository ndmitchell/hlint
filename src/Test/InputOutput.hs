{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards, ViewPatterns #-}

-- | Check the input/output pairs in the tests/ directory
module Test.InputOutput(testInputOutput) where

import Control.Applicative
import Data.Tuple.Extra
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List.Extra
import Data.IORef
import System.Directory
import System.FilePath
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Verbosity
import System.Exit
import System.IO.Extra
import Prelude

import Test.Util


testInputOutput :: ([String] -> IO ()) -> Test ()
testInputOutput main = do
    xs <- liftIO $ getDirectoryContents "tests"
    xs <- pure $ filter ((==) ".test" . takeExtension) xs
    forM_ xs $ \file -> do
        ios <- liftIO $ parseInputOutputs <$> readFile ("tests" </> file)
        forM_ (zipFrom 1 ios) $ \(i,io@InputOutput{..}) -> do
            progress
            liftIO $ forM_ files $ \(name,contents) -> do
                createDirectoryIfMissing True $ takeDirectory name
                writeFile name contents
            checkInputOutput main io{name= "_" ++ takeBaseName file ++ "_" ++ show i}
        liftIO $ mapM_ (removeFile . fst) $ concatMap files ios

data InputOutput = InputOutput
    {name :: String
    ,files :: [(FilePath, String)]
    ,run :: [String]
    ,output :: String
    ,exit :: Maybe ExitCode
    } deriving Eq

parseInputOutputs :: String -> [InputOutput]
parseInputOutputs = f z . lines
    where
        z = InputOutput "unknown" [] [] "" Nothing
        interest x = any (`isPrefixOf` x) ["----","FILE","RUN","OUTPUT","EXIT"]

        f io ((stripPrefix "RUN " -> Just flags):xs) = f io{run = splitArgs flags} xs
        f io ((stripPrefix "EXIT " -> Just code):xs) = f io{exit = Just $ let i = read code in if i == 0 then ExitSuccess else ExitFailure i} xs
        f io ((stripPrefix "FILE " -> Just file):xs) | (str,xs) <- g xs = f io{files = files io ++ [(file,unlines str)]} xs
        f io ("OUTPUT":xs) | (str,xs) <- g xs = f io{output = unlines str} xs
        f io ((isPrefixOf "----" -> True):xs) = [io | io /= z] ++ f z xs
        f io [] = [io | io /= z]
        f io (x:xs) = error $ "Unknown test item, " ++ x

        g = first (reverse . dropWhile null . reverse) . break interest


---------------------------------------------------------------------
-- CHECK INPUT/OUTPUT PAIRS

checkInputOutput :: ([String] -> IO ()) -> InputOutput -> Test ()
checkInputOutput main InputOutput{..} = do
    code <- liftIO $ newIORef ExitSuccess
    got <- liftIO $ fmap (reverse . dropWhile null . reverse . map trimEnd . lines . fst) $ captureOutput $
        handle (\(e::SomeException) -> print e) $
        handle (\(e::ExitCode) -> writeIORef code e) $
        bracket getVerbosity setVerbosity $ const $ setVerbosity Normal >> main run
    code <- liftIO $ readIORef code
    (want,got) <- pure $ matchStarStar (lines output) got

    if maybe False (/= code) exit then
        failed
            ["TEST FAILURE IN tests/" ++ name
            ,"WRONG EXIT CODE"
            ,"GOT : " ++ show code
            ,"WANT: " ++ show exit
            ]
     else if length got == length want && and (zipWith matchStar want got) then
        passed
     else do
        let trail = replicate (max (length got) (length want)) "<EOF>"
        let (i,g,w):_ = [(i,g,w) | (i,g,w) <- zip3 [1..] (got++trail) (want++trail), not $ matchStar w g]
        failed $
            ["TEST FAILURE IN tests/" ++ name
            ,"DIFFER ON LINE: " ++ show i
            ,"GOT : " ++ g
            ,"WANT: " ++ w
            ,"FULL OUTPUT FOR GOT:"] ++ got


-- | First string may have stars in it (the want)
matchStar :: String -> String -> Bool
matchStar ('*':xs) ys = any (matchStar xs) $ tails ys
matchStar ('/':x:xs) ('\\':'\\':ys) | x /= '/' = matchStar (x:xs) ys -- JSON escaped newlines
matchStar (x:xs) (y:ys) = eq x y && matchStar xs ys
    where
        -- allow path differences between Windows and Linux
        eq '/' y = isPathSeparator y
        eq x y = x == y
matchStar [] [] = True
matchStar _ _ = False


matchStarStar :: [String] -> [String] -> ([String], [String])
matchStarStar want got = case break (== "**") want of
    (_, []) -> (want, got)
    (w1,_:w2) -> (w1++w2, g1 ++ takeEnd (length w2) g2)
        where (g1,g2) = splitAt (length w1) got

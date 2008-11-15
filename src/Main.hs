
module Main where

import Control.Monad
import Language.Haskell.Exts
import Data.List

import CmdLine
import Type
import Util
import Hint.All


main = do
    mode <- getMode
    hints <- readHints $ modeHints mode
    if modeTest mode then do
        n <- liftM sum $ mapM (runTest hints) (modeFiles mode)
        if n == 0
            then putStrLn $ "Tests passed"
            else putStrLn $ "Tests failed (" ++ show n ++ ")"
     else do
        n <- liftM sum $ mapM (runFile hints) (modeFiles mode)
        if n == 0
            then putStrLn "No relevant suggestions"
            else putStrLn $ "Found " ++ show n ++ " suggestions"


-- return the number of hints given
runFile :: Hint -> FilePath -> IO Int
runFile hint file = do
    src <- parseHsModule file
    let ideas = applyHint hint src
    mapM_ print ideas
    return $ length ideas


-- return the number of failures
runTest :: Hint -> FilePath -> IO Int
runTest hint file = do
    Module _ _ _ _ tests <- parseHsModule file
    let failures = concatMap f tests
    mapM_ putStrLn failures
    return $ length failures
    where
        f o | ("_NO" `isSuffixOf` name) == null ideas && length ideas <= 1 = []
            | otherwise = ["Test failed in " ++ name ++ concatMap ((++) " | " . show) ideas]
            where
                ideas = hint o
                name = declName o

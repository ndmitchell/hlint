
module Main where

import Control.Monad
import Language.Haskell.Exts
import Data.List

import CmdLine
import Hint.Type
import Hint.Util
import Hint.Read


main = do
    mode <- getMode
    hints <- readHints $ modeHints mode
    if modeTest mode then do
        let files = ifNull (modeFiles mode) ["Test.hs"]
        success <- liftM and $ mapM (runTest hints) files
        putStrLn $ "Tests " ++ if success then "passed" else "failed"
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


-- return False for failure
runTest :: Hint -> FilePath -> IO Bool
runTest hint file = do
    HsModule _ _ _ _ tests <- parseHsModule file
    let failures = concatMap f tests
    mapM_ putStr failures
    return $ null failures
    where
        f o | ("_NO" `isSuffixOf` name) == null ideas && length ideas <= 1 = []
            | otherwise = ["Test failed in " ++ name ++ concatMap ((++) " | " . show) ideas]
            where
                ideas = hint o
                name = declName o

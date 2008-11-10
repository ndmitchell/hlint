
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
    let test = modeTest mode

    n <- liftM sum $ mapM (runFile test hints) (modeFiles mode)
    when test $ putStrLn $ "Tests " ++ if n == 0 then "passed" else "failed"
    if n == 0
        then putStrLn "No relevant suggestions"
        else putStrLn $ "Found " ++ show n ++ " suggestions"


runFile test hints file = do
    src <- parseHsModule file
    if not test then do
        let ideas = applyHint hints src
        putStr $ unlines $ map show ideas
        return $ length ideas
     else do
        let HsModule _ _ _ _ tests = src
        liftM sum $ mapM f tests
    where
        f o | ("_NO" `isSuffixOf` name) == null ideas && length ideas <= 1 = return 0
            | otherwise = do
                putStrLn $ "Test failed in " ++ name ++ concatMap ((++) " | " . show) ideas
                return 1
            where
                ideas = applyHint hints $ HsModule undefined undefined undefined undefined [o]
                name = declName o

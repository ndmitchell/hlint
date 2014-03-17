{-# LANGUAGE RecordWildCards #-}

module Grep(runGrep) where

import Language.Haskell.HLint2
import HSE.All
import Control.Monad
import Data.List
import Util


runGrep :: String -> ParseFlags -> [FilePath] -> IO ()
runGrep pattern flags files = do
    exp <- case parseExp pattern of
        ParseOk x -> return x
        ParseFailed sl msg ->
            exitMessage $ (if "Parse error" `isPrefixOf` msg then msg else "Parse error in pattern: " ++ msg) ++ "\n" ++
                          pattern ++ "\n" ++
                          replicate (srcColumn sl - 1) ' ' ++ "^"
    let scope = scopeCreate $ Module an Nothing [] [] []
    let rule = hintRules [HintRule Warning "grep" scope exp (Tuple an Boxed []) Nothing []]
    forM_ files $ \file -> do
        res <- parseModuleEx flags file Nothing
        case res of
            Left (ParseError sl msg ctxt) -> do
                putStrLn $ showSrcLoc sl ++ ": " ++ (if "Parse error" `isPrefixOf` msg then msg else "Parse error: " ++ msg) ++ "\n" ++ ctxt
            Right m ->
                forM_ (applyHints [] rule [m]) $ \Idea{..} -> do
                    putStrLn $ unlines $ (showSrcLoc (getPointLoc ideaSpan) ++ ":") : map ("  "++) (lines ideaFrom)

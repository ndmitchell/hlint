{-# LANGUAGE RecordWildCards #-}

module Grep(runGrep) where

import Language.Haskell.HLint2
import HSE.All
import Control.Monad


runGrep :: String -> ParseFlags -> [FilePath] -> IO ()
runGrep pattern flags files = do
    let exp = fromParseResult $ parseExp pattern
    let scope = scopeCreate $ Module an Nothing [] [] []
    let rule = hintRules [HintRule Warning "grep" scope exp (Tuple an Boxed []) Nothing []]
    forM_ files $ \file -> do
        Right m <- parseModuleEx flags file Nothing
        forM_ (applyHints [] rule [m]) $ \Idea{..} -> do
            putStr $ unlines $ showSrcLoc (getPointLoc ideaSpan) : map ("  "++) (lines ideaFrom)

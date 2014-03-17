{-# LANGUAGE RecordWildCards #-}

module Grep(runGrep) where

import Language.Haskell.HLint2
import HSE.All
import Control.Monad


runGrep :: String -> Bool -> ParseFlags -> [FilePath] -> IO ()
runGrep pattern exact flags files = do
    let exp = fromParseResult $ parseExp pattern
    forM_ files $ \file -> do
        Right m <- parseModuleEx flags file Nothing
        let rule = hintRules [HintRule Warning "grep" (scopeCreate m) exp exp Nothing []]
        forM_ (applyHints [] rule [m]) $ \Idea{..} -> do
            putStr $ unlines $ showSrcLoc (getPointLoc ideaSpan) : map ("  "++) (lines ideaFrom)

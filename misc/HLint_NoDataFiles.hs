{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

-- This script shows how to use HLint without any data files (provided you do not use --report)
-- Running this script should report two errors

import Data.List
import Language.Haskell.HLint
import Language.Haskell.TH.Syntax
import Network.HTTP


hints :: String
hints = $(do
    let openURL x = getResponseBody =<< simpleHTTP (getRequest x)
    let grab x = qRunIO $ fmap lines $ openURL $ "http://community.haskell.org/~ndm/darcs/hlint/data/" ++ x
    outer <- grab "HLint.hs"
    hints <- grab "Default.hs"
    lift $ unlines $
        filter (not . isSuffixOf "HLint.Default") outer ++
        filter (not . isPrefixOf "module") hints
    )

main :: IO ()
main = do
    x <- hlint ["NoDataFiles.hs", "--quiet", "--with=" ++ hints]
    putStr $ unlines $ map show x


module Grep(runGrep) where

import Hint.All
import Apply
import Config.Type
import HSE.All
import Control.Monad
import Data.List
import Util
import Idea


runGrep :: String -> ParseFlags -> [FilePath] -> IO ()
runGrep patt flags files = do
    exp <- case parseExp patt of
        ParseOk x -> return x
        ParseFailed sl msg ->
            exitMessage $ (if "Parse error" `isPrefixOf` msg then msg else "Parse error in pattern: " ++ msg) ++ "\n" ++
                          patt ++ "\n" ++
                          replicate (srcColumn sl - 1) ' ' ++ "^"
    let scope = scopeCreate $ Module an Nothing [] [] []
    let rule = hintRules [HintRule Suggestion "grep" scope exp (Tuple an Boxed []) Nothing []]
    forM_ files $ \file -> do
        res <- parseModuleEx flags file Nothing
        case res of
            Left (ParseError sl msg ctxt) ->
                print $ rawIdeaN Error (if "Parse error" `isPrefixOf` msg then msg else "Parse error: " ++ msg) (mkSrcSpan sl sl) ctxt Nothing []
            Right m ->
                forM_ (applyHints [] rule [m]) $ \i ->
                    print i{ideaHint="", ideaTo=Nothing}


module HSE.All(
    module Language.Haskell.Exts,
    module HSE.Util, module HSE.Evaluate,
    module HSE.Bracket, module HSE.Match,
    module HSE.Operators,
    parseFile, parseString
    ) where

import Language.Haskell.Exts hiding (parseFile)
import qualified Language.Haskell.Exts as HSE

import HSE.Util
import HSE.Evaluate
import HSE.Bracket
import HSE.Match
import HSE.Operators



-- | On failure returns an empty module and prints to the console
parseFile :: FilePath -> IO Module
parseFile file = do
    res <- HSE.parseFile file
    case res of
        ParseOk x -> return $ hlintFixities x
        ParseFailed src msg -> do
            putStrLn $ showSrcLoc src ++ " Parse failure, " ++ limit 50 msg
            return $ Module nullSrcLoc (ModuleName "") [] Nothing Nothing [] []


-- | On failure crashes
parseString :: String -> String -> Module
parseString file src =
    case parseFileContents src of
        ParseOk x -> hlintFixities x
        _ -> error $ "Parse failure in " ++ file ++ "\n" ++ src


hlintFixities :: Module -> Module
hlintFixities = applyFixities (infix_ (-1) ["==>"] ++ preludeFixities)



module HSE.All(
    module Language.Haskell.Exts,
    module HSE.Util, module HSE.Evaluate,
    module HSE.Bracket, module HSE.Match,
    module HSE.All, module HSE.Operators
    ) where

import Language.Haskell.Exts
import HSE.Util
import HSE.Evaluate
import HSE.Bracket
import HSE.Match
import HSE.Operators



parseHsModule :: FilePath -> IO Module
parseHsModule file = do
    res <- parseFile file
    case res of
        ParseOk x -> return $ operatorPrec x
        ParseFailed src msg -> do
            putStrLn $ showSrcLoc src ++ " Parse failure, " ++ limit 50 msg
            return $ Module nullSrcLoc (ModuleName "") [] Nothing Nothing [] []


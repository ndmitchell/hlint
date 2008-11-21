
module Hint.All(readHints) where

import Control.Monad
import Util
import Type
import Language.Haskell.Exts

import Hint.Match
import Hint.List
import Hint.Monad
import Hint.Lambda
import Hint.Bracket


hints :: [(String,Hint)]
hints = let (*) = (,) in
    ["List"    * listHint
    ,"Monad"   * monadHint
    ,"Lambda"  * lambdaHint
    ,"Bracket" * bracketHint
    ]


readHints :: [FilePath] -> IO Hint
readHints = liftM (concatHints . concat) . mapM readHint


readHint :: FilePath -> IO [Hint]
readHint file = do
    modu <- parseHsModule file
    return $ readMatch modu : map snd hints

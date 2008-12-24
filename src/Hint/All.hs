
module Hint.All(readHints, allHints) where

import Control.Monad
import HSE.Util
import Type
import Language.Haskell.Exts

import Hint.Match
import Hint.List
import Hint.Monad
import Hint.Lambda
import Hint.Bracket


allHints :: [(String,Hint)]
allHints =
    let (*) = (,) in
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
    return $ readMatch modu : map snd allHints

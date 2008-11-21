
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


hints :: [(String,Module -> Hint)]
hints = let (*) = (,) in
    ["Match"   * readMatch
    ,"List"    * const listHint
    ,"Monad"   * const monadHint
    ,"Lambda"  * const lambdaHint
    ,"Bracket" * const bracketHint
    ]


readHints :: [FilePath] -> IO Hint
readHints = liftM (concatHints . concat) . mapM readHint


readHint :: FilePath -> IO [Hint]
readHint file = do
    modu <- parseHsModule file
    return $ map (($ modu) . snd) hints

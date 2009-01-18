
module Hint.All(readHints, allHints) where

import Control.Monad
import HSE.All
import Type

import Hint.Match
import Hint.List
import Hint.ListRec
import Hint.Monad
import Hint.Lambda
import Hint.Bracket
import Hint.Naming
import Hint.Structure


allHints :: [(String,Hint)]
allHints =
    let (*) = (,) in
    ["List"      * listHint
    ,"ListRec"   * listRecHint
    ,"Monad"     * monadHint
    ,"Lambda"    * lambdaHint
    ,"Bracket"   * bracketHint
    ,"Naming"    * namingHint
    ,"Structure" * structureHint
    ]


readHints :: [Setting] -> Hint
readHints settings = concatHints $ readMatch settings : map snd allHints


module Hint.All where

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


staticHints :: [(String,Hint)]
staticHints =
    let (*) = (,) in
    ["List"      * listHint
    ,"ListRec"   * listRecHint
    ,"Monad"     * monadHint
    ,"Lambda"    * lambdaHint
    ,"Bracket"   * bracketHint
    ,"Naming"    * namingHint
    ,"Structure" * structureHint
    ]

dynamicHints :: [Setting] -> Hint
dynamicHints = readMatch


allHints :: [Setting] -> Hint
allHints xs = concatHints $ dynamicHints xs : map snd staticHints

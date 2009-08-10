
module Hint.All where

import Type

import Hint.Match
import Hint.List
import Hint.ListRec
import Hint.Monad
import Hint.Lambda
import Hint.Bracket
import Hint.Naming
import Hint.Structure
import Hint.Import
import Hint.Pragma
import Hint.Extensions


staticHints :: [(String,Hint)]
staticHints =
    let x*y = (x,DeclHint y) ; x+y = (x,ModuHint y) in
    ["List"       * listHint
    ,"ListRec"    * listRecHint
    ,"Monad"      * monadHint
    ,"Lambda"     * lambdaHint
    ,"Bracket"    * bracketHint
    ,"Naming"     * namingHint
    ,"Structure"  * structureHint
    ,"Import"     + importHint
    ,"Pragma"     + pragmaHint
    ,"Extensions" + extensionsHint
    ]

dynamicHints :: [Setting] -> Hint
dynamicHints = DeclHint . readMatch


allHints :: [Setting] -> [Hint]
allHints xs = dynamicHints xs : map snd staticHints

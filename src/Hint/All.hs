
module Hint.All(
    Hint(..), DeclHint, ModuHint,
    staticHints, dynamicHints
    ) where

import Settings
import Data.Monoid
import Hint.Type

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
import Hint.Duplicate


staticHints :: [(String,Hint)]
staticHints =
    ["List"       ! listHint
    ,"ListRec"    ! listRecHint
    ,"Monad"      ! monadHint
    ,"Lambda"     ! lambdaHint
    ,"Bracket"    ! bracketHint
    ,"Naming"     ! namingHint
    ,"Structure"  ! structureHint
    ,"Import"     + importHint
    ,"Pragma"     + pragmaHint
    ,"Extensions" + extensionsHint
    ,"Duplicate"  * duplicateHint
    ]
    where
        x!y = (x,mempty{hintDecl=y})
        x+y = (x,mempty{hintModule=y})
        x*y = (x,mempty{hintModules=y})

dynamicHints :: [Setting] -> Hint
dynamicHints xs = mempty{hintDecl=readMatch xs}

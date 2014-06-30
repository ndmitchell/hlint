
module Hint.All(
    Hint(..), DeclHint, ModuHint,
    builtinHints, hintRules
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
import Hint.Comment


-- | A list of builtin hints, currently including entries such as @\"List\"@ and @\"Bracket\"@.
builtinHints :: [(String,Hint)]
builtinHints =
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
    ,"Comment"    - commentHint
    ]
    where
        x!y = (x,mempty{hintDecl=y})
        x+y = (x,mempty{hintModule=y})
        x*y = (x,mempty{hintModules=y})
        x-y = (x,mempty{hintComment=y})

-- | Transform a list of 'HintRule' into a 'Hint'.
hintRules :: [HintRule] -> Hint
hintRules xs = mempty{hintDecl=readMatch xs}

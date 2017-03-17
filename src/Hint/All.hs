
module Hint.All(
    Hint(..), HintBuiltin(..), DeclHint, ModuHint,
    resolveHints, hintRules, builtinHints
    ) where

import Data.Monoid
import Config.Type
import Data.Either
import Data.List
import Hint.Type
import Prelude

import Hint.Match
import Hint.List
import Hint.ListRec
import Hint.Monad
import Hint.Lambda
import Hint.Bracket
import Hint.Naming
import Hint.Pattern
import Hint.Import
import Hint.Export
import Hint.Pragma
import Hint.Restrict
import Hint.Extensions
import Hint.Duplicate
import Hint.Comment
import Hint.Unsafe
import Hint.NewType

-- | A list of the builtin hints wired into HLint.
--   This list is likely to grow over time.
data HintBuiltin =
    HintList | HintListRec | HintMonad | HintLambda |
    HintBracket | HintNaming | HintPattern | HintImport | HintExport |
    HintPragma | HintExtensions | HintUnsafe | HintDuplicate | HintRestrict |
    HintComment | HintNewType
    deriving (Show,Eq,Ord,Bounded,Enum)


builtin :: HintBuiltin -> Hint
builtin x = case x of
    HintList       -> decl listHint
    HintListRec    -> decl listRecHint
    HintMonad      -> decl monadHint
    HintLambda     -> decl lambdaHint
    HintBracket    -> decl bracketHint
    HintNaming     -> decl namingHint
    HintPattern    -> decl patternHint
    HintImport     -> modu importHint
    HintExport     -> modu exportHint
    HintPragma     -> modu pragmaHint
    HintExtensions -> modu extensionsHint
    HintUnsafe     -> modu unsafeHint
    HintDuplicate  -> mods duplicateHint
    HintComment    -> comm commentHint
    HintNewType    -> decl newtypeHint
    HintRestrict   -> mempty{hintModule=restrictHint}
    where
        decl x = mempty{hintDecl=const x}
        modu x = mempty{hintModule=const x}
        mods x = mempty{hintModules=const x}
        comm x = mempty{hintComment=const x}


-- | A list of builtin hints, currently including entries such as @\"List\"@ and @\"Bracket\"@.
builtinHints :: [(String, Hint)]
builtinHints = [(drop 4 $ show h, builtin h) | h <- [minBound .. maxBound]]

-- | Transform a list of 'HintBuiltin' or 'HintRule' into a 'Hint'.
resolveHints :: [Either HintBuiltin HintRule] -> Hint
resolveHints xs = mconcat $ mempty{hintDecl=const $ readMatch rights} : map builtin (nub lefts)
    where (lefts,rights) = partitionEithers xs

-- | Transform a list of 'HintRule' into a 'Hint'.
hintRules :: [HintRule] -> Hint
hintRules = resolveHints . map Right

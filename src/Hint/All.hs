
module Hint.All(
    Hint(..), DeclHint, ModuHint,
    resolveHints, hintRules, builtinHints
    ) where

import Data.Monoid
import Config.Type
import Data.Either
import Data.List.Extra
import Hint.Type
import Timing
import Util
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
import Hint.Smell

-- | A list of the builtin hints wired into HLint.
--   This list is likely to grow over time.
data HintBuiltin =
    HintList | HintListRec | HintMonad | HintLambda |
    HintBracket | HintNaming | HintPattern | HintImport | HintExport |
    HintPragma | HintExtensions | HintUnsafe | HintDuplicate | HintRestrict |
    HintComment | HintNewType | HintSmell
    deriving (Show,Eq,Ord,Bounded,Enum)


builtin :: HintBuiltin -> Hint
builtin x = case x of
    HintList       -> decl listHint
    HintListRec    -> decl listRecHint
    HintMonad      -> decl monadHint
    HintLambda     -> decl lambdaHint
    HintBracket    -> decl bracketHint
    HintNaming     -> decl' namingHint
    HintPattern    -> decl patternHint
    HintImport     -> modu importHint
    HintExport     -> modu exportHint
    HintComment    -> modu commentHint
    HintPragma     -> modu pragmaHint
    HintExtensions -> modu extensionsHint
    HintUnsafe     -> decl' unsafeHint
    HintDuplicate  -> mods duplicateHint
    HintNewType    -> decl' newtypeHint
    HintRestrict   -> mempty{hintModule=restrictHint}
    HintSmell      -> mempty{hintDecl=smellHint,hintModule=smellModuleHint}
    where
        wrap = timed "Hint" (drop 4 $ show x) . forceList
        decl f = mempty{hintDecl=const $ \a b c -> wrap $ f a b c}
        decl' f = mempty{hintDecl'=const $ \a b c -> wrap $ f a b c}
        modu f = mempty{hintModule=const $ \a b -> wrap $ f a b}
        mods f = mempty{hintModules=const $ \a -> wrap $ f a}

-- | A list of builtin hints, currently including entries such as @\"List\"@ and @\"Bracket\"@.
builtinHints :: [(String, Hint)]
builtinHints = [(drop 4 $ show h, builtin h) | h <- [minBound .. maxBound]]

-- | Transform a list of 'HintBuiltin' or 'HintRule' into a 'Hint'.
resolveHints :: [Either HintBuiltin HintRule] -> Hint
resolveHints xs = mconcat $ mempty{hintDecl=const $ readMatch rights} : map builtin (nubOrd lefts)
    where (lefts,rights) = partitionEithers xs

-- | Transform a list of 'HintRule' into a 'Hint'.
hintRules :: [HintRule] -> Hint
hintRules = resolveHints . map Right

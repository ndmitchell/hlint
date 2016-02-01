
module Hint.All(
    Hint(..), HintBuiltin(..), DeclHint, ModuHint,
    resolveHints, hintRules, resolveBuiltin, builtinHints
    ) where

import Data.Monoid
import Settings
import Data.Either
import Data.List
import Data.Maybe
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
import Hint.Pragma
import Hint.Extensions
import Hint.Duplicate
import Hint.Comment
import Hint.Unsafe

-- | A list of the builtin hints wired into HLint.
--   This list is likely to grow over time.
data HintBuiltin =
    HintList | HintListRec | HintMonad | HintLambda |
    HintBracket | HintNaming | HintPattern | HintImport |
    HintPragma | HintExtensions | HintUnsafe | HintDuplicate |
    HintComment
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
    HintPragma     -> modu pragmaHint
    HintExtensions -> modu extensionsHint
    HintUnsafe     -> modu unsafeHint
    HintDuplicate  -> mods duplicateHint
    HintComment    -> comm commentHint
    where
        decl x = mempty{hintDecl=x}
        modu x = mempty{hintModule=x}
        mods x = mempty{hintModules=x}
        comm x = mempty{hintComment=x}


-- | A list of builtin hints, currently including entries such as @\"List\"@ and @\"Bracket\"@.
builtinHints :: [(String, Hint)]
builtinHints = [(drop 4 $ show h, resolveHints [Left h]) | h <- [minBound .. maxBound]]

-- | Transform a list of 'HintRule' into a 'Hint'.
resolveHints :: [Either HintBuiltin HintRule] -> Hint
resolveHints xs = mconcat $ mempty{hintDecl=readMatch rights} : map builtin (nub lefts)
    where (lefts,rights) = partitionEithers xs

resolveBuiltin :: [String] -> [Hint]
resolveBuiltin builtin = map f $ nub $ concat [if x == "All" then map fst builtinHints else [x] | x <- builtin]
    where f x = fromMaybe (error $ "Unknown builtin hints: HLint.Builtin." ++ x) $ lookup x builtinHints

hintRules :: [HintRule] -> Hint
hintRules = resolveHints . map Right

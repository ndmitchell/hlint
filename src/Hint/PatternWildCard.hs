{-# LANGUAGE CPP #-}
{-
    Warn against wildcards in pattern

<TEST>
foo (case x of { Foo _ -> spam }) -- @Ignore ???
case x of { Foo (Spam (Eggs _)) -> spam } -- @Ignore ???
case x of { Foo _ -> spam } -- @Ignore ???
case x of { Foo bar -> spam }
foo (case x of { Foo bar -> spam })
</TEST>
-}

module Hint.PatternWildCard (patternWildCardHint)
where

import Hint.Type (DeclHint, ignoreNoSuggestion, Idea)
import GHC.Hs
import GHC.Types.SrcLoc
import Data.Generics.Uniplate.DataOnly

patternWildCardHint :: DeclHint
patternWildCardHint _ _ code = concatMap inspectCode $ childrenBi code

inspectCode :: LHsExpr GhcPs -> [Idea]
#if __GLASGOW_HASKELL__ >= 906
inspectCode (L _ ((HsCase _ _ (MG _ (L _ cases))))) = concatMap inspectCase cases
#else
inspectCode (L _ ((HsCase _ _ (MG _ (L _ cases) _)))) = concatMap inspectCase cases
#endif
inspectCode o = concatMap inspectCode $ children o

inspectCase :: LMatch GhcPs (LHsExpr GhcPs) -> [Idea]
inspectCase c@(L _ (Match _ _ pats _)) = concatMap inspectPat pats

inspectPat :: LPat GhcPs -> [Idea]
inspectPat c@(L _ (WildPat _)) = [ignoreNoSuggestion "Don't use wildcard in pattern match" (reLoc c)]
inspectPat o = concatMap inspectPat $ children o

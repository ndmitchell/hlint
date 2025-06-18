{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, PatternGuards, RecordWildCards #-}
{-
    Reduce the number of import declarations.
    Two import declarations can be combined if:
      (note, A[] is A with whatever import list, or none)

    import A[]; import A[] = import A[]
    import A(B); import A(C) = import A(B,C)
    import A; import A(C) = import A
    import A; import A hiding (C) = import A
    import A[]; import A[] as Y = import A[] as Y

<TEST>
import A
import A; import A -- import A
import A; import A; import A -- import A
import A(Foo) ; import A -- import A
import A ;import A(Foo) -- import A
import A(Bar(..)); import {-# SOURCE #-} A
import A; import B
import A(B) ; import A(C) -- import A(B,C)
import A; import A hiding (C) -- import A
import A; import A as Y -- import A as Y
import A; import qualified A as Y
import A as B; import A as C
import A as A -- import A
import qualified A as A -- import qualified A
import A; import B; import A -- import A
import qualified A; import A
import B; import A; import A -- import A
import A hiding(Foo); import A hiding(Bar)
import A (foo) \
import A (fie) \
import A (bar) \
import A (baz) -- import A (foo, fie, bar, baz)
import A (Foo(A), Foo(B)) -- import A (Foo(A,B))
import A (Foo(A), Bar(C,D), Foo(B,C), Baz(E,F)) -- import A (Foo(A,B,C), Bar(C,D), Baz(E,F))
import A (Foo(A,B), Bar(C,D)) \
import A (Baz(E,F)) -- import A ( Foo(A, B), Bar(C, D), Baz(E, F) )
import A (Foo(A,B), Foo(C), Bar) \
import A (Foo(C,D), Baz) -- import A (Foo(A,B,C,D),Bar, Baz)
import A (Foo(A,B), Bar(C,D), Foo(E), Foo(C,D)) \
import A (Foo(A,F), Baz(D,A)) -- import A (Foo(A,B,C,D,E,F), Bar(C,D), Baz(D,A))
import qualified A (Foo(A,B), Foo(C), Bar) \
import qualified A (Foo(C,D), Baz) -- import qualified A (Foo(A,B,C,D),Bar, Baz)
import qualified A (Foo(A,B), Bar(C,D), Foo(E), Foo(C,D)) \
import qualified A (Foo(A,F)) -- import qualified A (Foo(A,B,C,D,E,F), Bar(C,D))
import qualified A as M (Foo(A,B), Foo(C), Bar) \
import qualified A as M (Foo(C,D), Baz) -- import qualified A as M (Foo(A,B,C,D), Bar, Baz)
import qualified A as M (Foo(A,B), Bar(C,D), Foo(E), Foo(C,D)) \
import qualified A as M (Foo(A,F)) -- import qualified A as M (Foo(A,B,C,D,E,F), Bar(C,D))
import A hiding (Foo(A,B), Bar, Foo(D), Foo(C,A)) -- import A hiding (Foo(A, B, C, D), Bar)
import A hiding ( RType(Match) )
import Refact.Types hiding ( RType(Pattern, Match), SrcSpan ) \
import Refact.Types qualified as R ( RType(Pattern, Match), SrcSpan )
import Hint.Type ( Idea, DeclHint, Note(DecreasesLaziness), ideaNote, ignoreNoSuggestion, suggestN )
</TEST>
-}

module Hint.Import(importHint) where

import Hint.Type(ModuHint,ModuleEx(..),Idea(..),Severity(..),suggest,toSSA,rawIdea)
import Refact.Types hiding (ModuleName)
import Refact.Types qualified as R
import Data.Tuple.Extra
import Data.List.Extra
import Data.Generics.Uniplate.DataOnly
import Data.Maybe
import Control.Applicative
import Control.Monad
import Prelude

import GHC.Data.FastString
import GHC.Types.SourceText
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Types.PkgQual

import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

rawPkgQualToMaybe :: RawPkgQual -> Maybe StringLiteral
rawPkgQualToMaybe x =
  case x of
    NoRawPkgQual -> Nothing
    RawPkgQual lit -> Just lit

importHint :: ModuHint
importHint _ ModuleEx {ghcModule=L _ HsModule{hsmodImports=ms}} =
  -- Ideas for combining multiple imports.
  concatMap (reduceImports . snd) (
    groupSort [((n, pkg), i) | i <- ms
              , ideclSource (unLoc i) == NotBoot
              , let i' = unLoc i
              , let n = unLoc $ ideclName i'
              , let pkg  = unpackFS . sl_fs <$> rawPkgQualToMaybe (ideclPkgQual i')]) ++
  -- Ideas for removing redundant 'as' clauses.
  concatMap stripRedundantAlias ms

reduceImports :: [LImportDecl GhcPs] -> [Idea]
reduceImports [] = []
reduceImports ms@(m:_) =
  [rawIdea Hint.Type.Warning "Use fewer imports" (locA (getLoc m)) (f ms) (Just $ f x) [] rs
  | Just (x, rs) <- [simplify ms]]
  where f = unlines . map unsafePrettyPrint

{-
    The simplify function combines multiple import declarations for any
    given module into a single import declaration. It also combines multiple
    imports of the same field for any imported class, type, or constructor. It
    will combine imports like:
    ```haskell
    import A (Foo(A,B), Bar(C,D))
    import A (Baz(E,F))
    ```
    into:
    ```haskell
    import A (Foo(A,B), Bar(C,D), Baz(E,F))
    ```
    Both simplifications will be applied. For example, the imports:
    ```haskell
    import A (Foo(A,B), Bar(C,D), Foo(E), Foo(C,D))
    import A (Foo(A,F), Baz(D,A))
    ```
    would first be simplified to combine the imports:
    ```haskell
    import A (Foo(A,B), Bar(C,D), Foo(E), Foo(C,D), Foo(A,F), Baz(D,A))
    ```
    and then further simplified to combine the fields:
    ```haskell
    import A (Foo(A,B,C,D,E,F), Bar(C,D), Baz(D,A))
    ```

    Note: types, constructors, and classes which have multiple field imports
    in the same declaration will have those fields sorted and deduplicated,
    but things with a single field import will not. The sorting is a result
    of how deduplication is performed. If this is not desired, this could be
    rewritten to preserve the order of the fields. We do this already for
    import lists at the type, etc. level (see `combine` within
    `combineFields`, within`combineFieldsInDecl` below).
-}
simplify :: [LImportDecl GhcPs]
         -> Maybe ([LImportDecl GhcPs], [Refactoring R.SrcSpan])
simplify [] = Nothing
simplify (x : xs) = case simplifyDecl x xs of
  Nothing -> simplifyFields x $ simplify xs
  Just (xs, rs) -> do
    let deletions = filter (\case Delete{} -> True; _ -> False) rs
    pure $ maybe (xs, rs) (second (++ deletions)) $ simplify xs
  where
    -- Given a single import declaration, and a list of other import
    -- declarations, try to combine the first with one of the others.
    -- If successful, return the new combined decl, and the refactorings
    -- needed to replace and delete the other decl. If not, return the
    -- original decl, and the refactorings to delete the other decl.
    simplifyDecl :: LImportDecl GhcPs
                 -> [LImportDecl GhcPs]
                 -> Maybe ([LImportDecl GhcPs], [Refactoring R.SrcSpan])
    simplifyDecl x (y : ys) = case combineDecls x y of
        Nothing -> first (x:) <$> simplify ys
            -- If the decls x and y were not combined, we first recursively
            -- simplify the rest of the decls. We add x to the result when
            -- there was a simplification, as in that case, we need to check
            -- whether x combines with any of the new decls. Since y didn't
            -- merge with x, we can drop it.
        Just (xy, rs) -> simplifyFields xy $ Just (ys, rs)
            -- If the decls x and y were combined, then simplify the
            -- fields in that new combined decl. `simplifyFields` defaults
            -- `(xy:ys,rs)` when no fields simplification is found.
    simplifyDecl x [] = Nothing

    -- Try to combine two import declarations. 
    combineDecls :: LImportDecl GhcPs
                 -> LImportDecl GhcPs
                 -> Maybe (LImportDecl GhcPs, [Refactoring R.SrcSpan])
    combineDecls x@(L loc x') y@(L _ y')
      -- Both (un/)qualified, common 'as', same names: Delete the second.
      | qual, as, specs = Just (x, [Delete Import (toSSA y)])
      -- Both (un/)qualified, common 'as', different names: Merge the
      -- second into the first and delete it.
      | qual, as
      , Just (False, xs) <- first (== EverythingBut) <$> ideclImportList x'
      , Just (False, ys) <- first (== EverythingBut) <$> ideclImportList y' =
          let newImp = L loc x'{ideclImportList = Just (Exactly, noLocA (unLoc xs ++ unLoc ys))}
          in Just (newImp, [Replace Import (toSSA x) [] (unsafePrettyPrint (unLoc newImp))
                          , Delete Import (toSSA y)])
      -- Both (un/)qualified, common 'as', one has names the other doesn't:
      -- Delete the one with names.
      | qual, as, isNothing (ideclImportList x') || isNothing (ideclImportList y') =
          let (newImp, toDelete) = if isNothing (ideclImportList x') then (x, y) else (y, x)
          in Just (newImp, [Delete Import (toSSA toDelete)])
      -- Both unqualified, same names, one (and only one) has an 'as'
      -- clause: Delete the one without an 'as'.
      | ideclQualified x' == NotQualified, qual, specs, length ass == 1 =
          let (newImp, toDelete) = if isJust (ideclAs x') then (x, y) else (y, x)
          in Just (newImp, [Delete Import (toSSA toDelete)])
      -- No hints.
      | otherwise = Nothing
        where
            eqMaybe:: Eq a => Maybe (LocatedA a) -> Maybe (LocatedA a) -> Bool
            eqMaybe (Just x) (Just y) = x `eqLocated` y
            eqMaybe Nothing Nothing = True
            eqMaybe _ _ = False

            qual = ideclQualified x' == ideclQualified y'
            as = ideclAs x' `eqMaybe` ideclAs y'
            ass = mapMaybe ideclAs [x', y']
            specs = transformBi (const noSrcSpan) (ideclImportList x') ==
                        transformBi (const noSrcSpan) (ideclImportList y')

    -- Given a single import declaration x, simplify it by merging multiple
    -- field list instances that belong to the same type, constructor, or
    -- class. If successful, add the new combined decl and its refactorings
    -- to the current simplification "state"; if not successful, leave it
    -- unchanged.
    simplifyFields :: LImportDecl GhcPs
      -> Maybe ([LImportDecl GhcPs], [Refactoring R.SrcSpan])
      -> Maybe ([LImportDecl GhcPs], [Refactoring R.SrcSpan])
    simplifyFields x xrs
      | Just (xs, rs) <- xrs = do
        let (y, ss) = fromMaybe (x, []) $ combineFieldsInDecl x
        pure (y:xs, overrideReplaces ss rs)
      | otherwise = first (:[]) <$> combineFieldsInDecl x
      where
        overrideReplaces :: [Refactoring R.SrcSpan] -> [Refactoring R.SrcSpan] -> [Refactoring R.SrcSpan]
        overrideReplaces [] rs = rs
        overrideReplaces ss [] = ss
        overrideReplaces (s@(Replace _ sp _ _):ss) rs =
          s : overrideReplaces ss (filter (sameSpanReplace sp) rs)
            -- If a `Replace` refactoring is already present for this span, drop it,
            -- and add the new refactoring. Other refactoring types (e.g. `Delete`)
            -- are preserved, by virtue of `replaceToSSA` yielding `Nothing` for
            -- all but `Replace`. Then, recursively override using the rest of
            -- the new refactorings.
          where
            sameSpanReplace :: R.SrcSpan -> Refactoring R.SrcSpan -> Bool
            sameSpanReplace sp1 (Replace _ sp2 _ _) = sp1 == sp2
            sameSpanReplace _ _ = False

    combineFieldsInDecl :: LImportDecl GhcPs -> Maybe (LImportDecl GhcPs, [Refactoring R.SrcSpan])
    combineFieldsInDecl lid@(L loc il) = do
      (hidden, ies) <- ideclImportList il
      new_ies <- combineFields $ unLoc ies
      -- Construct a new import declaration with the combined `IEThingWith` entries
      let new_lid = L loc (il {ideclImportList = Just (hidden, noLocA new_ies)})
      -- Return the new import declaration and a new `Replace` refactoring
      pure (new_lid, [Replace Import (toSSA new_lid) [] (unsafePrettyPrint new_lid)])
      where
        -- Preserving the relative order of the "things" (class or type) in an
        -- import declaration import list elements, combine fields for any
        -- multiple instances of the same "thing".
        combineFields :: [LIE GhcPs] -> Maybe [LIE GhcPs]
        combineFields ies = do
          guard $ hasMultipleFieldsImports ies -- nothing to do if there are no fields exported
          pure $ combine ies
          where
            hasMultipleFieldsImports :: [LIE GhcPs] -> Bool
            hasMultipleFieldsImports
              = any ((>1) . length)
              . groupSortOn nameKey
              . mapMaybe thingOf

            combine :: [LIE GhcPs] -> [LIE GhcPs]
            combine [] = []
            combine (L loc (IEThingWith src thing wild fields xd):ies)
              | not $ null same = new_ie : combine others
              where
                (same, others) = partition (sameThing thing) ies
                extras = concatMap (\case L _ (IEThingWith _ _ _ fs _) -> fs; _ -> []) same
                new_ie = L loc (IEThingWith src thing wild (nubSortOn nameKey (fields ++ extras)) xd)
            combine (ie:ies) = ie : combine ies

            sameThing :: LIEWrappedName GhcPs -> LIE GhcPs -> Bool
            sameThing t1 (L _ (IEThingWith _ t2 _ _ _)) = nameKey t1 == nameKey t2
            sameThing _ _ = False

            thingOf :: LIE GhcPs -> Maybe (LIEWrappedName GhcPs)
            thingOf (L _ (IEThingWith _ thing _ _ _)) = Just thing
            thingOf _ = Nothing

            -- Overkill; is there a better/idiomatic way to do this? We need
            -- an Ord instances for `LIEWrappedName pass`.
            nameKey :: LIEWrappedName GhcPs -> String
            nameKey = unsafePrettyPrint

stripRedundantAlias :: LImportDecl GhcPs -> [Idea]
stripRedundantAlias x@(L _ i@ImportDecl {..})
  -- Suggest 'import M as M' be just 'import M'.
  | Just (unLoc ideclName) == fmap unLoc ideclAs =
      [suggest "Redundant as" (reLoc x) (noLoc i{ideclAs=Nothing} :: Located (ImportDecl GhcPs)) [RemoveAsKeyword (toSSA x)]]
stripRedundantAlias _ = []

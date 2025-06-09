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
import A (bar) \
import A (baz) -- import A ( foo, bar, baz )
import A (Foo(A,B), Foo(C), Bar) \
import A (Foo(C,D), Baz) -- import A (Bar, Baz, Foo(A,B,C,D))
import A (Foo(A,B), Bar(C,D), Foo(E), Foo(C,D)) \
import A (Foo(A,F), Baz(D,A)) -- import A (Bar(C,D), Baz(D,A), Foo(A,B,C,D,E,F))
import qualified A (Foo(A,B), Foo(C), Bar) \
import qualified A (Foo(C,D), Baz) -- import qualified A (Bar, Baz, Foo(A,B,C,D))
import qualified A (Foo(A,B), Bar(C,D), Foo(E), Foo(C,D)) \
import qualified A (Foo(A,F)) -- import qualified A (Bar(C,D), Foo(A,B,C,D,E,F))
import qualified A as M (Foo(A,B), Foo(C), Bar) \
import qualified A as M (Foo(C,D), Baz) -- import qualified A as M (Bar, Baz, Foo(A,B,C,D))
import qualified A as M (Foo(A,B), Bar(C,D), Foo(E), Foo(C,D)) \
import qualified A as M (Foo(A,F)) -- import qualified A as M (Bar(C,D), Foo(A,B,C,D,E,F))
import A hiding (Foo(A,B), Bar, Foo(D), Foo(C,A))
</TEST>
-}


module Hint.Import(importHint) where

import Hint.Type(ModuHint,ModuleEx(..),Idea(..),Severity(..),suggest,toSSA,rawIdea)
import Refact.Types hiding (ModuleName)
import Refact.Types qualified as R
import Data.Tuple.Extra
import Data.List.Extra hiding (groupBy)
import Data.List.NonEmpty (NonEmpty(..), groupBy)
import Data.Function (on)
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
  | Just (x, rs) <- [simplifyThingsWith <$> simplify ms]]
  where f = unlines . map unsafePrettyPrint

{-
    Combine multiple fields for any imported "thing" (type or class). Provided
    any given "thing" (type or class) appears in only one non-hiding import
    declaration (i.e., as guaranteed by simplify), this will combine
    each thing's explicit field imports into a single import list element.

    Hiding imports are left untouched.

    For example:
    ```haskell
    import A (Foo(A,B), Bar(C,D), Foo(E), Foo(C,D))
    import A (Foo(A,F), Baz(D,A))
    ```
    would be `simplify`d to:
    ```haskell
    import A (Foo(A,B), Bar(C,D), Foo(E), Foo(C,D), Foo(A,F), Baz(D,A))
    ```
    and then `simplifyThingsWith` would yield:
    ```haskell
    import A (Bar(C,D), Baz(D,A), Foo(A,B,C,D,E,F))
    ```

    The refactoring returned will replace the original import declarations
    with the new one, and drop any (now) obsolete refactorings.

    Note: things which have multiple field imports in the same declaration
    will have those fields sorted and deduplicated, but things with a single
    field import will not. The sorting is for the nub to dedupe properly,
    but the sort is done wrt Ord String (cf. unsafePrettyPrint), so might be
    "wrong" when unicode identifiers are involved (or it might be spot on).
    Whether this matters is another question.
-}
simplifyThingsWith :: ([LImportDecl GhcPs], [Refactoring R.SrcSpan])
               -> ([LImportDecl GhcPs], [Refactoring R.SrcSpan])
simplifyThingsWith (xs, rs) = fromMaybe (xs, rs) $ do
  let (ys, ss) = second concat . unzip $ map combineThingFields xs
  guard $ not (null ys)
  return (ys, override ss rs)
  where
    override :: [Refactoring R.SrcSpan] -> [Refactoring R.SrcSpan] -> [Refactoring R.SrcSpan]
    override [] rs = rs
    override ss [] = ss
    override (s@(Replace _ sp _ _):ss) rs =
      s : override ss (filter (\r -> toSSA r /= Just sp) rs)
      -- If a `Replace` refactoring is already present for this span, drop it,
      -- and add the new refactoring. Recursively, override with the rest of
      -- the new refactorings.
      where
        toSSA :: Refactoring R.SrcSpan -> Maybe R.SrcSpan
        toSSA (Replace _ sp _ _) = Just sp
        toSSA _ = Nothing

    combineThingFields :: LImportDecl GhcPs -> (LImportDecl GhcPs, [Refactoring R.SrcSpan])
    combineThingFields lid@(L loc ie) = fromMaybe (lid, []) $ do
      (hidden, is) <- ideclImportList ie
      let (things_with, rest) =
            partition (\case L _ (IEThingWith{}) -> True; _ -> False) (unLoc is)
      guard $ not (null things_with)
      let new_is = map combineFields
              -- Combine the fields of each group into a single
              -- `IEThingWith` entry.
            $ groupBy ((==) `on` thingKey)
              -- The sort ensures that all `IEThingWith` entries for the
              -- same "thing" appear in the same group. (This is redundant
              -- if an earlier pass has already sorted them.)
            $ sortBy (compare `on` thingKey) things_with
      
      let new_lid = L loc (ie {ideclImportList = Just (hidden, noLocA $ rest ++ concat new_is)})
      return (new_lid, [Replace Import (toSSA lid) [] (unsafePrettyPrint new_lid)])
      where
        -- Overkill; is there a better/idiomatic way to do this? We need
        -- an Ord instances for `LIEWrappedName pass`.
        nameKey :: LIEWrappedName GhcPs -> String
        nameKey = unsafePrettyPrint

        thingKey :: LIE GhcPs -> String
        thingKey (L _ (IEThingWith _ thing _ _ _)) = nameKey thing
        thingKey _ = ""

        addFields :: [LIEWrappedName GhcPs] -> LIE GhcPs -> LIE GhcPs
        addFields fs' (L loc (IEThingWith src thing wild fs xd)) =
          L loc (IEThingWith src thing wild (nubSortBy (compare `on` nameKey) $ fs ++ fs') xd)
            -- Use `nubSortBy` to ensure that we don't have duplicate fields, which
            -- seems possible, given that we're in `pass` `GhcPs`.
        addFields _ lie = lie

        -- Combine the fields of all `IEThingWith` entries for this "thing"
        -- (type or class).
        combineFields :: NonEmpty (LIE GhcPs) -> [LIE GhcPs]
        combineFields (ie :| ies)
          | null fields = ie:ies
          | otherwise = [addFields fields ie]
          where
            fields = concat $
              mapMaybe (\case L _ (IEThingWith _ _ _ fs _) -> Just fs; _ -> Nothing) ies

simplify :: [LImportDecl GhcPs]
         -> Maybe ([LImportDecl GhcPs], [Refactoring R.SrcSpan])
simplify [] = Nothing
simplify (x : xs) = case simplifyHead x xs of
    Nothing -> first (x:) <$> simplify xs
    Just (xs, rs) ->
      let deletions = filter (\case Delete{} -> True; _ -> False) rs
       in Just $ maybe (xs, rs) (second (++ deletions)) $ simplify xs

simplifyHead :: LImportDecl GhcPs
             -> [LImportDecl GhcPs]
             -> Maybe ([LImportDecl GhcPs], [Refactoring R.SrcSpan])
simplifyHead x (y : ys) = case combine x y of
    Nothing -> first (y:) <$> simplifyHead x ys
    Just (xy, rs) -> Just (xy : ys, rs)
simplifyHead x [] = Nothing

combine :: LImportDecl GhcPs
        -> LImportDecl GhcPs
        -> Maybe (LImportDecl GhcPs, [Refactoring R.SrcSpan])
combine x@(L loc x') y@(L _ y')
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

stripRedundantAlias :: LImportDecl GhcPs -> [Idea]
stripRedundantAlias x@(L _ i@ImportDecl {..})
  -- Suggest 'import M as M' be just 'import M'.
  | Just (unLoc ideclName) == fmap unLoc ideclAs =
      [suggest "Redundant as" (reLoc x) (noLoc i{ideclAs=Nothing} :: Located (ImportDecl GhcPs)) [RemoveAsKeyword (toSSA x)]]
stripRedundantAlias _ = []

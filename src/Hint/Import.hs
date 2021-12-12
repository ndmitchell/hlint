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
</TEST>
-}


module Hint.Import(importHint) where

import Hint.Type(ModuHint,ModuleEx(..),Idea(..),Severity(..),suggest,toSSA,rawIdea)
import Refact.Types hiding (ModuleName)
import qualified Refact.Types as R
import Data.Tuple.Extra
import Data.List.Extra
import Data.Generics.Uniplate.DataOnly
import Data.Maybe
import Control.Applicative
import Prelude

import GHC.Data.FastString
import GHC.Types.SourceText
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Unit.Types -- for 'NotBoot'

import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

importHint :: ModuHint
importHint _ ModuleEx {ghcModule=L _ HsModule{hsmodImports=ms}} =
  -- Ideas for combining multiple imports.
  concatMap (reduceImports . snd) (
    groupSort [((n, pkg), i) | i <- ms
              , ideclSource (unLoc i) == NotBoot
              , let i' = unLoc i
              , let n = unLoc $ ideclName i'
              , let pkg  = unpackFS . sl_fs <$> ideclPkgQual i']) ++
  -- Ideas for removing redundant 'as' clauses.
  concatMap stripRedundantAlias ms

reduceImports :: [LImportDecl GhcPs] -> [Idea]
reduceImports [] = []
reduceImports ms@(m:_) =
  [rawIdea Hint.Type.Warning "Use fewer imports" (locA (getLoc m)) (f ms) (Just $ f x) [] rs
  | Just (x, rs) <- [simplify ms]]
  where f = unlines . map unsafePrettyPrint

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
  -- Both (un/)qualified, common 'as', same names : Delete the second.
  | qual, as, specs = Just (x, [Delete Import (toSSA y)])
    -- Both (un/)qualified, common 'as', different names : Merge the
    -- second into the first and delete it.
  | qual, as
  , Just (False, xs) <- ideclHiding x'
  , Just (False, ys) <- ideclHiding y' =
      let newImp = L loc x'{ideclHiding = Just (False, noLocA (unLoc xs ++ unLoc ys))}
      in Just (newImp, [Replace Import (toSSA x) [] (unsafePrettyPrint (unLoc newImp))
                       , Delete Import (toSSA y)])
  -- Both (un/qualified), common 'as', one has names the other doesn't
  -- : Delete the one with names.
  | qual, as, isNothing (ideclHiding x') || isNothing (ideclHiding y') =
       let (newImp, toDelete) = if isNothing (ideclHiding x') then (x, y) else (y, x)
       in Just (newImp, [Delete Import (toSSA toDelete)])
  -- Both unqualified, same names, one (and only one) has an 'as'
  -- clause : Delete the one without an 'as'.
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
        specs = transformBi (const noSrcSpan) (ideclHiding x') ==
                    transformBi (const noSrcSpan) (ideclHiding y')

stripRedundantAlias :: LImportDecl GhcPs -> [Idea]
stripRedundantAlias x@(L _ i@ImportDecl {..})
  -- Suggest 'import M as M' be just 'import M'.
  | Just (unLoc ideclName) == fmap unLoc ideclAs =
      [suggest "Redundant as" (reLoc x) (noLoc i{ideclAs=Nothing} :: Located (ImportDecl GhcPs)) [RemoveAsKeyword (toSSA x)]]
stripRedundantAlias _ = []

{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards #-}
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
import List -- import Data.List @NoRefactor: apply-refact bug
import qualified List -- import qualified Data.List as List @NoRefactor
import Char(foo) -- import Data.Char(foo) @NoRefactor
import IO(foo)
import IO as X -- import System.IO as X; import System.IO.Error as X; import Control.Exception  as X (bracket,bracket_) @NoRefactor
</TEST>
-}


module Hint.Import(importHint) where

import Hint.Type(ModuHint,ModuleEx(..),Idea(..),Severity(..),suggest',toSS',rawIdea',rawIdeaN')
import Refact.Types hiding (ModuleName)
import qualified Refact.Types as R
import Data.Tuple.Extra
import Data.List.Extra
import Data.Generics.Uniplate.Operations
import Data.Maybe
import Control.Applicative
import Prelude

import FastString
import BasicTypes
import RdrName
import Module
import HsSyn
import SrcLoc
import GHC.Util

importHint :: ModuHint
importHint _ ModuleEx {ghcModule=L _ HsModule{hsmodImports=ms}} =
  -- Ideas for combining multiple imports.
  concatMap (reduceImports . snd) (
    groupSort [((n, pkg), i) | i <- ms
              , not $ ideclSource (unLoc i)
              , let i' = unLoc i
              , let n = unLoc $ ideclName i'
              , let pkg  = unpackFS . sl_fs <$> ideclPkgQual i']) ++
  -- Ideas for removing redundant 'as' clauses.
  concatMap stripRedundantAlias ms ++
  -- Ideas for replacing deprecated imports by their preferred
  -- equivalents.
  concatMap preferHierarchicalImports ms

reduceImports :: [LImportDecl GhcPs] -> [Idea]
reduceImports [] = []
reduceImports ms@(m:_) =
  [rawIdea' Hint.Type.Warning "Use fewer imports" (getLoc m) (f ms) (Just $ f x) [] rs
  | Just (x, rs) <- [simplify ms]]
  where f = unlines . map unsafePrettyPrint

simplify :: [LImportDecl GhcPs]
         -> Maybe ([LImportDecl GhcPs], [Refactoring R.SrcSpan])
simplify [] = Nothing
simplify (x : xs) = case simplifyHead x xs of
    Nothing -> first (x:) <$> simplify xs
    Just (xs, rs) -> Just $ maybe (xs, rs) (second (++ rs)) $ simplify xs

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
combine x@(LL _ x') y@(LL _ y')
  -- Both (un/)qualified, common 'as', same names : Delete the second.
  | qual, as, specs = Just (x, [Delete Import (toSS' y)])
    -- Both (un/)qualified, common 'as', different names : Merge the
    -- second into the first and delete it.
  | qual, as
  , Just (False, xs) <- ideclHiding x'
  , Just (False, ys) <- ideclHiding y' =
      let newImp = noLoc x'{ideclHiding = Just (False, noLoc (unLoc xs ++ unLoc ys))}
      in Just (newImp, [Replace Import (toSS' x) [] (unsafePrettyPrint (unLoc newImp))
                       , Delete Import (toSS' y)])
  -- Both (un/qualified), common 'as', one has names the other doesn't
  -- : Delete the one with names.
  | qual, as, isNothing (ideclHiding x') || isNothing (ideclHiding y') =
       let (newImp, toDelete) = if isNothing (ideclHiding x') then (x, y) else (y, x)
       in Just (newImp, [Delete Import (toSS' toDelete)])
  -- Both unqualified, same names, one (and only one) has an 'as'
  -- clause : Delete the one without an 'as'.
  | not (ideclQualified x'), qual, specs, length ass == 1 =
       let (newImp, toDelete) = if isJust (ideclAs x') then (x, y) else (y, x)
       in Just (newImp, [Delete Import (toSS' toDelete)])
  -- No hints.
  | otherwise = Nothing
    where
        eqMaybe:: Eq a => Maybe (Located a) -> Maybe (Located a) -> Bool
        eqMaybe (Just x) (Just y) = x `eqLocated` y
        eqMaybe Nothing Nothing = True
        eqMaybe _ _ = False

        qual = ideclQualified x' == ideclQualified y'
        as = ideclAs x' `eqMaybe` ideclAs y'
        ass = mapMaybe ideclAs [x', y']
        specs = transformBi (const noSrcSpan) (ideclHiding x') ==
                    transformBi (const noSrcSpan) (ideclHiding y')
combine _ _ = Nothing -- {-# COMPLETE LL #-}

stripRedundantAlias :: LImportDecl GhcPs -> [Idea]
stripRedundantAlias x@(LL loc i@ImportDecl {..})
  -- Suggest 'import M as M' be just 'import M'.
  | Just (unLoc ideclName) == fmap unLoc ideclAs =
      [suggest' "Redundant as" x (cL loc i{ideclAs=Nothing} :: LImportDecl GhcPs) [RemoveAsKeyword (toSS' x)]]
stripRedundantAlias _ = []

preferHierarchicalImports :: LImportDecl GhcPs -> [Idea]
preferHierarchicalImports x@(LL loc i@ImportDecl{ideclName=L _ n,ideclPkgQual=Nothing})
  -- Suggest 'import IO' be rewritten 'import System.IO, import
  -- System.IO.Error, import Control.Exception(bracket, bracket_)'.
  | n == mkModuleName "IO" && isNothing (ideclHiding i) =
      [rawIdeaN' Suggestion "Use hierarchical imports" loc
      (trimStart $ unsafePrettyPrint i) (
          Just $ unlines $ map (trimStart . unsafePrettyPrint)
          [ f "System.IO" Nothing, f "System.IO.Error" Nothing
          , f "Control.Exception" $ Just (False, noLoc [mkLIE x | x <- ["bracket","bracket_"]])]) []]
  -- Suggest that a module import like 'Monad' should be rewritten with
  -- its hiearchical equivalent e.g. 'Control.Monad'.
  | Just y <- lookup (moduleNameString n) newNames =
    let newModuleName = y ++ "." ++ moduleNameString n
        r = [Replace R.ModuleName (toSS' x) [] newModuleName] in
    [suggest' "Use hierarchical imports"
     x (noLoc (desugarQual i){ideclName=noLoc (mkModuleName newModuleName)} :: LImportDecl GhcPs) r]
  where
    -- Substitute a new module name.
    f a b = (desugarQual i){ideclName=noLoc (mkModuleName a), ideclHiding=b}
    -- Wrap a literal name into an 'IE' (import/export) value.
    mkLIE :: String -> LIE GhcPs
    mkLIE n = noLoc $ IEVar noExt (noLoc (IEName (noLoc (mkVarUnqual (fsLit n)))))
    -- Rewrite 'import qualified X' as 'import qualified X as X'.
    desugarQual :: ImportDecl GhcPs -> ImportDecl GhcPs
    desugarQual i
      | ideclQualified i && isNothing (ideclAs i) = i{ideclAs = Just (ideclName i)}
      | otherwise = i

preferHierarchicalImports _ = []

newNames :: [(String, String)]
newNames = let (*) = flip (,) in
    ["Control" * "Monad"
    ,"Data" * "Char"
    ,"Data" * "List"
    ,"Data" * "Maybe"
    ,"Data" * "Ratio"
    ,"System" * "Directory"

    -- Special, see bug https://code.google.com/archive/p/ndmitchell/issues/393
    -- ,"System" * "IO"

    -- Do not encourage use of old-locale/old-time over haskell98
    -- ,"System" * "Locale"
    -- ,"System" * "Time"
    ]

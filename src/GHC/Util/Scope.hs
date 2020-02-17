{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Util.Scope (
   Scope'
  ,scopeCreate',scopeImports',scopeMatch',scopeMove'
) where

import HsSyn
import SrcLoc
import BasicTypes
import Module
import FastString
import RdrName
import OccName

import GHC.Util.Module
import GHC.Util.RdrName
import Outputable

import Data.List.Extra
import Data.Maybe

-- A scope is a list of import declarations.
newtype Scope' = Scope' [LImportDecl GhcPs]
               deriving (Outputable, Monoid, Semigroup)

-- Create a 'Scope' from a module's import declarations.
scopeCreate' :: HsModule GhcPs -> Scope'
scopeCreate' xs = Scope' $ [prelude | not $ any isPrelude res] ++ res
  where
    -- Package qualifier of an import declaration.
    pkg :: LImportDecl GhcPs -> Maybe StringLiteral
    pkg (LL _ x) = ideclPkgQual x
    pkg _  = Nothing -- {-# COMPLETE LL #-}

    -- The import declaraions contained by the module 'xs'.
    res :: [LImportDecl GhcPs]
    res = [x | x <- hsmodImports xs , pkg x /= Just (StringLiteral NoSourceText (fsLit "hint"))]

    -- Mock up an import declaraion corresponding to 'import Prelude'.
    prelude :: LImportDecl GhcPs
    prelude = noLoc $ simpleImportDecl (mkModuleName "Prelude")

    -- Predicate to test for a 'Prelude' import declaration.
    isPrelude :: LImportDecl GhcPs -> Bool
    isPrelude (LL _ x) = fromModuleName' (ideclName x) == "Prelude"
    isPrelude _ = False -- {-# COMPLETE LL #-}

-- Access the imports in scope 'x'.
scopeImports' :: Scope' -> [LImportDecl GhcPs]
scopeImports' (Scope' x) = x

-- Test if two names in two scopes may be referring to the same
-- thing. This is the case if the names are equal and (1) denote a
-- builtin type or data constructor or (2) the intersection of the
-- candidate modules where the two names arise is non-empty.
scopeMatch' :: (Scope', Located RdrName) -> (Scope', Located RdrName) -> Bool
scopeMatch' (a, x) (b, y)
  | isSpecial' x && isSpecial' y = rdrNameStr' x == rdrNameStr' y
  | isSpecial' x || isSpecial' y = False
  | otherwise =
     rdrNameStr' (unqual' x) == rdrNameStr' (unqual' y) && not (possModules' a x `disjoint` possModules' b y)

-- Given a name in a scope, and a new scope, create a name for the new
-- scope that will refer to the same thing. If the resulting name is
-- ambiguous, pick a plausible candidate.
scopeMove' :: (Scope', Located RdrName) -> Scope' -> Located RdrName
scopeMove' (a, x@(fromQual' -> Just name)) (Scope' b) = case imps of
  [] -> headDef x real
  imp:_ | all ideclQualified imps -> noLoc $ mkRdrQual (unLoc . fromMaybe (ideclName imp) $ firstJust ideclAs imps) name
        | otherwise -> unqual' x
  where
    real :: [Located RdrName]
    real = [noLoc $ mkRdrQual (mkModuleName m) name | m <- possModules' a x]

    imps :: [ImportDecl GhcPs]
    imps = [unLoc i | r <- real, i <- b, possImport' i r]
scopeMove' (_, x) _ = x

-- Calculate which modules a name could possibly lie in. If 'x' is
-- qualified but no imported element matches it, assume the user just
-- lacks an import.
possModules' :: Scope' -> Located RdrName -> [String]
possModules' (Scope' is) x = f x
  where
    res :: [String]
    res = [fromModuleName' $ ideclName (unLoc i) | i <- is, possImport' i x]

    f :: Located RdrName -> [String]
    f n | isSpecial' n = [""]
    f (L _ (Qual mod _)) = [moduleNameString mod | null res] ++ res
    f _ = res

-- Determine if 'x' could possibly lie in the module named by the
-- import declaration 'i'.
possImport' :: LImportDecl GhcPs -> Located RdrName -> Bool
possImport' i n | isSpecial' n = False
possImport' (LL _ i) (L _ (Qual mod x)) =
  moduleNameString mod `elem` map fromModuleName' ms && possImport' (noLoc i{ideclQualified=False}) (noLoc $ mkRdrUnqual x)
  where ms = ideclName i : maybeToList (ideclAs i)
possImport' (LL _ i) (L _ (Unqual x)) = not (ideclQualified i) && maybe True f (ideclHiding i)
  where
    f :: (Bool, Located [LIE GhcPs]) -> Bool
    f (hide, L _ xs) =
      if hide then
        Just True `notElem` ms
      else
        Nothing `elem` ms || Just True `elem` ms
      where ms = map g xs

    tag :: String
    tag = occNameString x

    g :: LIE GhcPs -> Maybe Bool -- Does this import cover the name 'x'?
    g (L _ (IEVar _ y)) = Just $ tag == unwrapName y
    g (L _ (IEThingAbs _ y)) = Just $ tag == unwrapName y
    g (L _ (IEThingAll _ y)) = if tag == unwrapName y then Just True else Nothing
    g (L _ (IEThingWith _ y _wildcard ys _fields)) = Just $ tag `elem` unwrapName y : map unwrapName ys
    g _ = Just False

    unwrapName :: LIEWrappedName RdrName -> String
    unwrapName x = occNameString (rdrNameOcc $ ieWrappedName (unLoc x))
possImport' _ _ = False -- {-# COMPLETE LL #-}

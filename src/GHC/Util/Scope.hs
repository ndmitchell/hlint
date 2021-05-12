
{-# Language ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Util.Scope (
   Scope
  ,scopeCreate,scopeMatch,scopeMove,possModules
) where

import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Unit.Module
import GHC.Data.FastString
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence

import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

import Data.List.Extra
import Data.Maybe

-- A scope is a list of import declarations.
newtype Scope = Scope [LImportDecl GhcPs]
               deriving (Monoid, Semigroup)

instance Show Scope where
    show (Scope x) = unsafePrettyPrint x

-- Create a 'Scope from a module's import declarations.
scopeCreate :: HsModule -> Scope
scopeCreate xs = Scope $ [prelude | not $ any isPrelude res] ++ res
  where
    -- Package qualifier of an import declaration.
    pkg :: LImportDecl GhcPs -> Maybe StringLiteral
    pkg (L _ x) = ideclPkgQual x

    -- The import declaraions contained by the module 'xs'.
    res :: [LImportDecl GhcPs]
    res = [x | x <- hsmodImports xs
              , pkg x /= Just (StringLiteral NoSourceText (fsLit "hint") Nothing)
            ]

    -- Mock up an import declaraion corresponding to 'import Prelude'.
    prelude :: LImportDecl GhcPs
    prelude = noLocA $ simpleImportDecl (mkModuleName "Prelude")

    -- Predicate to test for a 'Prelude' import declaration.
    isPrelude :: LImportDecl GhcPs -> Bool
    isPrelude (L _ x) = moduleNameString (unLoc (ideclName x)) == "Prelude"

-- Test if two names in two scopes may be referring to the same
-- thing. This is the case if the names are equal and (1) denote a
-- builtin type or data constructor or (2) the intersection of the
-- candidate modules where the two names arise is non-empty.
scopeMatch :: (Scope, LocatedN RdrName) -> (Scope, LocatedN RdrName) -> Bool
scopeMatch (a, x) (b, y)
  | isSpecial x && isSpecial y = rdrNameStr x == rdrNameStr y
  | isSpecial x || isSpecial y = False
  | otherwise =
     rdrNameStr (unqual x) == rdrNameStr (unqual y) && not (possModules a x `disjointOrd` possModules b y)

-- Given a name in a scope, and a new scope, create a name for the new
-- scope that will refer to the same thing. If the resulting name is
-- ambiguous, pick a plausible candidate.
scopeMove :: (Scope, LocatedN RdrName) -> Scope -> LocatedN RdrName
scopeMove (a, x@(fromQual -> Just name)) (Scope b) = case imps of
  [] -> headDef x real
  imp:_ | all (\x -> ideclQualified x /= NotQualified) imps -> noLocA $ mkRdrQual (unLoc . fromMaybe (ideclName imp) $ firstJust ideclAs imps) name
        | otherwise -> unqual x
  where
    real :: [LocatedN RdrName]
    real = [noLocA $ mkRdrQual m name | m <- possModules a x]

    imps :: [ImportDecl GhcPs]
    imps = [unLoc i | r <- real, i <- b, possImport i r]
scopeMove (_, x) _ = x

-- Calculate which modules a name could possibly lie in. If 'x' is
-- qualified but no imported element matches it, assume the user just
-- lacks an import.
possModules :: Scope -> LocatedN RdrName -> [ModuleName]
possModules (Scope is) x = f x
  where
    res :: [ModuleName]
    res = [unLoc $ ideclName $ unLoc i | i <- is, possImport i x]

    f :: LocatedN RdrName -> [ModuleName]
    f n | isSpecial n = [mkModuleName ""]
    f (L _ (Qual mod _)) = [mod | null res] ++ res
    f _ = res

-- Determine if 'x' could possibly lie in the module named by the
-- import declaration 'i'.
possImport :: LImportDecl GhcPs -> LocatedN RdrName -> Bool
possImport i n | isSpecial n = False
possImport (L _ i) (L _ (Qual mod x)) =
  mod `elem` ms && possImport (noLocA i{ideclQualified=NotQualified}) (noLocA $ mkRdrUnqual x)
  where ms = map unLoc $ ideclName i : maybeToList (ideclAs i)
possImport (L _ i) (L _ (Unqual x)) = ideclQualified i == NotQualified && maybe True f (ideclHiding i)
  where
    f :: (Bool, LocatedL [LIE GhcPs]) -> Bool
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
    g (L _ (IEThingWith _ y _wildcard ys)) = Just $ tag `elem` unwrapName y : map unwrapName ys
    g _ = Just False

    unwrapName :: LIEWrappedName RdrName -> String
    unwrapName x = occNameString (rdrNameOcc $ ieWrappedName (unLoc x))
possImport _ _ = False

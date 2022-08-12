
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
import GHC.Types.PkgQual

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
    pkg (L _ x) =
      case ideclPkgQual x of
        RawPkgQual s -> Just s
        NoRawPkgQual -> Nothing

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
  [] | -- If `possModules a x` includes Prelude, but `b` does not contain any module that may import `x`,
       -- then unqualify `x` and assume that it is from Prelude (#1298).
       any (\(L _ x) -> (moduleNameString . fst <$> isQual_maybe x) == Just "Prelude") real -> unqual x
     | otherwise -> headDef x real
  imp:_ | all (\x -> ideclQualified x /= NotQualified) imps -> noLocA $ mkRdrQual (unLoc . fromMaybe (ideclName imp) $ firstJust ideclAs imps) name
        | otherwise -> unqual x
  where
    real :: [LocatedN RdrName]
    real = [noLocA $ mkRdrQual m name | m <- possModules a x]

    imps :: [ImportDecl GhcPs]
    imps = [unLoc i | r <- real, i <- b, possImport i r /= NotImported]
scopeMove (_, x) _ = x

-- Calculate which modules a name could possibly lie in. If 'x' is
-- qualified but no imported element matches it, assume the user just
-- lacks an import.
-- 'prelude' is added to the result, unless we are certain which module a name is from (#1298).
possModules :: Scope -> LocatedN RdrName -> [ModuleName]
possModules (Scope is) x =
    [prelude | prelude `notElem` map fst res, not (any snd res)] ++ fmap fst res
  where
    -- The 'Bool' signals whether we are certain that 'x' is imported from the module.
    res0, res :: [(ModuleName, Bool)]
    res0 = [ (unLoc $ ideclName $ unLoc i, isImported == Imported)
           | i <- is, let isImported = possImport i x, isImported /= NotImported ]

    res | isSpecial x = [(mkModuleName "", True)]
        | L _ (Qual mod _) <- x = [(mod, True) | null res0] ++ res0
        | otherwise = res0

    prelude = mkModuleName "Prelude"

data IsImported = Imported | PossiblyImported | NotImported  deriving (Eq)

-- Determine if 'x' could possibly lie in the module named by the
-- import declaration 'i'.
possImport :: LImportDecl GhcPs -> LocatedN RdrName -> IsImported
possImport i n | isSpecial n = NotImported
possImport (L _ i) (L _ (Qual mod x)) =
  if mod `elem` ms && NotImported /= possImport (noLocA i{ideclQualified=NotQualified}) (noLocA $ mkRdrUnqual x)
    then Imported
    else NotImported
  where ms = map unLoc $ ideclName i : maybeToList (ideclAs i)
possImport (L _ i) (L _ (Unqual x)) =
  if ideclQualified i == NotQualified
    then maybe PossiblyImported f (ideclHiding i)
    else NotImported
  where
    f :: (Bool, LocatedL [LIE GhcPs]) -> IsImported
    f (hide, L _ xs)
      | hide = if Just True `elem` ms then NotImported else PossiblyImported
      | Just True `elem` ms = Imported
      | Nothing `elem` ms = PossiblyImported
      | otherwise = NotImported
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
possImport _ _ = NotImported

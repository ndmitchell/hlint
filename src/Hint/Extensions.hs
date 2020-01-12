{-# LANGUAGE ScopedTypeVariables #-}

{-
    Suggest removal of unnecessary extensions
    i.e. They have {-# LANGUAGE RecursiveDo #-} but no mdo keywords
<TEST>
{-# LANGUAGE Arrows #-} \
f = id --
{-# LANGUAGE RebindableSyntax #-} \
f = id
{-# LANGUAGE RebindableSyntax, ParallelListComp, ImplicitParams #-} \
f = [(a,c) | a <- b | c <- d] -- {-# LANGUAGE RebindableSyntax, ParallelListComp #-}
{-# LANGUAGE EmptyDataDecls #-} \
data Foo
{-# LANGUAGE TemplateHaskell #-} \
$(deriveNewtypes typeInfo)
{-# LANGUAGE TemplateHaskell #-} \
main = foo ''Bar
{-# LANGUAGE PatternGuards #-} \
test = case x of _ | y <- z -> w
{-# LANGUAGE TemplateHaskell,EmptyDataDecls #-} \
$(fmap return $ dataD (return []) (mkName "Void") [] [] [])
{-# LANGUAGE RecursiveDo #-} \
main = mdo x <- y; return y
{-# LANGUAGE RecursiveDo #-} \
main = do {rec {x <- return 1}; print x}
{-# LANGUAGE ImplicitParams, BangPatterns #-} \
sort :: (?cmp :: a -> a -> Bool) => [a] -> [a] \
sort !f = undefined
{-# LANGUAGE KindSignatures #-} \
data Set (cxt :: * -> *) a = Set [a]
{-# LANGUAGE BangPatterns #-} \
foo x = let !y = x in y
{-# LANGUAGE BangPatterns #-} \
data Foo = Foo !Int --
{-# LANGUAGE RecordWildCards #-} \
record field = Record{..}
{-# LANGUAGE RecordWildCards #-} \
record = 1 -- @Note may require `{-# LANGUAGE DisambiguateRecordFields #-}` adding to the top of the file
{-# LANGUAGE RecordWildCards #-} \
{-# LANGUAGE DisambiguateRecordFields #-} \
record = 1 -- @NoNote
{-# LANGUAGE UnboxedTuples #-} \
record = 1 --
{-# LANGUAGE TemplateHaskell #-} \
foo
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} \
record = 1 --
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} \
newtype Foo = Foo Int deriving Data -- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} \
data Foo = Foo Int deriving Data -- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} \
newtype Foo = Foo Int deriving Class -- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} \
data Foo = Foo Int deriving Class --
{-# LANGUAGE DeriveFunctor #-} \
data Foo = Foo Int deriving Functor
{-# LANGUAGE DeriveFunctor #-} \
newtype Foo = Foo Int deriving Functor
{-# LANGUAGE GeneralizedNewtypeDeriving #-} \
newtype Foo = Foo Int deriving Functor
{-# LANGUAGE GeneralizedNewtypeDeriving #-} \
newtype Foo = Foo Int deriving Data --
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-} \
deriving instance Functor Bar
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-} \
deriving instance Show Bar -- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-} \
newtype Micro = Micro Int deriving Generic -- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} \
instance Class Int where {newtype MyIO a = MyIO a deriving NewClass}
{-# LANGUAGE UnboxedTuples #-} \
f :: Int -> (# Int, Int #)
{-# LANGUAGE UnboxedTuples #-} \
f :: x -> (x, x); f x = (x, x) --
{-# LANGUAGE UnboxedTuples #-} \
f x = case x of (# a, b #) -> a
{-# LANGUAGE DefaultSignatures #-} \
class Val a where; val :: a --
{-# LANGUAGE DefaultSignatures #-} \
class Val a where; val :: a; default val :: Int
{-# LANGUAGE TypeApplications #-} \
foo = id --
{-# LANGUAGE TypeApplications #-} \
foo = id @Int
{-# LANGUAGE LambdaCase #-} \
foo = id --
{-# LANGUAGE LambdaCase #-} \
foo = \case () -> ()
{-# LANGUAGE NumDecimals #-} \
foo = 12.3e2
{-# LANGUAGE NumDecimals #-} \
foo = id --
{-# LANGUAGE NumDecimals #-} \
foo = 12.345e2 --
{-# LANGUAGE TupleSections #-} \
main = map (,1,2) xs
{-# LANGUAGE TupleSections #-} \
main = id --
{-# LANGUAGE OverloadedStrings #-} \
main = "test"
{-# LANGUAGE OverloadedStrings #-} \
main = id --
{-# LANGUAGE DeriveAnyClass #-} \
main = id --
{-# LANGUAGE DeriveAnyClass #-} \
data Foo = Foo deriving Bob
{-# LANGUAGE DeriveAnyClass #-} \
data Foo a = Foo a deriving (Eq,Data,Functor) --
{-# LANGUAGE MagicHash #-} \
foo# = id
{-# LANGUAGE MagicHash #-} \
main = "foo"#
{-# LANGUAGE MagicHash #-} \
main = 5#
{-# LANGUAGE MagicHash #-} \
main = 'a'#
{-# LANGUAGE MagicHash #-} \
main = 5.6#
{-# LANGUAGE MagicHash #-} \
foo = id --
{-# LANGUAGE GeneralizedNewtypeDeriving #-} \
newtype X = X Int deriving newtype Show
{-# LANGUAGE EmptyCase #-} \
main = case () of {}
{-# LANGUAGE EmptyCase #-} \
main = case () of x -> x --
{-# LANGUAGE EmptyCase #-} \
main = case () of x -> x --
{-# LANGUAGE PolyKinds, KindSignatures #-} -- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PolyKinds, KindSignatures #-} \
data Set (cxt :: * -> *) a = Set [a] -- @Note Extension KindSignatures is implied by PolyKinds
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-} \
main = putStrLn [f|{T.intercalate "blah" []}|]
</TEST>
-}


module Hint.Extensions(extensionsHint) where

import Hint.Type(ModuHint, rawIdea',Severity(Warning),Note(..),toSS',ghcAnnotations,ghcModule,extensionImpliedBy,extensionImplies)
import Language.Haskell.Exts.Extension

import Data.Generics.Uniplate.Operations
import Control.Monad.Extra
import Data.List.Extra
import Data.Ratio
import Data.Data
import Refact.Types
import qualified Data.Set as Set
import qualified Data.Map as Map

import SrcLoc
import HsSyn
import BasicTypes
import Class
import RdrName
import OccName
import ForeignCall
import GHC.Util

extensionsHint :: ModuHint
extensionsHint _ x =
    [ rawIdea' Hint.Type.Warning "Unused LANGUAGE pragma"
        sl
        (comment (mkLangExts sl exts))
        (Just newPragma)
        ( [RequiresExtension $ prettyExtension gone | x <- before \\ after, gone <- Map.findWithDefault [] x disappear] ++
            [ Note $ "Extension " ++ prettyExtension x ++ " is " ++ reason x
            | x <- explainedRemovals])
        [ModifyComment (toSS' (mkLangExts sl exts)) newPragma]
    | (LL sl _,  exts) <- langExts $ pragmas (ghcAnnotations x)
    , let before = map parseExtension exts
    , let after = filter (`Set.member` keep) before
    , before /= after
    , let explainedRemovals
            | null after && not (any (`Map.member` implied) before) = []
            | otherwise = before \\ after
    , let newPragma =
            if null after then "" else comment (mkLangExts sl $ map prettyExtension after)
    ]
  where
    usedTH :: Bool
    usedTH = used TemplateHaskell (ghcModule x) || used QuasiQuotes (ghcModule x)
      -- If TH or QuasiQuotes is on, can use all other extensions
      -- programmatically.

    -- All the extensions defined to be used.
    extensions :: Set.Set Extension
    extensions = Set.fromList [ parseExtension e
                              | let exts = concatMap snd $ langExts (pragmas (ghcAnnotations x))
                              , e <- exts ]
    -- Those extensions we detect to be useful.
    useful :: Set.Set Extension
    useful = if usedTH then extensions else Set.filter (`usedExt` ghcModule x) extensions
    -- Those extensions which are useful, but implied by other useful
    -- extensions.
    implied :: Map.Map Extension Extension
    implied = Map.fromList
        [ (e, a)
        | e <- Set.toList useful
        , a:_ <- [filter (`Set.member` useful) $ extensionImpliedBy e]]
    -- Those we should keep.
    keep :: Set.Set Extension
    keep =  useful `Set.difference` Map.keysSet implied
    -- The meaning of (a,b) is a used to imply b, but has gone, so
    -- suggest enabling b.
    disappear =
        Map.fromListWith (++) $
        nubOrdOn snd -- Only keep one instance for each of a.
        [ (e, [a])
        | e <- Set.toList $ extensions `Set.difference` keep
        , a <- extensionImplies e
        , a `Set.notMember` useful
        , usedTH || usedExt a (ghcModule x)
        ]
    reason :: Extension -> String
    reason x =
      case Map.lookup x implied of
        Just a -> "implied by " ++ prettyExtension a
        Nothing -> "not used"

deriveHaskell = ["Eq","Ord","Enum","Ix","Bounded","Read","Show"]
deriveGenerics = ["Data","Typeable","Generic","Generic1","Lift"]
deriveCategory = ["Functor","Foldable","Traversable"]

-- | Classes that can't require newtype deriving
noDeriveNewtype =
    delete "Enum" deriveHaskell ++ -- Enum can't always be derived on a newtype
    deriveGenerics -- Generics stuff can't newtype derive since it has the ctor in it

-- | Classes that can appear as stock, and can't appear as anyclass
deriveStock :: [String]
deriveStock = deriveHaskell ++ deriveGenerics ++ deriveCategory

usedExt :: Extension -> Located (HsModule GhcPs) -> Bool
usedExt (EnableExtension x) = used x
usedExt (UnknownExtension "NumDecimals") = hasS isWholeFrac
usedExt (UnknownExtension "DeriveLift") = hasDerive ["Lift"]
usedExt (UnknownExtension "DeriveAnyClass") = not . null . derivesAnyclass . derives
usedExt _ = const True

used :: KnownExtension -> Located (HsModule GhcPs) -> Bool
used RecursiveDo = hasS isMDo' ||^ hasS isRecStmt'
used ParallelListComp = hasS isParComp'
used FunctionalDependencies = hasT (un :: FunDep (Located RdrName))
used ImplicitParams = hasT (un :: HsIPName)
used TypeApplications = hasS isTypeApp'
used EmptyDataDecls = hasS f
  where
    f :: HsDataDefn GhcPs -> Bool
    f (HsDataDefn _ _ _ _ _ [] _) = True
    f _ = False
used EmptyCase = hasS f
  where
    f :: HsExpr GhcPs -> Bool
    f (HsCase _ _ (MG _ (LL _ []) _)) = True
    f (HsLamCase _ (MG _ (LL _ []) _)) = True
    f _ = False
used KindSignatures = hasT (un :: HsKind GhcPs)
used BangPatterns = hasS isPBangPat' ||^ hasS isStrictMatch
  where
    isStrictMatch :: HsMatchContext RdrName -> Bool
    isStrictMatch FunRhs{mc_strictness=SrcStrict} = True
    isStrictMatch _ = False
used TemplateHaskell = hasT2' (un :: (HsBracket GhcPs, HsSplice GhcPs)) ||^ hasS f ||^ hasS isSpliceDecl'
    where
      f :: HsBracket GhcPs -> Bool
      f VarBr{} = True
      f TypBr{} = True
      f _ = False
used ForeignFunctionInterface = hasT (un :: CCallConv)
used PatternGuards = hasS f
  where
    f :: GRHS GhcPs (LHsExpr GhcPs) -> Bool
    f (GRHS _ xs _) = g xs
    f _ = False -- new ctor
    g :: [GuardLStmt GhcPs] -> Bool
    g [] = False
    g [LL _ BodyStmt{}] = False
    g _ = True
used StandaloneDeriving = hasS isDerivD'
used PatternSignatures = hasS isPatTypeSig'
used RecordWildCards = hasS hasFieldsDotDot' ||^ hasS hasPFieldsDotDot'
used RecordPuns = hasS isPFieldPun' ||^ hasS isFieldPun'
used NamedFieldPuns = hasS isPFieldPun' ||^ hasS isFieldPun'
used UnboxedTuples = has isUnboxedTuple' ||^ has (== Unboxed)
used PackageImports = hasS f
    where
        f :: ImportDecl GhcPs -> Bool
        f ImportDecl{ideclPkgQual=Just _} = True
        f _ = False
used QuasiQuotes = hasS isQuasiQuote' ||^ hasS isTyQuasiQuote'
used ViewPatterns = hasS isPViewPat'
used DefaultSignatures = hasS isClsDefSig'
used DeriveDataTypeable = hasDerive ["Data","Typeable"]
used DeriveFunctor = hasDerive ["Functor"]
used DeriveFoldable = hasDerive ["Foldable"]
used DeriveTraversable = hasDerive ["Traversable","Foldable","Functor"]
used DeriveGeneric = hasDerive ["Generic","Generic1"]
used GeneralizedNewtypeDeriving = not . null . derivesNewtype' . derives
used LambdaCase = hasS isLCase'
used TupleSections = hasS isTupleSection'
used OverloadedStrings = hasS isString'
used Arrows = hasS f
  where
    f :: HsExpr GhcPs -> Bool
    f HsProc{} = True
    f HsArrApp{} = True
    f _ = False
used TransformListComp = hasS f
    where
      f :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Bool
      f TransStmt{} = True
      f _ = False
used MagicHash = hasS f ||^ hasS isPrimLiteral'
    where
      f :: RdrName -> Bool
      f s = "#" `isSuffixOf` (occNameString . rdrNameOcc) s
-- For forwards compatibility, if things ever get added to the
-- extension enumeration.
used x = usedExt $ UnknownExtension $ show x

hasDerive :: [String] -> Located (HsModule GhcPs) -> Bool
hasDerive want = any (`elem` want) . derivesStock' . derives

-- Derivations can be implemented using any one of 3 strategies, so for each derivation
-- add it to all the strategies that might plausibly implement it
data Derives = Derives
    {derivesStock' :: [String]
    ,derivesAnyclass :: [String]
    ,derivesNewtype' :: [String]
    }
instance Semigroup Derives where
    Derives x1 x2 x3 <> Derives y1 y2 y3 =
        Derives (x1 ++ y1) (x2 ++ y2) (x3 ++ y3)
instance Monoid Derives where
    mempty = Derives [] [] []
    mappend = (<>)

addDerives :: Maybe NewOrData -> Maybe (DerivStrategy GhcPs) -> [String] -> Derives
addDerives _ (Just s) xs = case s of
    StockStrategy -> mempty{derivesStock' = xs}
    AnyclassStrategy -> mempty{derivesAnyclass = xs}
    NewtypeStrategy -> mempty{derivesNewtype' = xs}
    ViaStrategy{} -> mempty
addDerives nt _ xs = mempty
    {derivesStock' = stock
    ,derivesAnyclass = other
    ,derivesNewtype' = if maybe True isNewType' nt then filter (`notElem` noDeriveNewtype) xs else []}
    where (stock, other) = partition (`elem` deriveStock) xs

derives :: Located (HsModule GhcPs) -> Derives
derives (LL _ m) =  mconcat $ map decl (childrenBi m) ++ map idecl (childrenBi m)
  where
    idecl :: Located (DataFamInstDecl GhcPs) -> Derives
    idecl (LL _ (DataFamInstDecl (HsIB _ FamEqn {feqn_rhs=HsDataDefn {dd_ND=dn, dd_derivs=(LL _ ds)}}))) = g dn ds
    idecl _ = mempty

    decl :: LHsDecl GhcPs -> Derives
    decl (LL _ (TyClD _ (DataDecl _ _ _ _ HsDataDefn {dd_ND=dn, dd_derivs=(LL _ ds)}))) = g dn ds -- Data declaration.
    decl (LL _ (DerivD _ (DerivDecl _ (HsWC _ sig) strategy _))) = addDerives Nothing (fmap unLoc strategy) [derivedToStr sig] -- A deriving declaration.
    decl _ = mempty

    g :: NewOrData -> [LHsDerivingClause GhcPs] -> Derives
    g dn ds = mconcat [addDerives (Just dn) (fmap unLoc strategy) $ map derivedToStr tys | LL _ (HsDerivingClause _ strategy (LL _ tys)) <- ds]

    derivedToStr :: LHsSigType GhcPs -> String
    derivedToStr (HsIB _ t) = ih t
      where
        ih :: LHsType GhcPs -> String
        ih (LL _ (HsQualTy _ _ a)) = ih a
        ih (LL _ (HsParTy _ a)) = ih a
        ih (LL _ (HsAppTy _ a _)) = ih a
        ih (LL _ (HsTyVar _ _ a)) = unsafePrettyPrint $ unqual' a
        ih (LL _ a) = unsafePrettyPrint a -- I don't anticipate this case is called.
        ih _ = "" -- {-# COMPLETE LL #-}
    derivedToStr _ = "" -- new ctor

derives _ = mempty -- {-# COMPLETE LL #-}

un = undefined

hasT t x = not $ null (universeBi x `asTypeOf` [t])
hasT2' ~(t1,t2) = hasT t1 ||^ hasT t2

hasS :: (Data x, Data a) => (a -> Bool) -> x -> Bool
hasS test = any test . universeBi

has f = any f . universeBi

-- Only whole number fractions are permitted by NumDecimals extension.
-- Anything not-whole raises an error.
isWholeFrac :: HsExpr GhcPs -> Bool
isWholeFrac (HsLit _ (HsRat _ (FL _ _ v) _)) = denominator v == 1
isWholeFrac (HsOverLit _ (OverLit _ (HsFractional (FL _ _ v)) _)) = denominator v == 1
isWholeFrac _ = False

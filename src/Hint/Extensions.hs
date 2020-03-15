{-# LANGUAGE LambdaCase #-}

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
{-# LANGUAGE GeneralizedNewtypeDeriving,UnboxedTuples #-} \
newtype T m a = T (m a) deriving (PrimMonad)
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
{-# LANGUAGE NamedFieldPuns #-} \
foo = x{bar}
{-# LANGUAGE PatternSynonyms #-} \
module Foo (pattern Bar) where x = 42
{-# LANGUAGE PatternSynonyms #-} \
import Foo (pattern Bar); x = 42
{-# LANGUAGE PatternSynonyms #-} \
pattern Foo s <- Bar s _ where Foo s = Bar s s
{-# LANGUAGE PatternSynonyms #-} \
x = 42 --
{-# LANGUAGE MultiWayIf #-} \
x = if | b1 -> v1 | b2 -> v2 | otherwise -> v3
{-# LANGUAGE MultiWayIf #-} \
x = if b1 then v1 else if b2 then v2 else v3 --
</TEST>
-}


module Hint.Extensions(extensionsHint) where

import Hint.Type(ModuHint, rawIdea',Severity(Warning),Note(..),toSS',ghcAnnotations,ghcModule)
import Extension

import Data.Generics.Uniplate.Operations
import Control.Monad.Extra
import Data.List.Extra
import Data.Ratio
import Data.Data
import Data.Maybe
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

import Language.Haskell.GhclibParserEx.GHC.Hs.Pat
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Hs.Types
import Language.Haskell.GhclibParserEx.GHC.Hs.Decls
import qualified Language.Haskell.GhclibParserEx.DynFlags as GhclibParserEx
import GHC.LanguageExtensions.Type

-- Terrible hack.
readExtension' e =
  case e of
   "CPP" -> Just Cpp
   "NamedFieldPuns" -> Just RecordPuns
   "Rank2Types" -> Just RankNTypes
   'N' : 'o' : _ -> Nothing
   _ ->  GhclibParserEx.readExtension e

extensionsHint :: ModuHint
extensionsHint _ x =
    [ rawIdea' Hint.Type.Warning "Unused LANGUAGE pragma"
        sl
        (comment (mkLangExts sl exts))
        (Just newPragma)
        ( [RequiresExtension (show gone) | x <- before \\ after, gone <- Map.findWithDefault [] x disappear] ++
            [ Note $ "Extension " ++ show x ++ " is " ++ reason x
            | x <- explainedRemovals])
        [ModifyComment (toSS' (mkLangExts sl exts)) newPragma]
    | (L sl _,  exts) <- langExts $ pragmas (ghcAnnotations x)
    , let before = mapMaybe readExtension' exts
    , let after = filter (`Set.member` keep) before
    , before /= after
    , let explainedRemovals
            | null after && not (any (`Map.member` implied) before) = []
            | otherwise = before \\ after
    , let newPragma =
            if null after then "" else comment (mkLangExts sl $ map show after)
    ]
  where
    usedTH :: Bool
    usedTH = used TemplateHaskell (ghcModule x) || used QuasiQuotes (ghcModule x)
      -- If TH or QuasiQuotes is on, can use all other extensions
      -- programmatically.

    -- All the extensions defined to be used.
    extensions :: Set.Set Extension
    extensions = Set.fromList $ catMaybes [ readExtension' e
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
        , a:_ <- [filter (`Set.member` useful) $ extensionImpliedEnabledBy e]
        ]
    -- Those we should keep.
    keep :: Set.Set Extension
    keep =  useful `Set.difference` Map.keysSet implied
    -- The meaning of (a,b) is a used to imply b, but has gone, so
    -- suggest enabling b.
    disappear :: Map.Map Extension [Extension]
    disappear =
        Map.fromListWith (++) $
        nubOrdOn snd -- Only keep one instance for each of a.
        [ (e, [a])
        | e <- Set.toList $ extensions `Set.difference` keep
        , a <- fst $ extensionImplies e
        , a `Set.notMember` useful
        , usedTH || usedExt a (ghcModule x)
        ]
    reason :: Extension -> String
    reason x =
      case Map.lookup x implied of
        Just a -> "implied by " ++ show a
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
usedExt NumDecimals = hasS isWholeFrac
usedExt DeriveLift = hasDerive ["Lift"]
usedExt DeriveAnyClass = not . null . derivesAnyclass . derives
usedExt x = used x

used :: Extension -> Located (HsModule GhcPs) -> Bool
used RecursiveDo = hasS isMDo ||^ hasS isRecStmt
used ParallelListComp = hasS isParComp
used FunctionalDependencies = hasT (un :: FunDep (Located RdrName))
used ImplicitParams = hasT (un :: HsIPName)
used TypeApplications = hasS isTypeApp
used EmptyDataDecls = hasS f
  where
    f :: HsDataDefn GhcPs -> Bool
    f (HsDataDefn _ _ _ _ _ [] _) = True
    f _ = False
used EmptyCase = hasS f
  where
    f :: HsExpr GhcPs -> Bool
    f (HsCase _ _ (MG _ (L _ []) _)) = True
    f (HsLamCase _ (MG _ (L _ []) _)) = True
    f _ = False
used KindSignatures = hasT (un :: HsKind GhcPs)
used BangPatterns = hasS isPBangPat ||^ hasS isStrictMatch
  where
    isStrictMatch :: HsMatchContext RdrName -> Bool
    isStrictMatch FunRhs{mc_strictness=SrcStrict} = True
    isStrictMatch _ = False
used TemplateHaskell = hasT2' (un :: (HsBracket GhcPs, HsSplice GhcPs)) ||^ hasS f ||^ hasS isSpliceDecl
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
    f _ = False -- Extension constructor
    g :: [GuardLStmt GhcPs] -> Bool
    g [] = False
    g [L _ BodyStmt{}] = False
    g _ = True
used StandaloneDeriving = hasS isDerivD
used RecordWildCards = hasS hasFieldsDotDot ||^ hasS hasPFieldsDotDot
used RecordPuns = hasS isPFieldPun ||^ hasS isFieldPun ||^ hasS isFieldPunUpdate
used UnboxedTuples = hasS isUnboxedTuple ||^ hasS (== Unboxed) ||^ hasS isDeriving
    where
        -- detect if there are deriving declarations or data ... deriving stuff
        -- by looking for the deriving strategy both contain (even if its Nothing)
        -- see https://github.com/ndmitchell/hlint/issues/833 for why we care
        isDeriving :: Maybe (LDerivStrategy GhcPs) -> Bool
        isDeriving _ = True
used PackageImports = hasS f
    where
        f :: ImportDecl GhcPs -> Bool
        f ImportDecl{ideclPkgQual=Just _} = True
        f _ = False
used QuasiQuotes = hasS isQuasiQuote ||^ hasS isTyQuasiQuote
used ViewPatterns = hasS isPViewPat
used DefaultSignatures = hasS isClsDefSig
used DeriveDataTypeable = hasDerive ["Data","Typeable"]
used DeriveFunctor = hasDerive ["Functor"]
used DeriveFoldable = hasDerive ["Foldable"]
used DeriveTraversable = hasDerive ["Traversable","Foldable","Functor"]
used DeriveGeneric = hasDerive ["Generic","Generic1"]
used GeneralizedNewtypeDeriving = not . null . derivesNewtype' . derives
used MultiWayIf = hasS f
  where
    f :: HsExpr GhcPs -> Bool
    f = \case HsMultiIf{} -> True; _ -> False
used LambdaCase = hasS isLCase
used TupleSections = hasS isTupleSection
used OverloadedStrings = hasS isString
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
used MagicHash = hasS f ||^ hasS isPrimLiteral
    where
      f :: RdrName -> Bool
      f s = "#" `isSuffixOf` (occNameString . rdrNameOcc) s
used PatternSynonyms = hasS isPatSynBind ||^ hasS isPatSynIE
    where
      isPatSynBind :: HsBind GhcPs -> Bool
      isPatSynBind PatSynBind{} = True
      isPatSynBind _ = False

      isPatSynIE :: IEWrappedName RdrName -> Bool
      isPatSynIE IEPattern{} = True
      isPatSynIE _ = False
used _= const True

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
    ,derivesNewtype' = if maybe True isNewType nt then filter (`notElem` noDeriveNewtype) xs else []}
    where (stock, other) = partition (`elem` deriveStock) xs

derives :: Located (HsModule GhcPs) -> Derives
derives (L _ m) =  mconcat $ map decl (childrenBi m) ++ map idecl (childrenBi m)
  where
    idecl :: Located (DataFamInstDecl GhcPs) -> Derives
    idecl (L _ (DataFamInstDecl (HsIB _ FamEqn {feqn_rhs=HsDataDefn {dd_ND=dn, dd_derivs=(L _ ds)}}))) = g dn ds
    idecl _ = mempty

    decl :: LHsDecl GhcPs -> Derives
    decl (L _ (TyClD _ (DataDecl _ _ _ _ HsDataDefn {dd_ND=dn, dd_derivs=(L _ ds)}))) = g dn ds -- Data declaration.
    decl (L _ (DerivD _ (DerivDecl _ (HsWC _ sig) strategy _))) = addDerives Nothing (fmap unLoc strategy) [derivedToStr sig] -- A deriving declaration.
    decl _ = mempty

    g :: NewOrData -> [LHsDerivingClause GhcPs] -> Derives
    g dn ds = mconcat [addDerives (Just dn) (fmap unLoc strategy) $ map derivedToStr tys | L _ (HsDerivingClause _ strategy (L _ tys)) <- ds]

    derivedToStr :: LHsSigType GhcPs -> String
    derivedToStr (HsIB _ t) = ih t
      where
        ih :: LHsType GhcPs -> String
        ih (L _ (HsQualTy _ _ a)) = ih a
        ih (L _ (HsParTy _ a)) = ih a
        ih (L _ (HsAppTy _ a _)) = ih a
        ih (L _ (HsTyVar _ _ a)) = unsafePrettyPrint $ unqual' a
        ih (L _ a) = unsafePrettyPrint a -- I don't anticipate this case is called.
    derivedToStr _ = "" -- new ctor

un = undefined

hasT t x = not $ null (universeBi x `asTypeOf` [t])
hasT2' ~(t1,t2) = hasT t1 ||^ hasT t2

hasS :: (Data x, Data a) => (a -> Bool) -> x -> Bool
hasS test = any test . universeBi

-- Only whole number fractions are permitted by NumDecimals extension.
-- Anything not-whole raises an error.
isWholeFrac :: HsExpr GhcPs -> Bool
isWholeFrac (HsLit _ (HsRat _ (FL _ _ v) _)) = denominator v == 1
isWholeFrac (HsOverLit _ (OverLit _ (HsFractional (FL _ _ v)) _)) = denominator v == 1
isWholeFrac _ = False

-- Field puns in updates have a different type to field puns in constructions
isFieldPunUpdate :: HsRecField' (AmbiguousFieldOcc GhcPs) (LHsExpr GhcPs) -> Bool
isFieldPunUpdate = \case HsRecField {hsRecPun=True} -> True; _ -> False

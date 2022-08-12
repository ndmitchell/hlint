{-# LANGUAGE LambdaCase, NamedFieldPuns, ScopedTypeVariables #-}

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
main = foo ''Bar --
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-} \
f x = x + [e| x + 1 |] + [foo| x + 1 |] -- {-# LANGUAGE QuasiQuotes #-}
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
{-# LANGUAGE TypeOperators #-} \
data (<+>) a b = Foo a b
{-# LANGUAGE TypeOperators #-} \
data Foo a b = a :+ b --
{-# LANGUAGE TypeOperators #-} \
type (<+>) a b = Foo a b
{-# LANGUAGE TypeOperators #-} \
type Foo a b = a :+ b
{-# LANGUAGE TypeOperators, TypeFamilies #-} \
type family Foo a b :: Type where Foo a b = a :+ b
{-# LANGUAGE TypeOperators, TypeFamilies #-} \
type family Foo a b :: Type where Foo a b = (<+>) a b -- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-} \
class Foo a where data (<+>) a
{-# LANGUAGE TypeOperators, TypeFamilies #-} \
class Foo a where foo :: a -> Int <+> Bool
{-# LANGUAGE TypeOperators #-} \
class (<+>) a where
{-# LANGUAGE TypeOperators #-} \
foo :: Int -> Double <+> Bool \
foo x = y
{-# LANGUAGE TypeOperators #-} \
foo :: Int -> (<+>) Double Bool \
foo x = y --
{-# LANGUAGE TypeOperators #-} \
(<+>) :: Int -> Int -> Int \
x <+> y = x + y --
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
{-# LANGUAGE DeriveGeneric, TypeFamilies #-} \
data family Bar a; data instance Bar Foo = Foo deriving (Generic)
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
{-# LANGUAGE InstanceSigs #-} \
instance Eq a => Eq (T a) where \
  (==) :: T a -> T a -> Bool \
  (==) (T x) (T y) = x==y
{-# LANGUAGE InstanceSigs #-} \
instance Eq a => Eq (T a) where \
  (==) (T x) (T y) = x==y --
{-# LANGUAGE DefaultSignatures #-} \
class Val a where; val :: a --
{-# LANGUAGE DefaultSignatures #-} \
class Val a where; val :: a; default val :: Int
{-# LANGUAGE TypeApplications #-} \
foo = id --
{-# LANGUAGE TypeApplications #-} \
foo = id @Int
{-# LANGUAGE TypeApplications #-} \
x :: Typeable b => TypeRep @Bool b
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
{-# LANGUAGE OverloadedLists #-} \
main = []
{-# LANGUAGE OverloadedLists #-} \
main = [1]
{-# LANGUAGE OverloadedLists #-} \
main [1] = True
{-# LANGUAGE OverloadedLists #-} \
main = id --
{-# LANGUAGE OverloadedLabels #-} \
main = #foo
{-# LANGUAGE OverloadedLabels #-} \
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
static = 42
{-# LANGUAGE NamedFieldPuns #-} \
foo Foo{x} = x
{-# LANGUAGE NamedFieldPuns #-} \
foo = Foo{x}
{-# LANGUAGE NamedFieldPuns #-} \
foo = bar{x}
{-# LANGUAGE NamedFieldPuns #-} --
{-# LANGUAGE NumericUnderscores #-} \
lessThanPi = (< 3.141_592_653_589_793)
{-# LANGUAGE NumericUnderscores #-} \
oneMillion = 0xf4__240
{-# LANGUAGE NumericUnderscores #-} \
avogadro = 6.022140857e+23 --
{-# LANGUAGE StaticPointers #-} \
static = 42 --
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Trustworthy, NamedFieldPuns #-} -- {-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE NoStarIsType, ExplicitNamespaces #-} \
import GHC.TypeLits(KnownNat, type (+), type (*))
{-# LANGUAGE LambdaCase, MultiWayIf, NoRebindableSyntax #-} \
foo = \case True -> 3 -- {-# LANGUAGE LambdaCase, NoRebindableSyntax #-}
{-# LANGUAGE ImportQualifiedPost #-} \
import Control.Monad qualified as CM
{-# LANGUAGE ImportQualifiedPost #-} \
import qualified Control.Monad as CM hiding (mapM) \
import Data.Foldable -- @NoRefactor: refactor only works when using GHC 8.10
{-# LANGUAGE StandaloneKindSignatures #-} \
type T :: (k -> Type) -> k -> Type \
data T m a = MkT (m a) (T Maybe (m a))
{-# LANGUAGE NoMonomorphismRestriction, NamedFieldPuns #-} \
main = 1 -- @Note Extension NamedFieldPuns is not used
{-# LANGUAGE FunctionalDependencies #-} \
class HasField x r a | x r -> a
{-# LANGUAGE OverloadedRecordDot #-} \
f x = x.foo
{-# LANGUAGE OverloadedRecordDot #-} \
f x = x . foo -- @NoRefactor: refactor requires GHC >= 9.2.1
{-# LANGUAGE OverloadedRecordDot #-} \
f = (.foo)
{-# LANGUAGE OverloadedRecordDot #-} \
f = (. foo) -- @NoRefactor: refactor requires GHC >= 9.2.1
</TEST>
-}


module Hint.Extensions(extensionsHint) where

import Hint.Type(ModuHint,rawIdea,Severity(Warning),Note(..),toSSAnc,ghcModule,modComments)
import Extension

import Data.Generics.Uniplate.DataOnly
import Control.Monad.Extra
import Data.Maybe
import Data.List.Extra
import Data.Data
import Refact.Types
import qualified Data.Set as Set
import qualified Data.Map as Map

import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Hs
import GHC.Types.Basic
import GHC.Types.Name.Reader
import GHC.Types.ForeignCall
import qualified GHC.Data.Strict
import GHC.Types.PkgQual

import GHC.Util
import GHC.LanguageExtensions.Type

import Language.Haskell.GhclibParserEx.GHC.Hs.Pat
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Hs.Type
import Language.Haskell.GhclibParserEx.GHC.Hs.Decls
import Language.Haskell.GhclibParserEx.GHC.Hs.Binds
import Language.Haskell.GhclibParserEx.GHC.Hs.ImpExp
import Language.Haskell.GhclibParserEx.GHC.Driver.Session
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader

extensionsHint :: ModuHint
extensionsHint _ x =
    [
        rawIdea Hint.Type.Warning "Unused LANGUAGE pragma"
        (RealSrcSpan (anchor sl) GHC.Data.Strict.Nothing)
        (comment_ (mkLanguagePragmas sl exts))
        (Just newPragma)
        ( [RequiresExtension (show gone) | (_, Just x) <- before \\ after, gone <- Map.findWithDefault [] x disappear] ++
            [ Note $ "Extension " ++ s ++ " is " ++ reason x
            | (s, Just x) <- explainedRemovals])
        [ModifyComment (toSSAnc (mkLanguagePragmas sl exts)) newPragma]
    | (L sl _,  exts) <- languagePragmas $ pragmas (modComments x)
    , let before = [(x, readExtension x) | x <- exts]
    , let after = filter (maybe True (`Set.member` keep) . snd) before
    , before /= after
    , let explainedRemovals
            | null after && not (any (`Map.member` implied) $ mapMaybe snd before) = []
            | otherwise = before \\ after
    , let newPragma =
            if null after then "" else comment_ (mkLanguagePragmas sl $ map fst after)
    ]
  where
    usedTH :: Bool
    usedTH = used TemplateHaskell (ghcModule x)
               || used TemplateHaskellQuotes (ghcModule x)
               || used QuasiQuotes (ghcModule x)
      -- If TH or QuasiQuotes is on, can use all other extensions
      -- programmatically.

    -- All the extensions defined to be used.
    extensions :: Set.Set Extension
    extensions = Set.fromList $ mapMaybe readExtension $
        concatMap snd $ languagePragmas (pragmas (modComments x))

    -- Those extensions we detect to be useful.
    useful :: Set.Set Extension
    useful =
      if usedTH
        then Set.filter (\case TemplateHaskell -> usedExt TemplateHaskell (ghcModule x); _ -> True) extensions
        else Set.filter (`usedExt` ghcModule x) extensions
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

usedExt :: Extension -> Located HsModule -> Bool
usedExt NumDecimals = hasS isWholeFrac
  -- Only whole number fractions are permitted by NumDecimals
  -- extension.  Anything not-whole raises an error.
usedExt DeriveLift = hasDerive ["Lift"]
usedExt DeriveAnyClass = not . null . derivesAnyclass . derives
usedExt x = used x

used :: Extension -> Located HsModule -> Bool

used RecursiveDo = hasS isMDo ||^ hasS isRecStmt
used ParallelListComp = hasS isParComp
used FunctionalDependencies = hasT (un :: FunDep GhcPs)
used ImplicitParams = hasT (un :: HsIPName)
used TypeApplications = hasS isTypeApp ||^ hasS isKindTyApp
used EmptyDataDecls = hasS f
  where
    f :: HsDataDefn GhcPs -> Bool
    f (HsDataDefn _ _ _ _ _ [] _) = True
    f _ = False
used EmptyCase = hasS f
  where
    f :: HsExpr GhcPs -> Bool
    f (HsCase _ _ (MG _ (L _ []) _)) = True
    f (HsLamCase _ _ (MG _ (L _ []) _)) = True
    f _ = False
used KindSignatures = hasT (un :: HsKind GhcPs)
used BangPatterns = hasS isPBangPat ||^ hasS isStrictMatch
used TemplateHaskell = hasS $ not . isQuasiQuoteSplice
used TemplateHaskellQuotes = hasT (un :: HsQuote GhcPs)
used ForeignFunctionInterface = hasT (un :: CCallConv)
used PatternGuards = hasS f
  where
    f :: GRHS GhcPs (LHsExpr GhcPs) -> Bool
    f (GRHS _ xs _) = g xs
    g :: [GuardLStmt GhcPs] -> Bool
    g [] = False
    g [L _ BodyStmt{}] = False
    g _ = True
used StandaloneDeriving = hasS isDerivD
used TypeOperators = hasS tyOpInSig ||^ hasS tyOpInDecl
  where
    tyOpInSig :: HsType GhcPs -> Bool
    tyOpInSig = \case
      HsOpTy{} -> True; _ -> False

    tyOpInDecl :: HsDecl GhcPs -> Bool
    tyOpInDecl = \case
      (TyClD _ (FamDecl _ FamilyDecl{fdLName})) -> isOp fdLName
      (TyClD _ SynDecl{tcdLName}) -> isOp tcdLName
      (TyClD _ DataDecl{tcdLName}) -> isOp tcdLName
      (TyClD _ ClassDecl{tcdLName, tcdATs}) -> any isOp (tcdLName : [fdLName famDecl | L _ famDecl <- tcdATs])
      _ -> False

    isOp (L _ name) = isSymbolRdrName name

used RecordWildCards = hasS hasFieldsDotDot ||^ hasS hasPFieldsDotDot
used NamedFieldPuns = hasS isPFieldPun ||^ hasS isFieldPun ||^ hasS isFieldPunUpdate
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
      f ImportDecl{ideclPkgQual=RawPkgQual _} = True
      f _ = False
used QuasiQuotes = hasS isQuasiQuoteExpr ||^ hasS isTyQuasiQuote
used ViewPatterns = hasS isPViewPat
used InstanceSigs = hasS f
  where
    f :: HsDecl GhcPs -> Bool
    f (InstD _ decl) = hasT (un :: Sig GhcPs) decl
    f _ = False
used DefaultSignatures = hasS isClsDefSig
used DeriveDataTypeable = hasDerive ["Data","Typeable"]
used DeriveFunctor = hasDerive ["Functor"]
used DeriveFoldable = hasDerive ["Foldable"]
used DeriveTraversable = hasDerive ["Traversable","Foldable","Functor"]
used DeriveGeneric = hasDerive ["Generic","Generic1"]
used GeneralizedNewtypeDeriving = not . null . derivesNewtype' . derives
used MultiWayIf = hasS isMultiIf
used NumericUnderscores = hasS f
  where
    f :: OverLitVal -> Bool
    f (HsIntegral (IL (SourceText t) _ _)) = '_' `elem` t
    f (HsFractional (FL (SourceText t) _ _ _ _)) = '_' `elem` t
    f _ = False

used LambdaCase = hasS isLCase
used TupleSections = hasS isTupleSection
used OverloadedStrings = hasS isString
used OverloadedLists = hasS isListExpr ||^ hasS isListPat
  where
    isListExpr :: HsExpr GhcPs -> Bool
    isListExpr (HsVar _ n) = rdrNameStr n == "[]"
    isListExpr ExplicitList{} = True
    isListExpr ArithSeq{} = True
    isListExpr _ = False

    isListPat :: Pat GhcPs -> Bool
    isListPat ListPat{} = True
    isListPat _ = False

used OverloadedLabels = hasS isOverLabel

used Arrows = hasS isProc
used TransformListComp = hasS isTransStmt
used MagicHash = hasS f ||^ hasS isPrimLiteral
  where
    f :: RdrName -> Bool
    f s = "#" `isSuffixOf` occNameStr s
used PatternSynonyms = hasS isPatSynBind ||^ hasS isPatSynIE
used ImportQualifiedPost = hasS (== QualifiedPost)
used StandaloneKindSignatures = hasT (un :: StandaloneKindSig GhcPs)
used OverloadedRecordDot = hasT (un :: DotFieldOcc GhcPs)

used _= const True

hasDerive :: [String] -> Located HsModule -> Bool
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
    StockStrategy {} -> mempty{derivesStock' = xs}
    AnyclassStrategy {} -> mempty{derivesAnyclass = xs}
    NewtypeStrategy {} -> mempty{derivesNewtype' = xs}
    ViaStrategy {} -> mempty
addDerives nt _ xs = mempty
    {derivesStock' = stock
    ,derivesAnyclass = other
    ,derivesNewtype' = if maybe True isNewType nt then filter (`notElem` noDeriveNewtype) xs else []}
    where (stock, other) = partition (`elem` deriveStock) xs

derives :: Located HsModule -> Derives
derives (L _ m) =  mconcat $ map decl (childrenBi m) ++ map idecl (childrenBi m)
  where
    idecl :: DataFamInstDecl GhcPs -> Derives
    idecl (DataFamInstDecl FamEqn {feqn_rhs=HsDataDefn {dd_ND=dn, dd_derivs=ds}}) = g dn ds

    decl :: LHsDecl GhcPs -> Derives
    decl (L _ (TyClD _ (DataDecl _ _ _ _ HsDataDefn {dd_ND=dn, dd_derivs=ds}))) = g dn ds -- Data declaration.
    decl (L _ (DerivD _ (DerivDecl _ (HsWC _ sig) strategy _))) = addDerives Nothing (fmap unLoc strategy) [derivedToStr sig] -- A deriving declaration.
    decl _ = mempty

    g :: NewOrData -> [LHsDerivingClause GhcPs] -> Derives
    g dn ds = mconcat [addDerives (Just dn) (fmap unLoc strategy) $ map derivedToStr tys | (strategy, tys) <- stys]
      where
        stys =
          [(strategy, [ty]) | L _ (HsDerivingClause _ strategy (L _ (DctSingle _ ty))) <- ds] ++
          [(strategy, tys ) | L _ (HsDerivingClause _ strategy (L _ (DctMulti _ tys))) <- ds]

    derivedToStr :: LHsSigType GhcPs -> String
    derivedToStr (L _ (HsSig _ _ t)) = ih t
      where
        ih :: LHsType GhcPs -> String
        ih (L _ (HsQualTy _ _ a)) = ih a
        ih (L _ (HsParTy _ a)) = ih a
        ih (L _ (HsAppTy _ a _)) = ih a
        ih (L _ (HsTyVar _ _ a)) = unsafePrettyPrint $ unqual a
        ih (L _ a) = unsafePrettyPrint a -- I don't anticipate this case is called.

un = undefined

hasT t x = not $ null (universeBi x `asTypeOf` [t])

hasS :: (Data x, Data a) => (a -> Bool) -> x -> Bool
hasS test = any test . universeBi

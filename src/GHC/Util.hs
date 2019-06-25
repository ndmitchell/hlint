{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module GHC.Util (
    baseDynFlags
  , parsePragmasIntoDynFlags
  , parseFileGhcLib
  , ParseResult (..)
  , pprErrMsgBagWithLoc
  , getMessages
  , SDoc
  , Located
  , hseToGhcExtension
  -- Temporary : Export these so GHC doesn't consider them unused and
  -- tell weeder to ignore them.
  , isAtom, addParen, paren, isApp, isOpApp, isAnyApp, isDot, isSection, isDotApp
  ) where

import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" BasicTypes
import "ghc-lib-parser" RdrName
import "ghc-lib-parser" DynFlags
import "ghc-lib-parser" Platform
import "ghc-lib-parser" Fingerprint
import "ghc-lib-parser" Config
import "ghc-lib-parser" Lexer
import "ghc-lib-parser" Parser
import "ghc-lib-parser" SrcLoc
import "ghc-lib-parser" FastString
import "ghc-lib-parser" StringBuffer
import "ghc-lib-parser" ErrUtils
import "ghc-lib-parser" Outputable
import "ghc-lib-parser" GHC.LanguageExtensions.Type
import "ghc-lib-parser" Panic
import "ghc-lib-parser" HscTypes
import "ghc-lib-parser" HeaderInfo

import Data.List
import System.FilePath
import Language.Preprocessor.Unlit
import qualified Language.Haskell.Exts.Extension as HSE
import qualified Data.Map.Strict

fakeSettings :: Settings
fakeSettings = Settings
  { sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=fingerprint0
  }
  where
    platform =
      Platform{platformWordSize=8
              , platformOS=OSUnknown
              , platformUnregisterised=True}
    platformConstants =
      PlatformConstants{pc_DYNAMIC_BY_DEFAULT=False,pc_WORD_SIZE=8}

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])

badExtensions :: [Extension]
badExtensions =
  [
    AlternativeLayoutRule
  , AlternativeLayoutRuleTransitional
  , Arrows
  , TransformListComp
  , UnboxedTuples
  , UnboxedSums
  , QuasiQuotes
  , RecursiveDo
 ]

enabledExtensions :: [Extension]
enabledExtensions = [x | x <- [Cpp .. StarIsType], x `notElem` badExtensions]
-- 'Cpp' are the first and last cases of type 'Extension' in
-- 'libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs'. When we are
-- on a version of GHC that has MR
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/826, we can
-- replace them with 'minBound' and 'maxBound' respectively.

baseDynFlags :: DynFlags
baseDynFlags = foldl' xopt_set
             (defaultDynFlags fakeSettings fakeLlvmConfig) enabledExtensions

-- | Adjust the input 'DynFlags' to take into account language
-- extensions to explicitly enable/disable as well as language
-- extensions enabled by pragma in the source.
parsePragmasIntoDynFlags :: DynFlags
                         -> ([Extension], [Extension])
                         -> FilePath
                         -> String
                         -> IO (Either String DynFlags)
parsePragmasIntoDynFlags flags (enable, disable) filepath str =
  catchErrors $ do
    let opts = getOptions flags (stringToStringBuffer str) filepath
    (flags, _, _) <- parseDynamicFilePragma flags opts
    -- Explicit enable extensions
    let flags' =  foldl' xopt_set flags enable
    -- Explicit disable extensions
    let flags'' = foldl' xopt_unset flags' disable
    return $ Right flags''
  where
    catchErrors :: IO (Either String DynFlags) -> IO (Either String DynFlags)
    catchErrors act = handleGhcException reportErr
                        (handleSourceError reportErr act)
    reportErr e = return $ Left (show e)

parseFileGhcLib ::
  FilePath -> String -> DynFlags -> ParseResult (Located (HsModule GhcPs))
parseFileGhcLib filename str flags =
  Lexer.unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer $
              if takeExtension filename /= ".lhs" then str else unlit filename str
    parseState = mkPState flags buffer location

---------------------------------------------------------------------
-- The following functions are from
-- https://github.com/pepeiborra/haskell-src-exts-util ("Utility code
-- for working with haskell-src-exts") rewritten for GHC parse trees
-- (of which at least one of them came from this project originally).

-- | 'isAtom e' if 'e' requires no bracketing ever.
isAtom :: (p ~ GhcPass pass) => HsExpr p -> Bool
isAtom x = case x of
  HsVar {}          -> True
  HsUnboundVar {}   -> True
  HsRecFld {}       -> True
  HsOverLabel {}    -> True
  HsIPVar {}        -> True
  HsPar {}          -> True
  SectionL {}       -> True
  SectionR {}       -> True
  ExplicitTuple {}  -> True
  ExplicitSum {}    -> True
  ExplicitList {}   -> True
  RecordCon {}      -> True
  RecordUpd {}      -> True
  ArithSeq {}       -> True
  HsBracket {}      -> True
  HsRnBracketOut {} -> True
  HsTcBracketOut {} -> True
  HsSpliceE {}      -> True
  HsLit _ x     | not $ isNegativeLit x     -> True
  HsOverLit _ x | not $ isNegativeOverLit x -> True
  _                 -> False
  where
    isNegativeLit (HsInt _ i) = il_neg i
    isNegativeLit (HsRat _ f _) = fl_neg f
    isNegativeLit (HsFloatPrim _ f) = fl_neg f
    isNegativeLit (HsDoublePrim _ f) = fl_neg f
    isNegativeLit (HsIntPrim _ x) = x < 0
    isNegativeLit (HsInt64Prim _ x) = x < 0
    isNegativeLit (HsInteger _ x _) = x < 0
    isNegativeLit _ = False

    isNegativeOverLit OverLit {ol_val=HsIntegral i} = il_neg i
    isNegativeOverLit OverLit {ol_val=HsFractional f} = fl_neg f
    isNegativeOverLit _ = False

-- | 'addParen e' wraps 'e' in parens.
addParen :: (p ~ GhcPass pass) => HsExpr p -> HsExpr p
addParen e = HsPar noExt (noLoc e)

-- | 'paren e' wraps 'e' in parens if 'e' is non-atomic.
paren :: (p ~ GhcPass pass) => HsExpr GhcPs -> HsExpr GhcPs
paren x
  | isAtom x  = x
  | otherwise = addParen x

-- | 'isApp e' if 'e' is a (regular) application.
isApp :: (p ~ GhcPass pass) => HsExpr p -> Bool
isApp x = case x of
  HsApp {}  -> True
  _         -> False

-- | 'isOpApp e' if 'e' is an operator application.
isOpApp :: (p ~ GhcPass pass) => HsExpr p -> Bool
isOpApp x = case x of
  OpApp {}   -> True
  _          -> False

-- | 'isAnyApp e' if 'e' is either an application or operator
-- application.
isAnyApp :: (p ~ GhcPass pass) => HsExpr p -> Bool
isAnyApp x = isApp x || isOpApp x

-- | 'isDot e'  if 'e' is the unqualifed variable '.'.
isDot :: HsExpr GhcPs -> Bool
isDot x
  | HsVar _ (L _ ident) <- x
    , ident == mkVarUnqual (fsLit ".") = True
  | otherwise                          = False

-- | 'isSection e' if 'e' is a section.
isSection :: (p ~ GhcPass pass) => HsExpr p -> Bool
isSection x = case x of
  SectionL {} -> True
  SectionR {} -> True
  _           -> False

-- | 'isDotApp e' if 'e' is dot application.
isDotApp :: HsExpr GhcPs -> Bool
isDotApp (OpApp _ _ (L _ op) _) = isDot op
isDotApp _ = False

-- | A mapping from 'HSE.KnownExtension' values to their
-- 'GHC.LanguageExtensions.Type.Extension' equivalents.
hseToGhcExtension :: Data.Map.Strict.Map HSE.KnownExtension Extension
hseToGhcExtension =
  Data.Map.Strict.fromList
    [ (HSE.OverlappingInstances, OverlappingInstances)
    , (HSE.UndecidableInstances, UndecidableInstances)
    , (HSE.IncoherentInstances, IncoherentInstances)
    , (HSE.InstanceSigs, InstanceSigs)
    , (HSE.RecursiveDo, RecursiveDo)
    , (HSE.ParallelListComp, ParallelListComp)
    , (HSE.MultiParamTypeClasses, MultiParamTypeClasses)
    , (HSE.MonomorphismRestriction, MonomorphismRestriction)
    , (HSE.FunctionalDependencies, FunctionalDependencies)
    , (HSE.RankNTypes, RankNTypes)
    , (HSE.ExistentialQuantification, ExistentialQuantification)
    , (HSE.ScopedTypeVariables, ScopedTypeVariables)
    , (HSE.ImplicitParams, ImplicitParams)
    , (HSE.FlexibleContexts, FlexibleContexts)
    , (HSE.FlexibleInstances, FlexibleInstances)
    , (HSE.EmptyDataDecls, EmptyDataDecls)
    , (HSE.KindSignatures, KindSignatures)
    , (HSE.BangPatterns, BangPatterns)
    , (HSE.TypeSynonymInstances, TypeSynonymInstances)
    , (HSE.TemplateHaskell, TemplateHaskell)
    , (HSE.ForeignFunctionInterface, ForeignFunctionInterface)
    , (HSE.Arrows, Arrows)
    , (HSE.ImplicitPrelude, ImplicitPrelude)
    , (HSE.PatternGuards, PatternGuards)
    , (HSE.GeneralizedNewtypeDeriving, GeneralizedNewtypeDeriving)
    , (HSE.DeriveAnyClass, DeriveAnyClass)
    , (HSE.MagicHash, MagicHash)
    , (HSE.BinaryLiterals, BinaryLiterals)
    , (HSE.TypeFamilies, TypeFamilies)
    , (HSE.StandaloneDeriving, StandaloneDeriving)
    , (HSE.UnicodeSyntax, UnicodeSyntax)
    , (HSE.UnliftedFFITypes, UnliftedFFITypes)
    , (HSE.LiberalTypeSynonyms, LiberalTypeSynonyms)
    , (HSE.TypeOperators, TypeOperators)
    , (HSE.ParallelArrays, ParallelArrays)
    , (HSE.RecordWildCards, RecordWildCards)
    , (HSE.RecordPuns, RecordPuns)
    , (HSE.DisambiguateRecordFields, DisambiguateRecordFields)
    , (HSE.OverloadedStrings, OverloadedStrings)
    , (HSE.GADTs, GADTs)
    , (HSE.MonoPatBinds, MonoPatBinds)
    , (HSE.RelaxedPolyRec, RelaxedPolyRec)
    , (HSE.ExtendedDefaultRules, ExtendedDefaultRules)
    , (HSE.UnboxedTuples, UnboxedTuples)
    , (HSE.DeriveDataTypeable, DeriveDataTypeable)
    , (HSE.ConstrainedClassMethods, ConstrainedClassMethods)
    , (HSE.PackageImports, PackageImports)
    , (HSE.LambdaCase, LambdaCase)
    , (HSE.EmptyCase, EmptyCase)
    , (HSE.ImpredicativeTypes, ImpredicativeTypes)
    , (HSE.PostfixOperators, PostfixOperators)
    , (HSE.QuasiQuotes, QuasiQuotes)
    , (HSE.TransformListComp, TransformListComp)
    , (HSE.ViewPatterns, ViewPatterns)
    , (HSE.TupleSections, TupleSections)
    , (HSE.GHCForeignImportPrim, GHCForeignImportPrim)
    , (HSE.NPlusKPatterns, NPlusKPatterns)
    , (HSE.DoAndIfThenElse, DoAndIfThenElse)
    , (HSE.RebindableSyntax, RebindableSyntax)
    , (HSE.ExplicitForAll, ExplicitForAll)
    , (HSE.DatatypeContexts, DatatypeContexts)
    , (HSE.MonoLocalBinds, MonoLocalBinds)
    , (HSE.DeriveFunctor, DeriveFunctor)
    , (HSE.DeriveGeneric, DeriveGeneric)
    , (HSE.DeriveTraversable, DeriveTraversable)
    , (HSE.DeriveFoldable, DeriveFoldable)
    , (HSE.NondecreasingIndentation, NondecreasingIndentation)
    , (HSE.InterruptibleFFI, InterruptibleFFI)
    , (HSE.CApiFFI, CApiFFI)
    , (HSE.JavaScriptFFI, JavaScriptFFI)
    , (HSE.ExplicitNamespaces, ExplicitNamespaces)
    , (HSE.DataKinds, DataKinds)
    , (HSE.PolyKinds, PolyKinds)
    , (HSE.MultiWayIf, MultiWayIf)
    , (HSE.DefaultSignatures, DefaultSignatures)
    , (HSE.ConstraintKinds, ConstraintKinds)
    , (HSE.RoleAnnotations, RoleAnnotations)
    , (HSE.PatternSynonyms, PatternSynonyms)
    , (HSE.PartialTypeSignatures, PartialTypeSignatures)
    , (HSE.TypeApplications, TypeApplications)
    , (HSE.TypeFamilyDependencies, TypeFamilyDependencies)
    , (HSE.OverloadedLabels, OverloadedLabels)
    , (HSE.DerivingStrategies, DerivingStrategies)
    , (HSE.UnboxedSums, UnboxedSums)
    , (HSE.TypeInType, TypeInType)
    , (HSE.Strict, Strict)
    , (HSE.StrictData, StrictData)
    , (HSE.DerivingVia, DerivingVia)

    -- These HSE "known extensions" don't appear to have GHC
    -- analogues.

    -- , (HSE.DoRec, DoRec)
    -- , (HSE.Rank2Types, Rank2Types)
    -- , (HSE.PolymorphicComponents, PolymorphicComponents)
    -- , (HSE.PatternSignatures, PatternSignatures)
    -- , (HSE.CPP, CPP)
    -- , (HSE.Generics, Generics)
    -- , (HSE.NamedFieldPuns, NamedFieldPuns)
    -- , (HSE.ExtensibleRecords, ExtensibleRecords)
    -- , (HSE.RestrictedTypeSynonyms, RestrictedTypeSynonyms)
    -- , (HSE.HereDocuments, HereDocuments)
    -- , (HSE.NewQualifiedOperators, NewQualifiedOperators)
    -- , (HSE.XmlSyntax, XmlSyntax)
    -- , (HSE.RegularPatterns, RegularPatterns)
    -- , (HSE.SafeImports, SafeImports)
    -- , (HSE.Safe, Safe)
    -- , (HSE.Trustworthy, Trustworthy)
    -- , (HSE.NamedWildCards, NamedWiledCards)
    ]

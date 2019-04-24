{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module GHC.Util (
    dynFlags
  , parseFileGhcLib
  , ParseResult (..)
  , pprErrMsgBagWithLoc
  , getMessages
  , SDoc
  , Located
  ) where

import "ghc-lib-parser" HsSyn
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

import Data.List
import Language.Preprocessor.Unlit

-- See note [ghc-lib-parser directives] in 'HSE/All.hs'.
{-# ANN module "HLint: ignore Avoid restricted extensions" #-}
{-# ANN module "HLint: ignore Avoid restricted flags" #-}

fakeSettings :: Settings
fakeSettings = Settings
  { sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=Config.cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=Fingerprint.fingerprint0
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

enabledExtensions :: [Extension]
enabledExtensions =
  [
     Cpp
   , OverlappingInstances
   , UndecidableInstances
   , IncoherentInstances
   , UndecidableSuperClasses
   , MonomorphismRestriction
   , MonoPatBinds
   , MonoLocalBinds
   , RelaxedPolyRec           -- Deprecated
   , ExtendedDefaultRules     -- Use GHC's extended rules for defaulting
   , ForeignFunctionInterface
   , UnliftedFFITypes
   , InterruptibleFFI
   , CApiFFI
   , GHCForeignImportPrim
   , JavaScriptFFI
   , ParallelArrays           -- Syntactic support for parallel arrays
   , Arrows                   -- Arrow-notation syntax
   , TemplateHaskell
   , TemplateHaskellQuotes    -- subset of TH supported by stage1, no splice
   , QuasiQuotes
   , ImplicitParams
   , ImplicitPrelude
   , ScopedTypeVariables
   , AllowAmbiguousTypes
   , UnboxedTuples
   , UnboxedSums
   , BangPatterns
   , TypeFamilies
   , TypeFamilyDependencies
   , TypeInType
   , OverloadedStrings
   , OverloadedLists
   , NumDecimals
   , DisambiguateRecordFields
   , RecordWildCards
   , RecordPuns
   , ViewPatterns
   , GADTs
   , GADTSyntax
   , NPlusKPatterns
   , DoAndIfThenElse
   , BlockArguments
   , RebindableSyntax
   , ConstraintKinds
   , PolyKinds                -- Kind polymorphism
   , DataKinds                -- Datatype promotion
   , InstanceSigs
   , ApplicativeDo

   , StandaloneDeriving
   , DeriveDataTypeable
   , AutoDeriveTypeable       -- Automatic derivation of Typeable
   , DeriveFunctor
   , DeriveTraversable
   , DeriveFoldable
   , DeriveGeneric            -- Allow deriving Generic/1
   , DefaultSignatures        -- Allow extra signatures for defmeths
   , DeriveAnyClass           -- Allow deriving any class
   , DeriveLift               -- Allow deriving Lift
   , DerivingStrategies
   , DerivingVia              -- Derive through equal representation

   , TypeSynonymInstances
   , FlexibleContexts
   , FlexibleInstances
   , ConstrainedClassMethods
   , MultiParamTypeClasses
   , NullaryTypeClasses
   , FunctionalDependencies
   , UnicodeSyntax
   , ExistentialQuantification
   , MagicHash
   , EmptyDataDecls
   , KindSignatures
   , RoleAnnotations
   , ParallelListComp
   , TransformListComp
   , MonadComprehensions
   , GeneralizedNewtypeDeriving
   , RecursiveDo
   , PostfixOperators
   , TupleSections
   , PatternGuards
   , LiberalTypeSynonyms
   , RankNTypes
   , ImpredicativeTypes
   , TypeOperators
   , ExplicitNamespaces
   , PackageImports
   , ExplicitForAll
   , AlternativeLayoutRule
   , AlternativeLayoutRuleTransitional
   , DatatypeContexts
   , NondecreasingIndentation
   , RelaxedLayout
   , TraditionalRecordSyntax
   , LambdaCase
   , MultiWayIf
   , BinaryLiterals
   , NegativeLiterals
   , HexFloatLiterals
   , DuplicateRecordFields
   , OverloadedLabels
   , EmptyCase
   , PatternSynonyms
   , PartialTypeSignatures
   , NamedWildCards
   , StaticPointers
   , TypeApplications
   , Strict
   , StrictData
   , MonadFailDesugaring
   , EmptyDataDeriving
   , NumericUnderscores
   , QuantifiedConstraints
   , StarIsType
  ]

dynFlags :: DynFlags
dynFlags = foldl' xopt_set
             (defaultDynFlags fakeSettings fakeLlvmConfig) enabledExtensions

parseFileGhcLib :: String -> String -> ParseResult (Located (HsModule GhcPs))
parseFileGhcLib filename str =
  Lexer.unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer (unlit filename str)
    parseState = mkPState dynFlags buffer location

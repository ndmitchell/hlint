{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module GHC.Util (
    dynFlags
  , parseFileGhcLib
  , ParseResult (..)
  , pprErrMsgBagWithLoc
  , getMessages
  , SDoc
  , Located
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

import Data.List
import System.FilePath
import Language.Preprocessor.Unlit

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
enabledExtensions = [Cpp .. StarIsType] -- First and last extension in ghc-boot-th/GHC/LanguageExtensions/Type.hs 'data Extension'.

dynFlags :: DynFlags
dynFlags = foldl' xopt_set
             (defaultDynFlags fakeSettings fakeLlvmConfig) enabledExtensions

parseFileGhcLib :: FilePath -> String -> ParseResult (Located (HsModule GhcPs))
parseFileGhcLib filename str =
  Lexer.unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer $
              if takeExtension filename /= ".lhs" then str else unlit filename str
    parseState = mkPState dynFlags buffer location

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
isDotApp (OpApp _ _ (L _ op) _)
  | isDot op  = True
  | otherwise = False

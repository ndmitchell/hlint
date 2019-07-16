{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies, NamedFieldPuns #-}
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
  , readExtension
  , commentText, isCommentMultiline
  , declName
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
import "ghc-lib-parser" OccName
import "ghc-lib-parser" FastString
import "ghc-lib-parser" StringBuffer
import "ghc-lib-parser" ErrUtils
import "ghc-lib-parser" Outputable
import "ghc-lib-parser" GHC.LanguageExtensions.Type
import "ghc-lib-parser" Panic
import "ghc-lib-parser" HscTypes
import "ghc-lib-parser" HeaderInfo
import "ghc-lib-parser" ApiAnnotation

import Data.List.Extra
import System.FilePath
import Language.Preprocessor.Unlit
import qualified Data.Map.Strict as Map

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
enabledExtensions = filter (`notElem` badExtensions) enumerateExtensions

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
    let flags' =  foldl' xopt_set flags enable
    let flags'' = foldl' xopt_unset flags' disable
    let flags''' = flags'' `gopt_set` Opt_KeepRawTokenStream
    return $ Right flags'''
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

-- | All available GHC extensions
enumerateExtensions :: [Extension]
-- 'Cpp' are the first and last cases of type 'Extension' in
-- 'libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs'. When we are
-- on a version of GHC that has MR
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/826, we can
-- replace them with 'minBound' and 'maxBound' respectively.
enumerateExtensions = [Cpp .. StarIsType]

-- | Parse a GHC extension
readExtension :: String -> Maybe Extension
readExtension x = Map.lookup x exts
  where exts = Map.fromList [(show x, x) | x <- [Cpp .. StarIsType]]

trimCommentStart s
    | Just s <- stripPrefix "{-" s = s
    | Just s <- stripPrefix "--" s = s
    | otherwise = s

trimCommentEnd s
    | Just s <- stripSuffix "-}" s = s
    | otherwise = s

trimCommentDelims = trimCommentEnd . trimCommentStart

-- | Access to a comment's text.
commentText :: Located AnnotationComment -> String
commentText (L _ (AnnDocCommentNext s)) = trimCommentDelims s
commentText (L _ (AnnDocCommentPrev s)) = trimCommentDelims s
commentText (L _ (AnnDocCommentNamed s)) = trimCommentDelims s
commentText (L _ (AnnDocSection _ s)) = trimCommentDelims s
commentText (L _ (AnnDocOptions s)) = trimCommentDelims s
commentText (L _ (AnnLineComment s)) = trimCommentDelims s
commentText (L _ (AnnBlockComment s)) = trimCommentDelims s

isCommentMultiline :: Located AnnotationComment -> Bool
isCommentMultiline (L _ (AnnBlockComment _)) = True
isCommentMultiline _ = False

declName :: HsDecl GhcPs -> String
declName (TyClD _ FamDecl{tcdFam=FamilyDecl{fdLName}}) = occNameString $ occName $ unLoc fdLName
declName (TyClD _ SynDecl{tcdLName}) = occNameString $ occName $ unLoc tcdLName
declName (TyClD _ DataDecl{tcdLName}) = occNameString $ occName $ unLoc tcdLName
declName (TyClD _ ClassDecl{tcdLName}) = occNameString $ occName $ unLoc tcdLName
declName (ValD _ FunBind{fun_id})  = occNameString $ occName $ unLoc fun_id
declName (ValD _ VarBind{var_id})  = occNameString $ occName var_id
declName (ValD _ (PatSynBind _ PSB{psb_id})) = occNameString $ occName $ unLoc psb_id
declName (SigD _ (TypeSig _ (x:_) _)) = occNameString $ occName $ unLoc x
declName (SigD _ (PatSynSig _ (x:_) _)) = occNameString $ occName $ unLoc x
declName (SigD _ (ClassOpSig _ _ (x:_) _)) = occNameString $ occName $ unLoc x
declName (ForD _ ForeignImport{fd_name}) = occNameString $ occName $ unLoc fd_name
declName (ForD _ ForeignExport{fd_name}) = occNameString $ occName $ unLoc fd_name
declName _ = ""

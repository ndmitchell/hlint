{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies, NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module GHC.Util (
    baseDynFlags
  , parsePragmasIntoDynFlags
  , parseFileGhcLib
  , ParseResult (..)
  , pprErrMsgBagWithLoc
  , dL, cL
  , getMessages
  , SDoc
  , Located
  , readExtension
  , comment, commentText, isCommentMultiline
  , declName, modName, rdrNameName
  , unsafePrettyPrint
  , eqMaybe
  , noloc, unloc, getloc, noext
  , isForD
  , W(..)
  , SrcSpanD(..)
  , isDot, isDol
  , pragmas, flags, langExts, mkFlags, mkLangExts
   -- Temporary : Export these so GHC doesn't consider them unused and
   -- tell weeder to ignore them.
  , isAtom, addParen, paren, isApp, isOpApp, isAnyApp, isSection, isDotApp
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
import "ghc-lib-parser" Module

import Control.Applicative
import Data.Maybe
import Data.List.Extra
import Data.Function
import System.FilePath
import Language.Preprocessor.Unlit
import qualified Data.Map.Strict as Map
import Data.Default

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

baseDynFlags :: DynFlags
baseDynFlags =
  -- The list of default enabled extensions is empty except for
  -- 'TemplateHaskellQuotes'. This is because:
  --  * The extensions to enable/disable are set exclusively in
  --    'parsePragmasIntoDynFlags' based solely on HSE parse flags
  --    (and source level annotations);
  --  * 'TemplateHaskellQuotes' is not a known HSE extension but IS
  --    needed if the GHC parse is to succeed for the unit-test at
  --    hlint.yaml:860
  let enable = [TemplateHaskellQuotes]
  in foldl' xopt_set (defaultDynFlags fakeSettings fakeLlvmConfig) enable

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
isAtom :: HsExpr GhcPs -> Bool
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
addParen :: HsExpr GhcPs -> HsExpr GhcPs
addParen e = HsPar noExt (noLoc e)

-- | 'paren e' wraps 'e' in parens if 'e' is non-atomic.
paren :: HsExpr GhcPs -> HsExpr GhcPs
paren x
  | isAtom x  = x
  | otherwise = addParen x

-- | 'isApp e' if 'e' is a (regular) application.
isApp :: HsExpr GhcPs -> Bool
isApp x = case x of
  HsApp {}  -> True
  _         -> False

-- | 'isOpApp e' if 'e' is an operator application.
isOpApp :: HsExpr GhcPs -> Bool
isOpApp x = case x of
  OpApp {}   -> True
  _          -> False

-- | 'isAnyApp e' if 'e' is either an application or operator
-- application.
isAnyApp :: HsExpr GhcPs -> Bool
isAnyApp x = isApp x || isOpApp x

-- | 'isDot e'  if 'e' is the unqualifed variable '.'.
isDot :: HsExpr GhcPs -> Bool
isDot (HsVar _ (dL -> L _ ident)) = ident == mkVarUnqual (fsLit ".")
isDot _ = False

-- | 'isDol e'  if 'e' is the unqualifed variable '$'.
isDol :: HsExpr GhcPs -> Bool
isDol (HsVar _ (dL -> L _ ident)) = ident == mkVarUnqual (fsLit "$")
isDol _ = False

-- | 'isSection e' if 'e' is a section.
isSection :: HsExpr GhcPs -> Bool
isSection x = case x of
  SectionL {} -> True
  SectionR {} -> True
  _           -> False

-- | 'isForD d' if 'd' is a foreign declaration.
isForD :: HsDecl GhcPs -> Bool
isForD ForD{} = True
isForD _ = False

-- | 'isDotApp e' if 'e' is dot application.
isDotApp :: HsExpr GhcPs -> Bool
isDotApp (OpApp _ _ (dL -> L _ op) _) = isDot op
isDotApp _ = False

-- | Parse a GHC extension
readExtension :: String -> Maybe Extension
readExtension x = Map.lookup x exts
  where exts = Map.fromList [(show x, x) | x <- [Cpp .. StarIsType]]

trimCommentStart :: String -> String
trimCommentStart s
    | Just s <- stripPrefix "{-" s = s
    | Just s <- stripPrefix "--" s = s
    | otherwise = s

trimCommentEnd :: String -> String
trimCommentEnd s
    | Just s <- stripSuffix "-}" s = s
    | otherwise = s

trimCommentDelims :: String -> String
trimCommentDelims = trimCommentEnd . trimCommentStart

-- | A comment as a string.
comment :: Located AnnotationComment -> String
comment (dL -> L _ (AnnBlockComment s)) = s
comment (dL -> L _ (AnnLineComment s)) = s
comment (dL -> L _ (AnnDocOptions s)) = s
comment (dL -> L _ (AnnDocCommentNamed s)) = s
comment (dL -> L _ (AnnDocCommentPrev s)) = s
comment (dL -> L _ (AnnDocCommentNext s)) = s
comment (dL -> L _ (AnnDocSection _ s)) = s

-- | The comment string with delimiters removed.
commentText :: Located AnnotationComment -> String
commentText = trimCommentDelims . comment

isCommentMultiline :: Located AnnotationComment -> Bool
isCommentMultiline (dL -> L _ (AnnBlockComment _)) = True
isCommentMultiline _ = False

-- | @declName x@ returns the \"new name\" that is created (for
-- example a function declaration) by @x@.  If @x@ isn't a declaration
-- that creates a new name (for example an instance declaration),
-- 'Nothing' is returned instead.  This is useful because we don't
-- want to tell users to rename binders that they aren't creating
-- right now and therefore usually cannot change.
declName :: HsDecl GhcPs -> Maybe String
declName x = occNameString . occName <$> case x of
    TyClD _ FamDecl{tcdFam=FamilyDecl{fdLName}} -> Just $ unLoc fdLName
    TyClD _ SynDecl{tcdLName} -> Just $ unLoc tcdLName
    TyClD _ DataDecl{tcdLName} -> Just $ unLoc tcdLName
    TyClD _ ClassDecl{tcdLName} -> Just $ unLoc tcdLName
    ValD _ FunBind{fun_id}  -> Just $ unLoc fun_id
    ValD _ VarBind{var_id}  -> Just var_id
    ValD _ (PatSynBind _ PSB{psb_id}) -> Just $ unLoc psb_id
    SigD _ (TypeSig _ (x:_) _) -> Just $ unLoc x
    SigD _ (PatSynSig _ (x:_) _) -> Just $ unLoc x
    SigD _ (ClassOpSig _ _ (x:_) _) -> Just $ unLoc x
    ForD _ ForeignImport{fd_name} -> Just $ unLoc fd_name
    ForD _ ForeignExport{fd_name} -> Just $ unLoc fd_name
    _ -> Nothing

rdrNameName :: RdrName -> String
rdrNameName = occNameString . rdrNameOcc

modName :: HsModule GhcPs -> String
modName HsModule {hsmodName=Nothing} = "Main"
modName HsModule {hsmodName=Just (dL -> L _ n)} = moduleNameString n

-- \"Unsafely\" in this case means that it uses the following
-- 'DynFlags' for printing -
-- <http://hackage.haskell.org/package/ghc-lib-parser-8.8.0.20190424/docs/src/DynFlags.html#v_unsafeGlobalDynFlags
-- unsafeGlobalDynFlags> This could lead to the issues documented
-- there, but it also might not be a problem for our use case.  TODO:
-- Decide whether this really is unsafe, and if it is, what needs to
-- be done to make it safe.
unsafePrettyPrint :: (Outputable.Outputable a) => a -> String
unsafePrettyPrint = Outputable.showSDocUnsafe . Outputable.ppr

-- | Test if two AST elements are equal modulo annotations.
(=~=) :: Eq a => Located a -> Located a -> Bool
a =~= b = unLoc a == unLoc b

-- | Compare two 'Maybe (Located a)' values for equality modulo
-- locations.
eqMaybe:: Eq a => Maybe (Located a) -> Maybe (Located a) -> Bool
eqMaybe (Just x) (Just y) = x =~= y
eqMaybe Nothing Nothing = True
eqMaybe _ _ = False

noloc :: e -> Located e
noloc = noLoc

unloc :: Located e -> e
unloc = unLoc

getloc :: Located e -> SrcSpan
getloc = getLoc

noext :: NoExt
noext = noExt

newtype SrcSpanD = SrcSpanD SrcSpan deriving (Outputable, Eq, Ord)
instance Default SrcSpanD where def = SrcSpanD noSrcSpan

newtype W a = W a deriving Outputable -- Wrapper of terms.
-- The issue is that at times, terms we work with in this program are
-- not in `Eq` and `Ord` and we need them to be. This work-around
-- resorts to implementing `Eq` and `Ord` for the these types via
-- lexicographical comparisons of string representations. As long as
-- two different terms never map to the same string representation,
-- basing `Eq` and `Ord` on their string representations rather than
-- the term types themselves, leads to identical results.
wToStr :: Outputable a => W a -> String
wToStr (W e) = showPpr baseDynFlags e
instance Outputable a => Eq (W a) where (==) a b = wToStr a == wToStr b
instance Outputable a => Ord (W a) where compare = compare `on` wToStr

-- GHC parse trees don't contain pragmas. We work around this with
-- (nasty) parsing of comments.

-- Pragmas. Comments not associated with a span in the annotations
-- that have the form @{-# ...#-}@.
pragmas :: ApiAnns -> [(Located AnnotationComment, String)]
pragmas anns =
  -- 'ApiAnns' stores pragmas in reverse order to how they were
  -- encountered in the source file with the last at the head of the
  -- list (makes sense when you think about it).
  reverse
    [ (c, s) |
        c@(dL -> L _ (AnnBlockComment comm)) <- fromMaybe [] $ Map.lookup noSrcSpan (snd anns)
      , let body = trimCommentDelims comm
      , Just rest <- [stripSuffix "#" =<< stripPrefix "#" body]
      , let s = trim rest
    ]

-- Utility for a case insensitive prefix strip.
stripPrefixCI :: String -> String -> Maybe String
stripPrefixCI pref str =
  let pref' = lower pref
      (str_pref, rest) = splitAt (length pref') str
  in if lower str_pref == pref' then Just rest else Nothing

-- Flags. The first element of the pair is the (located) annotation
-- comment that sets the flags enumerated in the second element of the
-- pair.
flags :: [(Located AnnotationComment, String)]
      -> [(Located AnnotationComment, [String])]
flags ps =
  -- Old versions of GHC accepted 'OPTIONS' rather than 'OPTIONS_GHC' (but
  -- this is deprecated).
  [(c, opts) | (c, s) <- ps
             , Just rest <- [stripPrefixCI "OPTIONS_GHC " s
                             <|> stripPrefixCI "OPTIONS " s]
             , let opts = words rest]

-- Language extensions. The first element of the pair is the (located)
-- annotation comment that enables the extensions enumerated by he
-- second element of the pair.
langExts :: [(Located AnnotationComment, String)]
         -> [(Located AnnotationComment, [String])]
langExts ps =
  [(c, exts) | (c, s) <- ps
             , Just rest <- [stripPrefixCI "LANGUAGE " s]
             , let exts = map trim (splitOn "," rest)]

-- Given a list of flags, make a GHC options pragma.
mkFlags :: SrcSpan -> [String] -> Located AnnotationComment
mkFlags loc flags =
  cL loc $ AnnBlockComment ("{-# " ++ "OPTIONS_GHC " ++ unwords flags ++ " #-}")

mkLangExts :: SrcSpan -> [String] -> Located AnnotationComment
mkLangExts loc exts =
  cL loc $ AnnBlockComment ("{-# " ++ "LANGUAGE " ++ intercalate ", " exts ++ " #-}")

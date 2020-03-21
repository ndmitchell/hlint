{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module HSE.All(
    CppFlags(..), ParseFlags(..), defaultParseFlags,
    parseFlagsAddFixities, parseFlagsSetLanguage,
    ParseMode(..), defaultParseMode,
    ParseError(..), ModuleEx(..),
    parseModuleEx, ghcComments,
    parseExpGhcWithMode, parseImportDeclGhcWithMode, parseDeclGhcWithMode,
    ) where

import Util
import Data.Char
import Data.List.Extra
import Data.Maybe
import Timing
import Language.Preprocessor.Cpphs
import Data.Either
import DynFlags(Language(..))
import qualified Data.Map as Map
import System.IO.Extra
import Data.Functor
import Fixity
import Extension
import FastString
import Prelude

import qualified HsSyn
import qualified SrcLoc as GHC
import qualified ErrUtils
import qualified Outputable
import qualified Lexer as GHC
import GHC.LanguageExtensions.Type
import qualified ApiAnnotation as GHC
import qualified BasicTypes as GHC
import qualified DynFlags as GHC

import GHC.Util (parsePragmasIntoDynFlags, parseFileGhcLib, parseExpGhcLib, parseDeclGhcLib, parseImportGhcLib, baseDynFlags)
import qualified Language.Haskell.GhclibParserEx.Fixity as GhclibParserEx

-- | What C pre processor should be used.
data CppFlags
    = NoCpp -- ^ No pre processing is done.
    | CppSimple -- ^ Lines prefixed with @#@ are stripped.
    | Cpphs CpphsOptions -- ^ The @cpphs@ library is used.

-- | Created with 'defaultParseFlags', used by 'parseModuleEx'.
data ParseFlags = ParseFlags
    {cppFlags :: CppFlags -- ^ How the file is preprocessed (defaults to 'NoCpp').
    ,hseFlags :: ParseMode -- ^ How the file is parsed (defaults to all fixities in the @base@ package and most non-conflicting extensions).
    }

data ParseMode = ParseMode {
        -- | base language (e.g. Haskell98, Haskell2010)
        baseLanguage :: Maybe Language,
        -- | list of extensions enabled for parsing
        extensions :: [Extension],
        -- | list of fixities to be aware of
        fixities :: [FixityInfo]
        }

defaultParseMode :: ParseMode
defaultParseMode =
  ParseMode {
      baseLanguage = Nothing
    , extensions = defaultExtensions
    , fixities = defaultFixities
    }

-- | Default value for 'ParseFlags'.
defaultParseFlags :: ParseFlags
defaultParseFlags = ParseFlags NoCpp defaultParseMode

-- | Given some fixities, add them to the existing fixities in 'ParseFlags'.
parseFlagsAddFixities :: [FixityInfo] -> ParseFlags -> ParseFlags
parseFlagsAddFixities fx x = x{hseFlags=hse{fixities = fx ++ fixities hse}}
    where hse = hseFlags x

parseFlagsSetLanguage :: (Maybe Language, [Extension]) -> ParseFlags -> ParseFlags
parseFlagsSetLanguage (l, es) x = x{hseFlags=(hseFlags x){baseLanguage = l, extensions = es}}


runCpp :: CppFlags -> FilePath -> String -> IO String
runCpp NoCpp _ x = pure x
runCpp CppSimple _ x = pure $ unlines [if "#" `isPrefixOf` trimStart x then "" else x | x <- lines x]
runCpp (Cpphs o) file x = dropLine <$> runCpphs o file x
    where
        -- LINE pragmas always inserted when locations=True
        dropLine (line1 -> (a,b)) | "{-# LINE " `isPrefixOf` a = b
        dropLine x = x

---------------------------------------------------------------------
-- PARSING

-- | A parse error.
data ParseError = ParseError
    { parseErrorLocation :: GHC.SrcSpan -- ^ Location of the error.
    , parseErrorMessage :: String  -- ^ Message about the cause of the error.
    , parseErrorContents :: String -- ^ Snippet of several lines (typically 5) including a @>@ character pointing at the faulty line.
    }

-- | Result of 'parseModuleEx', representing a parsed module.
data ModuleEx = ModuleEx {
    ghcModule :: GHC.Located (HsSyn.HsModule HsSyn.GhcPs)
  , ghcAnnotations :: GHC.ApiAnns
}

-- | Extract a list of all of a parsed module's comments.
ghcComments :: ModuleEx -> [GHC.Located GHC.AnnotationComment]
ghcComments m = concat (Map.elems $ snd (ghcAnnotations m))


-- | The error handler invoked when GHC parsing has failed.
ghcFailOpParseModuleEx :: String
                       -> FilePath
                       -> String
                       -> (GHC.SrcSpan, ErrUtils.MsgDoc)
                       -> IO (Either ParseError ModuleEx)
ghcFailOpParseModuleEx ppstr file str (loc, err) = do
   let pe = case loc of
            GHC.RealSrcSpan r -> context (GHC.srcSpanStartLine r) ppstr
            _ -> ""
       msg = Outputable.showSDoc baseDynFlags $
               ErrUtils.pprLocErrMsg (ErrUtils.mkPlainErrMsg baseDynFlags loc err)
   pure $ Left $ ParseError loc msg pe

-- A hacky function to get fixities from HSE parse flags suitable for
-- use by our own 'GHC.Util.Refact.Fixity' module.
ghcFixitiesFromParseMode :: ParseMode -> [(String, GHC.Fixity)]
ghcFixitiesFromParseMode = map toFixity . fixities


-- GHC enabled/disabled extensions given an HSE parse mode.
ghcExtensionsFromParseMode :: ParseMode -> ([Extension], [Extension])
ghcExtensionsFromParseMode ParseMode {extensions=exts}= (exts, [])

-- GHC extensions to enable/disable given HSE parse flags.
ghcExtensionsFromParseFlags :: ParseFlags -> ([Extension], [Extension])
ghcExtensionsFromParseFlags ParseFlags {hseFlags=mode} = ghcExtensionsFromParseMode mode

-- GHC fixities given HSE parse flags.
ghcFixitiesFromParseFlags :: ParseFlags -> [(String, GHC.Fixity)]
ghcFixitiesFromParseFlags ParseFlags {hseFlags=mode} = ghcFixitiesFromParseMode mode

-- These next two functions get called frorm 'Config/Yaml.hs' for user
-- defined hint rules.

parseModeToFlags :: ParseMode -> GHC.DynFlags
parseModeToFlags parseMode =
    flip GHC.lang_set (baseLanguage parseMode) $ foldl' GHC.xopt_unset (foldl' GHC.xopt_set baseDynFlags enable) disable
  where
    (enable, disable) = ghcExtensionsFromParseMode parseMode

parseExpGhcWithMode :: ParseMode -> String -> GHC.ParseResult (HsSyn.LHsExpr HsSyn.GhcPs)
parseExpGhcWithMode parseMode s =
  let fixities = ghcFixitiesFromParseMode parseMode
  in case parseExpGhcLib s $ parseModeToFlags parseMode of
    GHC.POk pst a -> GHC.POk pst (GhclibParserEx.applyFixities fixities a)
    f@GHC.PFailed{} -> f

parseImportDeclGhcWithMode :: ParseMode -> String -> GHC.ParseResult (HsSyn.LImportDecl HsSyn.GhcPs)
parseImportDeclGhcWithMode parseMode s =
  parseImportGhcLib s $ parseModeToFlags parseMode

parseDeclGhcWithMode :: ParseMode -> String -> GHC.ParseResult (HsSyn.LHsDecl HsSyn.GhcPs)
parseDeclGhcWithMode parseMode s =
  let fixities = ghcFixitiesFromParseMode parseMode
  in case parseDeclGhcLib s $ parseModeToFlags parseMode of
    GHC.POk pst a -> GHC.POk pst (GhclibParserEx.applyFixities fixities a)
    f@GHC.PFailed{} -> f

-- | Parse a Haskell module. Applies the C pre processor, and uses
-- best-guess fixity resolution if there are ambiguities.  The
-- filename @-@ is treated as @stdin@. Requires some flags (often
-- 'defaultParseFlags'), the filename, and optionally the contents of
-- that file. This version uses both hs-src-exts AND ghc-lib.
parseModuleEx :: ParseFlags -> FilePath -> Maybe String -> IO (Either ParseError ModuleEx)
parseModuleEx flags file str = timedIO "Parse" file $ do
        str <- case str of
            Just x -> pure x
            Nothing | file == "-" -> getContentsUTF8
                    | otherwise -> readFileUTF8' file
        str <- pure $ dropPrefix "\65279" str -- remove the BOM if it exists, see #130
        ppstr <- runCpp (cppFlags flags) file str
        let enableDisableExts = ghcExtensionsFromParseFlags flags
            fixities = ghcFixitiesFromParseFlags flags -- Note : Fixities are coming from HSE parse flags.
        dynFlags <- parsePragmasIntoDynFlags baseDynFlags enableDisableExts file ppstr
        case dynFlags of
          Right ghcFlags -> do
            ghcFlags <- pure $ GHC.lang_set ghcFlags $ baseLanguage $ hseFlags flags
            case parseFileGhcLib file ppstr ghcFlags of
                GHC.POk pst a ->
                    let anns =
                          ( Map.fromListWith (++) $ GHC.annotations pst
                          , Map.fromList ((GHC.noSrcSpan, GHC.comment_q pst) : GHC.annotations_comments pst)
                          ) in
                    let a' = GhclibParserEx.applyFixities fixities a in
                    pure $ Right (ModuleEx a' anns)
                -- Parse error if GHC parsing fails (see
                -- https://github.com/ndmitchell/hlint/issues/645).
                GHC.PFailed _ loc err ->
                    ghcFailOpParseModuleEx ppstr file str (loc, err)
          Left msg -> do
            -- Parsing GHC flags from dynamic pragmas in the source
            -- has failed. When this happens, it's reported by
            -- exception. It's impossible or at least fiddly getting a
            -- location so we skip that for now. Synthesize a parse
            -- error.
            let loc = GHC.mkSrcLoc (mkFastString file) (1 :: Int) (1 :: Int)
            pure $ Left (ParseError (GHC.mkSrcSpan loc loc) msg ppstr)


-- | Given a line number, and some source code, put bird ticks around the appropriate bit.
context :: Int -> String -> String
context lineNo src =
    unlines $ dropWhileEnd (all isSpace) $ dropWhile (all isSpace) $
    zipWith (++) ticks $ take 5 $ drop (lineNo - 3) $ lines src ++ ["","","","",""]
    where ticks = drop (3 - lineNo) ["  ","  ","> ","  ","  "]

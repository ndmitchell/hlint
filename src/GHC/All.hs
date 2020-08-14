{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.All(
    CppFlags(..), ParseFlags(..), defaultParseFlags,
    parseFlagsAddFixities, parseFlagsSetLanguage,
    ParseError(..), ModuleEx(..),
    parseModuleEx, createModuleEx, ghcComments,
    parseExpGhcWithMode, parseImportDeclGhcWithMode, parseDeclGhcWithMode,
    ) where

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Util
import Data.Char
import Data.List.Extra
import Timing
import Language.Preprocessor.Cpphs
import qualified Data.Map as Map
import System.IO.Extra
import Fixity
import Extension
import FastString

import GHC.Hs
import SrcLoc
import ErrUtils
import Outputable
import Lexer hiding (context)
import GHC.LanguageExtensions.Type
import ApiAnnotation
import DynFlags hiding (extensions)
import Bag

import Language.Haskell.GhclibParserEx.GHC.Parser
import Language.Haskell.GhclibParserEx.Fixity
import GHC.Util

-- | What C pre processor should be used.
data CppFlags
    = NoCpp -- ^ No pre processing is done.
    | CppSimple -- ^ Lines prefixed with @#@ are stripped.
    | Cpphs CpphsOptions -- ^ The @cpphs@ library is used.

-- | Created with 'defaultParseFlags', used by 'parseModuleEx'.
data ParseFlags = ParseFlags
    {cppFlags :: CppFlags -- ^ How the file is preprocessed (defaults to 'NoCpp').
    ,baseLanguage :: Maybe Language -- ^ Base language (e.g. Haskell98, Haskell2010), defaults to 'Nothing'.
    ,enabledExtensions :: [Extension] -- ^ List of extensions enabled for parsing, defaults to many non-conflicting extensions.
    ,disabledExtensions :: [Extension] -- ^ List of extensions disabled for parsing, usually empty.
    ,fixities :: [FixityInfo] -- ^ List of fixities to be aware of, defaults to those defined in @base@.
    }

-- | Default value for 'ParseFlags'.
defaultParseFlags :: ParseFlags
defaultParseFlags = ParseFlags NoCpp Nothing defaultExtensions [] defaultFixities

-- | Given some fixities, add them to the existing fixities in 'ParseFlags'.
parseFlagsAddFixities :: [FixityInfo] -> ParseFlags -> ParseFlags
parseFlagsAddFixities fx x = x{fixities = fx ++ fixities x}

parseFlagsSetLanguage :: (Maybe Language, ([Extension], [Extension])) -> ParseFlags -> ParseFlags
parseFlagsSetLanguage (l, (es, ds)) x = x{baseLanguage = l, enabledExtensions = es, disabledExtensions = ds}


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
    { parseErrorLocation :: SrcSpan -- ^ Location of the error.
    , parseErrorMessage :: String  -- ^ Message about the cause of the error.
    , parseErrorContents :: String -- ^ Snippet of several lines (typically 5) including a @>@ character pointing at the faulty line.
    }

-- | Result of 'parseModuleEx', representing a parsed module.
data ModuleEx = ModuleEx {
    ghcModule :: Located (HsModule GhcPs)
  , ghcAnnotations :: ApiAnns
}

-- | Extract a list of all of a parsed module's comments.
ghcComments :: ModuleEx -> [Located AnnotationComment]
ghcComments m = concat (Map.elems $ snd (ghcAnnotations m))


-- | The error handler invoked when GHC parsing has failed.
ghcFailOpParseModuleEx :: String
                       -> FilePath
                       -> String
                       -> (SrcSpan, ErrUtils.MsgDoc)
                       -> IO (Either ParseError ModuleEx)
ghcFailOpParseModuleEx ppstr file str (loc, err) = do
   let pe = case loc of
            RealSrcSpan r -> context (srcSpanStartLine r) ppstr
            _ -> ""
       msg = Outputable.showSDoc baseDynFlags err
   pure $ Left $ ParseError loc msg pe

-- GHC extensions to enable/disable given HSE parse flags.
ghcExtensionsFromParseFlags :: ParseFlags -> ([Extension], [Extension])
ghcExtensionsFromParseFlags ParseFlags{enabledExtensions=es, disabledExtensions=ds}= (es, ds)

-- GHC fixities given HSE parse flags.
ghcFixitiesFromParseFlags :: ParseFlags -> [(String, Fixity)]
ghcFixitiesFromParseFlags = map toFixity . fixities

-- These next two functions get called frorm 'Config/Yaml.hs' for user
-- defined hint rules.

parseModeToFlags :: ParseFlags -> DynFlags
parseModeToFlags parseMode =
    flip lang_set (baseLanguage parseMode) $ foldl' xopt_unset (foldl' xopt_set baseDynFlags enable) disable
  where
    (enable, disable) = ghcExtensionsFromParseFlags parseMode

parseExpGhcWithMode :: ParseFlags -> String -> ParseResult (LHsExpr GhcPs)
parseExpGhcWithMode parseMode s =
  let fixities = ghcFixitiesFromParseFlags parseMode
  in case parseExpression s $ parseModeToFlags parseMode of
    POk pst a -> POk pst $ applyFixities fixities a
    f@PFailed{} -> f

parseImportDeclGhcWithMode :: ParseFlags -> String -> ParseResult (LImportDecl GhcPs)
parseImportDeclGhcWithMode parseMode s =
  parseImport s $ parseModeToFlags parseMode

parseDeclGhcWithMode :: ParseFlags -> String -> ParseResult (LHsDecl GhcPs)
parseDeclGhcWithMode parseMode s =
  let fixities = ghcFixitiesFromParseFlags parseMode
  in case parseDeclaration s $ parseModeToFlags parseMode of
    POk pst a -> POk pst $ applyFixities fixities a
    f@PFailed{} -> f

-- | Create a 'ModuleEx' from GHC annotations and module tree. It
-- is assumed the incoming parse module has not been adjusted to
-- account for operator fixities (it uses the HLint default fixities).
createModuleEx :: ApiAnns -> Located (HsModule GhcPs) -> ModuleEx
createModuleEx anns ast =
  ModuleEx (applyFixities (fixitiesFromModule ast ++ map toFixity defaultFixities) ast) anns

-- | Parse a Haskell module. Applies the C pre processor, and uses
-- best-guess fixity resolution if there are ambiguities.  The
-- filename @-@ is treated as @stdin@. Requires some flags (often
-- 'defaultParseFlags'), the filename, and optionally the contents of
-- that file.
--
-- Note that certain programs, e.g. @main = do@ successfully parse
-- with GHC, but then fail with an error in the renamer. These
-- programs will return a successful parse.
parseModuleEx :: ParseFlags -> FilePath -> Maybe String -> IO (Either ParseError ModuleEx)
parseModuleEx flags file str = timedIO "Parse" file $ runExceptT $ do
  str <- case str of
    Just x -> pure x
    Nothing | file == "-" -> liftIO getContentsUTF8
            | otherwise -> liftIO $ readFileUTF8' file
  str <- pure $ dropPrefix "\65279" str -- Remove the BOM if it exists, see #130.
  let enableDisableExts = ghcExtensionsFromParseFlags flags
  -- Read pragmas for the first time.
  dynFlags <- withExceptT (parsePragmasErr str) $ ExceptT (parsePragmasIntoDynFlags baseDynFlags enableDisableExts file str)
  dynFlags <- pure $ lang_set dynFlags $ baseLanguage flags
  -- Avoid running cpp unless CPP is enabled, see #1075.
  str <- if not (xopt Cpp dynFlags) then pure str else liftIO $ runCpp (cppFlags flags) file str
  -- If we preprocessed the file, re-read the pragmas.
  dynFlags <- if not (xopt Cpp dynFlags) then pure dynFlags
              else withExceptT (parsePragmasErr str) $ ExceptT (parsePragmasIntoDynFlags baseDynFlags enableDisableExts file str)
  dynFlags <- pure $ lang_set dynFlags $ baseLanguage flags
  -- Done with pragmas. Proceed to parsing.
  case fileToModule file str dynFlags of
    POk s a -> do
      let errs = bagToList . snd $ getMessages s dynFlags
      if not $ null errs then
        ExceptT $ parseFailureErr dynFlags str file str errs
      else do
        let anns =
              ( Map.fromListWith (++) $ annotations s
              , Map.fromList ((noSrcSpan, comment_q s) : annotations_comments s)
              )
        let fixes = fixitiesFromModule a ++ ghcFixitiesFromParseFlags flags
        pure $ ModuleEx (applyFixities fixes a) anns
    PFailed s ->
      ExceptT $ parseFailureErr dynFlags str file str $  bagToList . snd $ getMessages s dynFlags
  where
    -- If parsing pragmas fails, synthesize a parse error from the
    -- error message.
    parsePragmasErr src msg =
      let loc = mkSrcLoc (mkFastString file) (1 :: Int) (1 :: Int)
      in ParseError (mkSrcSpan loc loc) msg src

    parseFailureErr dynFlags ppstr file str errs =
      let errMsg = head errs
          loc = errMsgSpan errMsg
          doc = formatErrDoc dynFlags (errMsgDoc errMsg)
      in ghcFailOpParseModuleEx ppstr file str (loc, doc)

-- | Given a line number, and some source code, put bird ticks around the appropriate bit.
context :: Int -> String -> String
context lineNo src =
    unlines $ dropWhileEnd (all isSpace) $ dropWhile (all isSpace) $
    zipWith (++) ticks $ take 5 $ drop (lineNo - 3) $ lines src ++ ["","","","",""]
    where ticks = drop (3 - lineNo) ["  ","  ","> ","  ","  "]

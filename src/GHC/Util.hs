
module GHC.Util (
    module GHC.Util.View
  , module GHC.Util.FreeVars
  , module GHC.Util.ApiAnnotation
  , module GHC.Util.HsDecl
  , module GHC.Util.HsExpr
  , module GHC.Util.HsType
  , module GHC.Util.LanguageExtensions.Type
  , module GHC.Util.Pat
  , module GHC.Util.Module
  , module GHC.Util.Outputable
  , module GHC.Util.SrcLoc
  , module GHC.Util.W
  , module GHC.Util.DynFlags
  , module GHC.Util.Scope
  , module GHC.Util.RdrName
  , module GHC.Util.Unify

  , parsePragmasIntoDynFlags, parseFileGhcLib, parseExpGhcLib, parseImportGhcLib
  ) where

import GHC.Util.View
import GHC.Util.FreeVars
import GHC.Util.ApiAnnotation
import GHC.Util.HsExpr
import GHC.Util.HsType
import GHC.Util.HsDecl
import GHC.Util.LanguageExtensions.Type
import GHC.Util.Pat
import GHC.Util.Module
import GHC.Util.Outputable
import GHC.Util.SrcLoc
import GHC.Util.W
import GHC.Util.DynFlags
import GHC.Util.RdrName
import GHC.Util.Scope
import GHC.Util.Unify

import HsSyn
import Lexer
import Parser
import SrcLoc
import FastString
import StringBuffer
import GHC.LanguageExtensions.Type
import Panic
import HscTypes
import HeaderInfo
import DynFlags

import Data.List.Extra
import System.FilePath
import Language.Preprocessor.Unlit

parseGhcLib :: P a -> String -> DynFlags -> ParseResult a
parseGhcLib p str flags =
  Lexer.unP p parseState
  where
    location = mkRealSrcLoc (mkFastString "<string>") 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

parseExpGhcLib :: String -> DynFlags -> ParseResult (LHsExpr GhcPs)
parseExpGhcLib = parseGhcLib Parser.parseExpression

parseImportGhcLib :: String -> DynFlags -> ParseResult (LImportDecl GhcPs)
parseImportGhcLib = parseGhcLib Parser.parseImport

parseFileGhcLib :: FilePath
                -> String
                -> DynFlags
                -> ParseResult (Located (HsModule GhcPs))
parseFileGhcLib filename str flags =
  Lexer.unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer $
              if takeExtension filename /= ".lhs" then str else unlit filename str
    parseState = mkPState flags buffer location

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

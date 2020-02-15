
module GHC.Util (
    module GHC.Util.View
  , module GHC.Util.FreeVars
  , module GHC.Util.ApiAnnotation
  , module GHC.Util.HsDecl
  , module GHC.Util.HsExpr
  , module GHC.Util.HsType
  , module GHC.Util.Pat
  , module GHC.Util.Module
  , module GHC.Util.Outputable
  , module GHC.Util.SrcLoc
  , module GHC.Util.DynFlags
  , module GHC.Util.Scope
  , module GHC.Util.RdrName
  , module GHC.Util.Unify

  , parsePragmasIntoDynFlags
  , parseFileGhcLib, parseExpGhcLib, parseImportGhcLib
  ) where

import GHC.Util.View
import GHC.Util.FreeVars
import GHC.Util.ApiAnnotation
import GHC.Util.HsExpr
import GHC.Util.HsType
import GHC.Util.HsDecl
import GHC.Util.Pat
import GHC.Util.Module
import GHC.Util.Outputable
import GHC.Util.SrcLoc
import GHC.Util.DynFlags
import GHC.Util.RdrName
import GHC.Util.Scope
import GHC.Util.Unify

import qualified Language.Haskell.GhclibParserEx.Parse as GhclibParserEx
import Language.Haskell.GhclibParserEx.DynFlags (parsePragmasIntoDynFlags)

import HsSyn
import Lexer
import SrcLoc
import DynFlags

import System.FilePath
import Language.Preprocessor.Unlit

parseExpGhcLib :: String -> DynFlags -> ParseResult (LHsExpr GhcPs)
parseExpGhcLib = GhclibParserEx.parseExpression

parseImportGhcLib :: String -> DynFlags -> ParseResult (LImportDecl GhcPs)
parseImportGhcLib = GhclibParserEx.parseImport

parseFileGhcLib :: FilePath -> String -> DynFlags -> ParseResult (Located (HsModule GhcPs))
parseFileGhcLib filename str flags =
  GhclibParserEx.parseFile filename flags
    (if takeExtension filename /= ".lhs" then str else unlit filename str)

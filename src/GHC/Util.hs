{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Util (
    module GHC.Util.View
  , module GHC.Util.FreeVars
  , module GHC.Util.ApiAnnotation
  , module GHC.Util.HsDecl
  , module GHC.Util.HsExpr
  , module GHC.Util.Module
  , module GHC.Util.Outputable
  , module GHC.Util.SrcLoc
  , module GHC.Util.DynFlags
  , module GHC.Util.Scope
  , module GHC.Util.RdrName
  , module GHC.Util.Unify

  , parsePragmasIntoDynFlags
  , parseFileGhcLib, parseExpGhcLib, parseImportGhcLib, parseDeclGhcLib
  , pattern SrcSpan, srcSpanFilename, srcSpanStartLine', srcSpanStartColumn, srcSpanEndLine', srcSpanEndColumn
  , pattern SrcLoc, srcFilename, srcLine, srcColumn
  , showSrcSpan',
  ) where

import GHC.Util.View
import GHC.Util.FreeVars
import GHC.Util.ApiAnnotation
import GHC.Util.HsExpr
import GHC.Util.HsDecl
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
import FastString

import System.FilePath
import Language.Preprocessor.Unlit

parseExpGhcLib :: String -> DynFlags -> ParseResult (LHsExpr GhcPs)
parseExpGhcLib = GhclibParserEx.parseExpression

parseImportGhcLib :: String -> DynFlags -> ParseResult (LImportDecl GhcPs)
parseImportGhcLib = GhclibParserEx.parseImport

parseDeclGhcLib :: String -> DynFlags -> ParseResult (LHsDecl GhcPs)
parseDeclGhcLib = GhclibParserEx.parseDeclaration

parseFileGhcLib :: FilePath -> String -> DynFlags -> ParseResult (Located (HsModule GhcPs))
parseFileGhcLib filename str flags =
  GhclibParserEx.parseFile filename flags
    (if takeExtension filename /= ".lhs" then str else unlit filename str)

{-# COMPLETE SrcSpan #-}
-- | The \"Line'\" thing is because there is already e.g. 'SrcLoc.srcSpanStartLine'
pattern SrcSpan :: String -> Int -> Int -> Int -> Int -> SrcSpan
pattern SrcSpan
  { srcSpanFilename
  , srcSpanStartLine'
  , srcSpanStartColumn
  , srcSpanEndLine'
  , srcSpanEndColumn
  }
  <-
    (toOldeSpan ->
      ( srcSpanFilename
      , srcSpanStartLine'
      , srcSpanStartColumn
      , srcSpanEndLine'
      , srcSpanEndColumn
      ))

toOldeSpan :: SrcSpan -> (String, Int, Int, Int, Int)
toOldeSpan (RealSrcSpan span) =
  ( unpackFS $ srcSpanFile span
  , srcSpanStartLine span
  , srcSpanStartCol span
  , srcSpanEndLine span
  , srcSpanEndCol span
  )
-- TODO: the bad locations are all (-1) right now
-- is this fine? it should be, since noLoc from HSE previously also used (-1) as an invalid location
toOldeSpan (UnhelpfulSpan str) =
  ( unpackFS str
  , -1
  , -1
  , -1
  , -1
  )

{-# COMPLETE SrcLoc #-}
pattern SrcLoc :: String -> Int -> Int -> SrcLoc
pattern SrcLoc
  { srcFilename
  , srcLine
  , srcColumn
  }
  <-
    (toOldeLoc ->
      ( srcFilename
      , srcLine
      , srcColumn
      ))

toOldeLoc :: SrcLoc -> (String, Int, Int)
toOldeLoc (RealSrcLoc loc) =
  ( unpackFS $ srcLocFile loc
  , srcLocLine loc
  , srcLocCol loc
  )
toOldeLoc (UnhelpfulLoc str) =
  ( unpackFS str
  , -1
  , -1
  )

showSrcSpan' :: SrcSpan -> String
showSrcSpan' = unsafePrettyPrint

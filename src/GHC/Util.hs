{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Util (
    module GHC.Util.View
  , module GHC.Util.FreeVars
  , module GHC.Util.ApiAnnotation
  , module GHC.Util.HsDecl
  , module GHC.Util.HsExpr
  , module GHC.Util.SrcLoc
  , module GHC.Util.DynFlags
  , module GHC.Util.Scope
  , module GHC.Util.Unify
  , parsePragmasIntoDynFlags
  , fileToModule
  , pattern SrcSpan, srcSpanFilename, srcSpanStartLine', srcSpanStartColumn, srcSpanEndLine', srcSpanEndColumn
  , pattern SrcLoc, srcFilename, srcLine, srcColumn
  , showSrcSpan,
  ) where

import GHC.Util.View
import GHC.Util.FreeVars
import GHC.Util.ApiAnnotation
import GHC.Util.HsExpr
import GHC.Util.HsDecl
import GHC.Util.SrcLoc
import GHC.Util.DynFlags
import GHC.Util.Scope
import GHC.Util.Unify

import Language.Haskell.GhclibParserEx.GHC.Parser (parseFile)
import Language.Haskell.GhclibParserEx.GHC.Driver.Session (parsePragmasIntoDynFlags)
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

import GHC.Hs
import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import GHC.Driver.Session
import GHC.Data.FastString

import System.FilePath
import Language.Preprocessor.Unlit

fileToModule :: FilePath -> String -> DynFlags -> ParseResult (Located HsModule)
fileToModule filename str flags =
  parseFile filename flags
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
toOldeSpan (RealSrcSpan span _) =
  ( unpackFS $ srcSpanFile span
  , srcSpanStartLine span
  , srcSpanStartCol span
  , srcSpanEndLine span
  , srcSpanEndCol span
  )
-- TODO: the bad locations are all (-1) right now
-- is this fine? it should be, since noLoc from HSE previously also used (-1) as an invalid location
toOldeSpan (UnhelpfulSpan _) =
  ( "no-span"
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
toOldeLoc (RealSrcLoc loc _) =
  ( unpackFS $ srcLocFile loc
  , srcLocLine loc
  , srcLocCol loc
  )
toOldeLoc (UnhelpfulLoc _) =
  ( "no-loc"
  , -1
  , -1
  )

showSrcSpan :: SrcSpan -> String
showSrcSpan = unsafePrettyPrint

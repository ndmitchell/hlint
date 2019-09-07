{-# LANGUAGE PackageImports #-}

module GHC.Util (
    module GHC.Util.View
  , module GHC.Util.FreeVars
  , module GHC.Util.ApiAnnotation
  , module GHC.Util.HsDecl
  , module GHC.Util.HsExpr
  , module GHC.Util.LanguageExtensions.Type
  , module GHC.Util.Pat
  , module GHC.Util.Module
  , module GHC.Util.Outputable
  , module GHC.Util.SrcLoc
  , module GHC.Util.W
  , module GHC.Util.DynFlags

  , parsePragmasIntoDynFlags, parseFileGhcLib
  ) where

import GHC.Util.View
import GHC.Util.FreeVars
import GHC.Util.ApiAnnotation
import GHC.Util.HsExpr
import GHC.Util.HsType ()
import GHC.Util.HsDecl
import GHC.Util.LanguageExtensions.Type
import GHC.Util.Pat
import GHC.Util.Module
import GHC.Util.Outputable
import GHC.Util.SrcLoc
import GHC.Util.W
import GHC.Util.DynFlags

import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" Lexer
import "ghc-lib-parser" Parser
import "ghc-lib-parser" SrcLoc
import "ghc-lib-parser" FastString
import "ghc-lib-parser" StringBuffer
import "ghc-lib-parser" GHC.LanguageExtensions.Type
import "ghc-lib-parser" Panic
import "ghc-lib-parser" HscTypes
import "ghc-lib-parser" HeaderInfo
import "ghc-lib-parser" DynFlags

import Data.List.Extra
import System.FilePath
import Language.Preprocessor.Unlit

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

{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module GHC.Util (
    dynFlags
  , parseFileGhcLib
  , ParseResult (..)
  , pprErrMsgBagWithLoc
  , getMessages
  , SDoc
  , Located
  ) where

import "ghc-lib-parser" HsSyn
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

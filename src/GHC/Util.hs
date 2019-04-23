{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

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

-- See note [ghc-lib-parser directives] in 'HSE/All.hs'.
{-# ANN module "HLint: ignore Avoid restricted extensions" #-}
{-# ANN module "HLint: ignore Avoid restricted flags" #-}

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

dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

parseFileGhcLib :: String -> String -> ParseResult (Located (HsModule GhcPs))
parseFileGhcLib filename str =
  Lexer.unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState dynFlags buffer location

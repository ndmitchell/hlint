{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module GHC.Util.DynFlags (baseDynFlags) where

import DynFlags
import Platform
import Config
import Fingerprint
import GHC.LanguageExtensions.Type

import Data.List.Extra

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

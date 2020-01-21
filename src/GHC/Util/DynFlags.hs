module GHC.Util.DynFlags (baseDynFlags) where

import DynFlags
import GHC.LanguageExtensions.Type
import Data.List.Extra
import Language.Haskell.GhclibParserEx.Parse

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

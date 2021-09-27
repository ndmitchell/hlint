module GHC.Util.DynFlags (initGlobalDynFlags, baseDynFlags) where

import GHC.Driver.Session
import Language.Haskell.GhclibParserEx.GHC.Settings.Config

-- The list of default enabled extensions is empty. This is because:
-- the extensions to enable/disable are set exclusively in
-- 'parsePragmasIntoDynFlags' based solely on the parse flags
-- (and source level annotations).
baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

initGlobalDynFlags :: IO ()
initGlobalDynFlags = setUnsafeGlobalDynFlags baseDynFlags

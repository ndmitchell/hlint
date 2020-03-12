
module HSE.Util(
    extensionImpliedBy,
    extensionImplies
    ) where

import qualified Data.Map as Map
import qualified Language.Haskell.GhclibParserEx.DynFlags as GhclibParserEx
import Language.Haskell.Exts(Extension(..), parseExtension)

-- | This extension implies the following extensions
extensionImplies :: Extension -> [Extension]
extensionImplies = \x -> Map.findWithDefault [] x mp
    where mp = Map.fromList extensionImplications

-- | This extension is implied by the following extensions
extensionImpliedBy :: Extension -> [Extension]
extensionImpliedBy = \x -> Map.findWithDefault [] x mp
    where mp = Map.fromListWith (++) [(b, [a]) | (a,bs) <- extensionImplications, b <- bs]

-- | (a, bs) means extension a implies all of bs. Uses GHC source at
-- DynFlags.impliedXFlags
extensionImplications :: [(Extension, [Extension])]
extensionImplications = map toHse GhclibParserEx.extensionImplications
  where
    enable ext = parseExtension (show ext)
    disable ext = parseExtension ("No" ++ show ext)
    toHse (e, (enables, disables)) = (enable e, map enable enables ++ map disable disables)

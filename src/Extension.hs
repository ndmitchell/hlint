module Extension(
  defaultExtensions,
  configExtensions,
  toHseEnabledExtensions,
  extensionImpliedEnabledBy,
  extensionImplies
  ) where

import Text.Read
import Data.List.Extra
import qualified Data.Map as Map
import Data.Maybe
import qualified Language.Haskell.Exts.Extension as HSE
import GHC.LanguageExtensions.Type
import qualified Language.Haskell.GhclibParserEx.DynFlags as GhclibParserEx

badExtensions =
  reallyBadExtensions ++
  [ Arrows -- steals proc
  , UnboxedTuples, UnboxedSums -- breaks (#) lens operator
  , QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
  , {- DoRec , -} RecursiveDo -- breaks rec
  , TypeApplications -- HSE fails on @ patterns
  ]

reallyBadExtensions =
  [ TransformListComp -- steals the group keyword
  {- , XmlSyntax , RegularPatterns -} -- steals a-b and < operators
  ]

-- | Extensions we turn on by default when parsing. Aim to parse as
-- many files as we can.
defaultExtensions :: [Extension]
defaultExtensions = enumerate \\ badExtensions

-- | Extensions we turn on when reading config files, don't have to deal with the whole world
--   of variations - in particular, we might require spaces in some places.
configExtensions :: [Extension]
configExtensions = enumerate \\ reallyBadExtensions

-- Note that this silenty ignores extensions that HSE doesn't
-- know. This function won't be here for much longer so shouldn't be a
-- problem.
toHseEnabledExtensions :: [Extension] -> [HSE.Extension]
toHseEnabledExtensions = mapMaybe ((HSE.EnableExtension <$>) . readMaybe . show')
  where
    show' e = if ex == "Cpp" then "CPP" else ex where ex = show e

-- | This extension implies the following extensions are
-- enabled/disabled.
extensionImplies :: Extension -> ([Extension], [Extension])
extensionImplies = \x ->Map.findWithDefault ([], []) x mp
  where mp = Map.fromList extensionImplications

-- 'x' is implied enabled by the result extensions.
extensionImpliedEnabledBy :: Extension -> [Extension]
extensionImpliedEnabledBy = \x -> Map.findWithDefault [] x mp
  where
    mp = Map.fromListWith (++) [(b, [a]) | (a, (bs, _)) <- extensionImplications, b <- bs]

-- 'x' is implied disabled by the result extensions. Not called at this time.
_extensionImpliedDisabledBy :: Extension -> [Extension]
_extensionImpliedDisabledBy = \x -> Map.findWithDefault [] x mp
  where
    mp = Map.fromListWith (++) [(b, [a]) | (a, (_, bs)) <- extensionImplications, b <- bs]

-- | (a, bs) means extension a implies all of bs. Uses GHC source at
-- DynFlags.impliedXFlags
extensionImplications :: [(Extension, ([Extension], [Extension]))]
extensionImplications = GhclibParserEx.extensionImplications

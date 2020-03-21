module Extension(
  defaultExtensions,
  configExtensions,
  extensionImpliedEnabledBy,
  extensionImplies
  ) where

import Data.List.Extra
import qualified Data.Map as Map
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
  , AlternativeLayoutRule -- Does not play well with 'MultiWayIf'
  , NegativeLiterals -- Was not enabled by HSE and enabling breaks tests.
  ]

-- | Extensions we turn on by default when parsing. Aim to parse as
-- many files as we can.
defaultExtensions :: [Extension]
defaultExtensions = enumerate \\ badExtensions

-- | Extensions we turn on when reading config files, don't have to deal with the whole world
--   of variations - in particular, we might require spaces in some places.
configExtensions :: [Extension]
configExtensions = enumerate \\ reallyBadExtensions

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

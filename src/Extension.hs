module Extension(
  defaultExtensions,
  configExtensions,
  extensionImpliedEnabledBy,
  extensionImplies
  ) where

import Data.List.Extra
import qualified Data.Map as Map
import GHC.LanguageExtensions.Type
import qualified Language.Haskell.GhclibParserEx.GHC.Driver.Session as GhclibParserEx

badExtensions =
  reallyBadExtensions ++
  [ Arrows -- steals proc
  , UnboxedTuples, UnboxedSums -- breaks (#) lens operator
  , QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
  , {- DoRec , -} RecursiveDo -- breaks rec
  , LexicalNegation -- changes '-', see https://github.com/ndmitchell/hlint/issues/1230
  -- These next two change syntax significantly and must be opt-in.
  , OverloadedRecordDot
  , OverloadedRecordUpdate
  ]

reallyBadExtensions =
  [ TransformListComp -- steals the group keyword
  , StaticPointers -- steals the static keyword
  {- , XmlSyntax , RegularPatterns -} -- steals a-b and < operators
  , AlternativeLayoutRule -- Does not play well with 'MultiWayIf'
  , NegativeLiterals -- Was not enabled by HSE and enabling breaks tests.
  , StarIsType -- conflicts with TypeOperators. StarIsType is currently enabled by default,
               -- so adding it here has no effect, but it may not be the case in future GHC releases.
  , MonadComprehensions -- Discussed in https://github.com/ndmitchell/hlint/issues/1261
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

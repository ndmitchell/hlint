module Extension(
  defaultExtensions,
  configExtensions,
  toHseEnabledExtensions
  ) where

import Text.Read
import Data.List.Extra
import Data.Maybe
import qualified Language.Haskell.Exts.Extension as HSE
import GHC.LanguageExtensions.Type

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

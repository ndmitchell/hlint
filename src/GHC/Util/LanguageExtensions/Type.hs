
module GHC.Util.LanguageExtensions.Type (
    readExtension
) where

import GHC.LanguageExtensions.Type

import qualified Data.Map.Strict as Map

-- | Parse a GHC extension
readExtension :: String -> Maybe Extension
readExtension x = Map.lookup x exts
  where exts = Map.fromList [(show x, x) | x <- [Cpp .. StarIsType]]

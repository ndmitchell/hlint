module GHC.Util.Name (isSymbolRdrName) where

import OccName
import RdrName

isSymbolRdrName :: RdrName -> Bool
isSymbolRdrName = isSymOcc . rdrNameOcc

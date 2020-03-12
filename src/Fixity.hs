
module Fixity(
    FixityInfo, Associativity(..),
    toHseFixity,
    fromFixitySig, toFixitySig
    ) where

import GHC.Generics(Associativity(..))
import qualified Language.Haskell.Exts as HSE
import HsBinds
import HsExtension
import OccName
import RdrName
import SrcLoc
import BasicTypes

-- Lots of things define a fixity. None define it quite right, so let's have our own type.
type FixityInfo = (String, Associativity, Int)


toHseFixity :: FixityInfo -> HSE.Fixity
toHseFixity = undefined


fromFixitySig :: FixitySig GhcPs -> [FixityInfo]
fromFixitySig (FixitySig _ names (Fixity _ i dir)) =
    [(occNameString $ occName $ unLoc name, f dir, i) | name <- names]
    where
        f InfixL = LeftAssociative
        f InfixR = RightAssociative
        f InfixN = NotAssociative
fromFixitySig _ = []


toFixitySig :: FixityInfo -> FixitySig GhcPs
toFixitySig (name, dir, i) = FixitySig NoExt [noLoc $ Unqual $ mkVarOcc name] $ Fixity NoSourceText i $ f dir
    where
        f LeftAssociative = InfixL
        f RightAssociative = InfixR
        f NotAssociative = InfixN

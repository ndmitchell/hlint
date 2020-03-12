{-# LANGUAGE ViewPatterns #-}

module Fixity(
    FixityInfo, Associativity(..),
    toHseFixity, fromHseFixity,
    fromFixitySig, toFixitySig, toFixity,
    ) where

import GHC.Generics(Associativity(..))
import qualified Language.Haskell.Exts as HSE (Fixity(..), QName(..), Name(..), Assoc(..), SpecialCon(..))
import HsBinds
import HsExtension
import OccName
import RdrName
import SrcLoc
import BasicTypes

-- Lots of things define a fixity. None define it quite right, so let's have our own type.

-- | A Fixity definition, comprising the name the fixity applies to,
--   the direction and the precedence. As an example:
--
-- > infixr 3 `foo`
--
--   Would create @(\"foo\", RightAssociative, 3)
type FixityInfo = (String, Associativity, Int)

fromHseFixity :: HSE.Fixity -> FixityInfo
fromHseFixity (HSE.Fixity dir i name) = (g name, f dir, i)
    where
        f HSE.AssocLeft{} = LeftAssociative
        f HSE.AssocRight{} = RightAssociative
        f HSE.AssocNone{} = NotAssociative

        g :: HSE.QName () -> String
        g (HSE.Special _ HSE.Cons{}) = ":"
        g (HSE.Special _ HSE.UnitCon{}) = "()"
        g (HSE.UnQual _ (HSE.Ident _ x)) = x
        g (HSE.UnQual _ (HSE.Symbol _ x)) = x
        g _ = ""

toHseFixity :: FixityInfo -> HSE.Fixity
toHseFixity (name, dir, i) = HSE.Fixity (f dir) i $ HSE.UnQual () $ HSE.Ident () name
    where
        f LeftAssociative = HSE.AssocLeft ()
        f RightAssociative = HSE.AssocRight ()
        f NotAssociative = HSE.AssocNone ()

fromFixitySig :: FixitySig GhcPs -> [FixityInfo]
fromFixitySig (FixitySig _ names (Fixity _ i dir)) =
    [(occNameString $ occName $ unLoc name, f dir, i) | name <- names]
    where
        f InfixL = LeftAssociative
        f InfixR = RightAssociative
        f InfixN = NotAssociative
fromFixitySig _ = []

toFixity :: FixityInfo -> (String, Fixity)
toFixity (name, dir, i) = (name, Fixity NoSourceText i $ f dir)
    where
        f LeftAssociative = InfixL
        f RightAssociative = InfixR
        f NotAssociative = InfixN

toFixitySig :: FixityInfo -> FixitySig GhcPs
toFixitySig (toFixity -> (name, x)) = FixitySig NoExt [noLoc $ Unqual $ mkVarOcc name] x

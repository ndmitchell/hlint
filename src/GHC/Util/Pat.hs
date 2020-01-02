{-# LANGUAGE MultiParamTypeClasses , FlexibleInstances, FlexibleContexts #-}

module GHC.Util.Pat (
    strToPat', patToStr'
  , Brackets'(..)
  , fromPChar', isPFieldWildcard', hasPFieldsDotDot', isPWildCard'
  , isPFieldPun', isPatTypeSig', isPBangPat', isPViewPat'
  ) where

import HsSyn
import SrcLoc
import TysWiredIn
import FastString
import RdrName

import GHC.Util.Brackets

patToStr' :: Pat GhcPs -> String
patToStr' (LL _ (ConPatIn (L _ x) (PrefixCon []))) | x == true_RDR = "True"
patToStr' (LL _ (ConPatIn (L _ x) (PrefixCon []))) | x == false_RDR = "False"
patToStr' (LL _ (ConPatIn (L _ x) (PrefixCon []))) | x == nameRdrName nilDataConName = "[]"
patToStr' _ = ""

strToPat' :: String -> Pat GhcPs
strToPat' z
  | z == "True"  = ConPatIn (noLoc true_RDR) (PrefixCon [])
  | z == "False" = ConPatIn (noLoc false_RDR) (PrefixCon [])
  | z == "[]"    = ConPatIn (noLoc $ nameRdrName nilDataConName) (PrefixCon [])
  | otherwise    = VarPat noExt (noLoc $ mkVarUnqual (fsLit z))

fromPChar' :: Pat GhcPs -> Maybe Char
fromPChar' (LL _ (LitPat _ (HsChar _ x))) = Just x
fromPChar' _ = Nothing

-- Contains a '..' as in 'Foo{..}'
hasPFieldsDotDot' :: HsRecFields GhcPs (Pat GhcPs) -> Bool
hasPFieldsDotDot' HsRecFields {rec_dotdot=Just _} = True
hasPFieldsDotDot' _ = False -- {-# COMPLETE LL #-}

-- Field has a '_' as in '{foo=_} or is punned e.g. '{foo}'.
isPFieldWildcard' :: LHsRecField GhcPs (Pat GhcPs) -> Bool
isPFieldWildcard' (LL _ HsRecField {hsRecFieldArg=(LL _ (WildPat _))}) = True
isPFieldWildcard' (LL _ HsRecField {hsRecPun=True}) = True
isPFieldWildcard' (LL _ HsRecField {}) = False
isPFieldWildcard' _ = False -- {-# COMPLETE LL #-}

isPWildCard' :: Pat GhcPs -> Bool
isPWildCard' (LL _ (WildPat _)) = True
isPWildCard' _ = False

isPFieldPun' :: LHsRecField GhcPs (Pat GhcPs) -> Bool
isPFieldPun' (LL _ HsRecField {hsRecPun=True}) = True
isPFieldPun' _ = False

isPatTypeSig', isPBangPat', isPViewPat' :: Pat GhcPs -> Bool
isPatTypeSig' (LL _ SigPat{}) = True; isPatTypeSig' _ = False
isPBangPat' (LL _ BangPat{}) = True; isPBangPat' _ = False
isPViewPat' (LL _ ViewPat{}) = True; isPViewPat' _ = False

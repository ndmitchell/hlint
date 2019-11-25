module GHC.Util.RdrName (isSpecial', unqual', rdrNameStr',fromQual') where

import SrcLoc
import Name
import RdrName

rdrNameStr' :: Located RdrName -> String
rdrNameStr' = occNameString . rdrNameOcc . unLoc

-- Builtin type or data constructors.
isSpecial' :: Located RdrName -> Bool
isSpecial' (L _ (Exact n)) = isDataConName n || isTyConName n
isSpecial' _ = False

-- Coerce qualified names to unqualified (by discarding the
-- qualifier).
unqual' :: Located RdrName -> Located RdrName
unqual' (L loc (Qual _ n)) = cL loc $ mkRdrUnqual n
unqual' x = x

fromQual' :: Located RdrName -> Maybe OccName
fromQual' (L _ (Qual _ x)) = Just x
fromQual' (L _ (Unqual x)) = Just x
fromQual' _ = Nothing

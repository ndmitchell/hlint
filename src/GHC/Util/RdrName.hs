module GHC.Util.RdrName (isSpecial, unqual, occNameStr, rdrNameStr,fromQual) where

import SrcLoc
import Name
import RdrName

-- These names may not seem natural here but they work out in
-- practice. The use of thse two functions is thoroughly ubiquitous.
occNameStr :: RdrName -> String; occNameStr = occNameString . rdrNameOcc
rdrNameStr :: Located RdrName -> String; rdrNameStr = occNameStr . unLoc

-- Builtin type or data constructors.
isSpecial :: Located RdrName -> Bool
isSpecial (L _ (Exact n)) = isDataConName n || isTyConName n
isSpecial _ = False

-- Coerce qualified names to unqualified (by discarding the
-- qualifier).
unqual :: Located RdrName -> Located RdrName
unqual (L loc (Qual _ n)) = cL loc $ mkRdrUnqual n
unqual x = x

fromQual :: Located RdrName -> Maybe OccName
fromQual (L _ (Qual _ x)) = Just x
fromQual (L _ (Unqual x)) = Just x
fromQual _ = Nothing

{-# LANGUAGE FlexibleInstances #-}

module HSE.Match(
    Named(fromNamed),
    isSym
    ) where

import Data.Char
import Language.Haskell.Exts

-- | fromNamed will return \"\" when it cannot be represented
--   toNamed may crash on \"\"
class Named a where
    fromNamed :: a -> String


isSym (x:_) = not $ isAlpha x || x `elem` "_'"
isSym _ = False

instance Named (QName SrcSpanInfo) where
    fromNamed (Special _ Cons{}) = ":"
    fromNamed (Special _ UnitCon{}) = "()"
    fromNamed (UnQual _ x) = fromNamed x
    fromNamed _ = ""

instance Named (Name SrcSpanInfo) where
    fromNamed (Ident _ x) = x
    fromNamed (Symbol _ x) = x

instance Named (QOp SrcSpanInfo) where
    fromNamed (QVarOp _ x) = fromNamed x
    fromNamed (QConOp _ x) = fromNamed x

{-# LANGUAGE FlexibleInstances #-}

module HSE.Match(
    Named(fromNamed),
    isSym
    ) where

import Data.Char
import HSE.Type

-- | fromNamed will return \"\" when it cannot be represented
--   toNamed may crash on \"\"
class Named a where
    fromNamed :: a -> String


isSym (x:_) = not $ isAlpha x || x `elem` "_'"
isSym _ = False

instance Named (QName S) where
    fromNamed (Special _ Cons{}) = ":"
    fromNamed (Special _ UnitCon{}) = "()"
    fromNamed (UnQual _ x) = fromNamed x
    fromNamed _ = ""

instance Named (Name S) where
    fromNamed (Ident _ x) = x
    fromNamed (Symbol _ x) = x

instance Named (QOp S) where
    fromNamed (QVarOp _ x) = fromNamed x
    fromNamed (QConOp _ x) = fromNamed x

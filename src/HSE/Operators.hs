{-# LANGUAGE PatternGuards, ViewPatterns, MultiParamTypeClasses #-}

module HSE.Operators where

import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Language.Haskell.Exts
import HSE.Match
import HSE.Util



-- "f $ g $ x" is parsed as "(f $ g) $ x", but should be "f $ (g $ x)"
-- ditto for (.)
-- this function rotates the ($) and (.), provided there are no explicit Parens
operatorPrec :: Module -> Module
operatorPrec = descendBi (transform f)
    where
        f (InfixApp (InfixApp x op2 y) op1 z) 
            | op <- opExp op1, op1 == op2, op ~= "." || op ~= "$" = f $ InfixApp x op1 (f $ InfixApp y op1 z)
        f x = x

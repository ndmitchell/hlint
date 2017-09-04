{-# LANGUAGE ViewPatterns #-}

-- Evaluate/reduce a HSE Exp as much as possible
module HSE.Reduce(reduce) where

import HSE.Match
import HSE.Util
import HSE.Type
import Language.Haskell.Exts.Util


reduce :: Exp_ -> Exp_
reduce = fromParen . transform reduce1


reduce1 :: Exp_ -> Exp_
reduce1 (App s len (Lit _ (String _ xs _))) | len ~= "length" = Lit s $ Int s n (show n)
    where n = fromIntegral $ length xs
reduce1 (App s len (List _ xs)) | len ~= "length" = Lit s $ Int s n (show n)
    where n = fromIntegral $ length xs
reduce1 (view -> App2 op (Lit _ x) (Lit _ y)) | op ~= "==" = toNamed $ show $ x =~= y
reduce1 (view -> App2 op (Lit _ (Int _ x _)) (Lit _ (Int _ y _)))
    | op ~= ">=" = toNamed $ show $ x >= y
reduce1 (view -> App2 op x y)
    | op ~= "&&" && x ~= "True"  = y
    | op ~= "&&" && x ~= "False" = x
reduce1 (Paren _ x) | isAtom x = x
reduce1 x = x

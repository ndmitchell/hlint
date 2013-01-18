{-# LANGUAGE ViewPatterns #-}

-- Evaluate a HSE Exp as much as possible
module HSE.Evaluate(evaluate) where

import HSE.Match
import HSE.Util
import HSE.Type
import HSE.Bracket


evaluate :: Exp_ -> Exp_
evaluate = fromParen . transform evaluate1


evaluate1 :: Exp_ -> Exp_
evaluate1 (App s len (Lit _ (String _ xs _))) | len ~= "length" = Lit s $ Int s n (show n)
    where n = fromIntegral $ length xs
evaluate1 (App s len (List _ xs)) | len ~= "length" = Lit s $ Int s n (show n)
    where n = fromIntegral $ length xs
evaluate1 (view -> App2 op (Lit _ x) (Lit _ y)) | op ~= "==" = toNamed $ show $ x =~= y
evaluate1 (view -> App2 op (Lit _ (Int _ x _)) (Lit _ (Int _ y _)))
    | op ~= ">=" = toNamed $ show $ x >= y
evaluate1 (view -> App2 op x y)
    | op ~= "&&" && x ~= "True"  = y
    | op ~= "&&" && x ~= "False" = x
evaluate1 (Paren _ x) | isAtom x = x
evaluate1 x = x

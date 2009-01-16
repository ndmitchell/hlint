{-# LANGUAGE ViewPatterns #-}

-- Evaluate a HSE Exp as much as possible
module HSE.Evaluate(evaluate) where

import Language.Haskell.Exts
import Data.Generics.PlateData
import HSE.Match
import HSE.Util
import HSE.Bracket


evaluate :: Exp -> Exp
evaluate = fromParen . transform evaluate1


evaluate1 :: Exp -> Exp
evaluate1 (App len (Lit (String xs))) | len ~= "length" = Lit $ Int $ fromIntegral $ length xs
evaluate1 (App len (List xs)) | len ~= "length" = Lit $ Int $ fromIntegral $ length xs
evaluate1 (view -> App2 op (Lit x) (Lit y)) | op ~= "==" = toNamed $ show $ x == y
evaluate1 (view -> App2 op x y)
    | op ~= "&&" && x ~= "True"  = y
    | op ~= "&&" && x ~= "False" = x
evaluate1 (Paren x) | isAtom x = x
evaluate1 x = x

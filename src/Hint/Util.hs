{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Hint.Util where

import HSE.All
import Util


-- | Generate a lambda, but prettier (if possible).
--   Generally no lambda is good, but removing just some arguments isn't so useful.
niceLambda :: [String] -> Exp_ -> Exp_

-- \xs -> (e) ==> \xs -> e
niceLambda xs (Paren _ x) = niceLambda xs x

-- \xs -> \v vs -> e ==> \xs v -> \vs -> e
-- \xs -> \ -> e ==> \xs -> e
niceLambda xs (Lambda _ ((view -> PVar_ v):vs) x) | v `notElem` xs = niceLambda (xs++[v]) (Lambda an vs x)
niceLambda xs (Lambda _ [] x) = niceLambda xs x

-- \ -> e ==> e
niceLambda [] x = x

-- \xs -> e xs ==> e
niceLambda xs (fromApps -> e) | map view xs2 == map Var_ xs, vars e2 `disjoint` xs, notNull e2 = apps e2
    where (e2,xs2) = splitAt (length e - length xs) e

-- \x y -> x + y ==> (+)
niceLambda [x,y] (InfixApp _ (view -> Var_ x1) (opExp -> op) (view -> Var_ y1))
    | x == x1, y == y1, vars op `disjoint` [x,y] = op

-- \x -> x + b ==> (+ b) [heuristic, b must be a single lexeme, or gets too complex]
niceLambda [x] (view -> App2 (expOp -> Just op) a b)
    | isLexeme b, view a == Var_ x, x `notElem` vars b, allowRightSection (fromNamed op) = rebracket1 $ RightSection an op b

-- \x y -> f y x = flip f
niceLambda [x,y] (view -> App2 op (view -> Var_ y1) (view -> Var_ x1))
    | x == x1, y == y1, vars op `disjoint` [x,y] = App an (toNamed "flip") op

-- \x -> f (b x) ==> f . b
-- \x -> f $ b x ==> f . b
niceLambda [x] y | Just z <- factor y, x `notElem` vars z = z
    where
        -- factor the expression with respect to x
        factor y@App{} | (ini,lst) <- unsnoc $ fromApps y, view lst == Var_ x = Just $ apps ini
        factor y@App{} | (ini,lst) <- unsnoc $ fromApps y, Just z <- factor lst = Just $ niceDotApp (apps ini) z
        factor (InfixApp _ y op (factor -> Just z)) | isDol op = Just $ niceDotApp y z
        factor (Paren _ y@App{}) = factor y
        factor _ = Nothing

-- \x -> (x +) ==> (+)
niceLambda [x] (LeftSection _ (view -> Var_ x1) op) | x == x1 = opExp op

-- base case
niceLambda ps x = Lambda an (map toNamed ps) x



-- ($) . b ==> b
niceDotApp :: Exp_ -> Exp_ -> Exp_
niceDotApp a b | a ~= "$" = b
               | otherwise = dotApp a b



-- | Convert expressions which have redundant junk in them away.
--   Mainly so that later stages can match on fewer alternatives.
simplifyExp :: Exp_ -> Exp_
simplifyExp (InfixApp _ x dol y) | isDol dol = App an x (paren y)
simplifyExp (Let _ (BDecls _ [PatBind _ (view -> PVar_ x) Nothing (UnGuardedRhs _ y) Nothing]) z)
    | x `notElem` vars y && x `notElem` pvars z && length (filter (== x) (vars z)) <= 1 = transform f z
    where f (view -> Var_ x') | x == x' = paren y
          f x = x
simplifyExp x = x

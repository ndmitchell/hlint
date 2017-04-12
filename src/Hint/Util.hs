{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Hint.Util(niceLambda, simplifyExp, niceLambdaR) where

import HSE.All
import Data.List.Extra
import Refact.Types
import Refact
import qualified Refact.Types as R (SrcSpan)

niceLambda :: [String] -> Exp_ -> Exp_
niceLambda ss e = fst (niceLambdaR ss e)


-- | Generate a lambda, but prettier (if possible).
--   Generally no lambda is good, but removing just some arguments isn't so useful.
niceLambdaR :: [String] -> Exp_ -> (Exp_, R.SrcSpan -> [Refactoring R.SrcSpan])

-- \xs -> (e) ==> \xs -> e
niceLambdaR xs (Paren l x) = niceLambdaR xs x

-- \xs -> \v vs -> e ==> \xs v -> \vs -> e
-- \xs -> \ -> e ==> \xs -> e
niceLambdaR xs (Lambda _ ((view -> PVar_ v):vs) x) | v `notElem` xs = niceLambdaR (xs++[v]) (Lambda an vs x)
niceLambdaR xs (Lambda _ [] x) = niceLambdaR xs x

-- \ -> e ==> e
niceLambdaR [] x = (x, const [])

-- \vs v -> e $ v ==> \vs -> e
niceLambdaR (unsnoc -> Just (vs, v)) (InfixApp _ e (isDol -> True) (view -> Var_ v2))
    | v == v2, vars e `disjoint` [v]
    = niceLambdaR vs e

-- \xs -> e xs ==> e
niceLambdaR xs (fromAppsWithLoc -> e) | map view xs2 == map Var_ xs, vars e2 `disjoint` xs, not $ null e2 =
    (apps e2, \s -> [Replace Expr s [("x", pos)] "x"])
    where (e',xs') = splitAt (length e - length xs) e
          (e2, xs2) = (map fst e', map fst xs')
          pos      = toRefactSrcSpan . srcInfoSpan $ snd (last e')

-- \x y -> x + y ==> (+)
niceLambdaR [x,y] (InfixApp _ (view -> Var_ x1) (opExp -> op) (view -> Var_ y1))
    | x == x1, y == y1, vars op `disjoint` [x,y] = (op, \s -> [Replace Expr s [] (prettyPrint op)])

-- \x -> x + b ==> (+ b) [heuristic, b must be a single lexeme, or gets too complex]
niceLambdaR [x] (view -> App2 (expOp -> Just op) a b)
    | isLexeme b, view a == Var_ x, x `notElem` vars b, allowRightSection (fromNamed op) =
      let e = rebracket1 $ RightSection an op b
      in (e, \s -> [Replace Expr s [] (prettyPrint e)])

-- \x y -> f y x = flip f
niceLambdaR [x,y] (view -> App2 op (view -> Var_ y1) (view -> Var_ x1))
    | x == x1, y == y1, vars op `disjoint` [x,y] = (gen op, \s -> [Replace Expr s [("x", toSS op)] (prettyPrint $ gen (toNamed "x"))])
    where
      gen = App an (toNamed "flip")

-- \x -> f (b x) ==> f . b
-- \x -> f $ b x ==> f . b
niceLambdaR [x] y | Just (z, subts) <- factor y, x `notElem` vars z = (z, \s -> [mkRefact subts s])
    where
        -- factor the expression with respect to x
        factor y@(App _ ini lst) | view lst == Var_ x = Just (ini, [ann ini])
        factor y@(App _ ini lst) | Just (z, ss) <- factor lst = let r = niceDotApp ini z
                                                           in if r == z then Just (r, ss)
                                                                        else Just (r, ann ini : ss)
        factor (InfixApp _ y op (factor -> Just (z, ss))) | isDol op = let r = niceDotApp y z
                                                                 in if r == z then Just (r, ss)
                                                                              else Just (r, ann y : ss)
        factor (Paren _ y@App{}) = factor y
        factor _ = Nothing
        mkRefact :: [S] -> R.SrcSpan -> Refactoring R.SrcSpan
        mkRefact subts s =
          let tempSubts = zipWith (\a b -> ([a], toRefactSrcSpan $ srcInfoSpan b)) ['a' .. 'z'] subts
              template = dotApps (map (toNamed . fst) tempSubts)
          in Replace Expr s tempSubts (prettyPrint template)


-- \x -> (x +) ==> (+)
-- Section handling is not yet supported for refactoring
niceLambdaR [x] (LeftSection _ (view -> Var_ x1) op) | x == x1 =
  let e = opExp op
  in (e, \s -> [Replace Expr s [] (prettyPrint e)])

-- base case
niceLambdaR ps x = (Lambda an (map toNamed ps) x, const [])



-- ($) . b ==> b
niceDotApp :: Exp_ -> Exp_ -> Exp_
niceDotApp a b | a ~= "$" = b
               | otherwise = dotApp a b



-- | Convert expressions which have redundant junk in them away.
--   Mainly so that later stages can match on fewer alternatives.
simplifyExp :: Exp_ -> Exp_
simplifyExp (InfixApp _ x dol y) | isDol dol = App an x (paren y)
simplifyExp (Let _ (BDecls _ [PatBind _ (view -> PVar_ x) (UnGuardedRhs _ y) Nothing]) z)
    | x `notElem` vars y && length [() | UnQual _ a <- universeS z, prettyPrint a == x] <= 1 = transform f z
    where f (view -> Var_ x') | x == x' = paren y
          f x = x
simplifyExp x = x

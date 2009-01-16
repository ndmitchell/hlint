{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Find and match:

    foo a = \x -> y (provided no where's outside the lambda)
    \x -> y (use const, if the variable x does not occur in y, and it doesn't need bracketing)
    foo x = y x (eta reduce)
    foo x = f $ g x (eta reduce, convert to .)
    foo x y = f (g x) (g y) ==> f `on` g
    -- Never offer to eta reduce if the variable is named mr and is the only one (Monomorphism Restriction)
    -- don't eta reduce func a b c = .... (g b) (g c) to (g b) . g, looks ugly

<TEST>
yes = 0 where f a = \x -> x + x ; res = "f a x = x + x"
yes a = foo (\x -> True) where res = const True
no = foo (\x -> map f [])
yes = 0 where f x = y x ; res = "f = y"
no mr = y mr
yes = 0 where f x = g $ f $ map head x ; res = "f = g . f . map head"
no z x y = f (g x) (g y)
no = 0 where f x y = f (g x) (g y) ; res = "f = f `on` g"
yes = 0 where f x y = g x == g y ; res = "f = (==) `on` g"
</TEST>
-}


module Hint.Lambda where

import HSE.All
import Type
import Control.Monad
import Data.Generics.PlateData
import Data.Maybe


lambdaHint :: Hint
lambdaHint x = concatMap lambdaExp (universeBi x) ++ concatMap lambdaDecl (universe x)


lambdaExp :: Exp -> [Idea]
lambdaExp o@(Lambda loc [v] y) | isAtom y, Just x <- f v, x `notElem` universeBi y =
        [warn "Use const" loc o res]
    where
        f (PVar x) = Just x
        f PWildCard = Just $ Ident "_"
        f _ = Nothing
        res = App (toNamed "const") y
lambdaExp _ = []


lambdaDecl :: Decl -> [Idea]
lambdaDecl (PatBind loc (PVar x) typ rhs bind) = lambdaDef $ Match loc x [] typ rhs bind
lambdaDecl (FunBind [x]) = lambdaDef x --only apply to 1-def, because arities must be the same
lambdaDecl _ = []


lambdaDef :: Match -> [Idea]
lambdaDef o@(Match loc name pats typ (UnGuardedRhs bod) (BDecls []))
    | Lambda loc vs y <- bod = [warn "Redundant lambda" loc o $ reform (pats++vs) y]
    | [PVar x, PVar y] <- pats, Just (f,g) <- useOn x y bod =
              [warn "Use on" loc o $ reform [] (ensureBracket1 $ InfixApp f (QVarOp $ UnQual $ Ident "on") g)]
    | Ident _ <- name, (p2,y) <- etaReduces pats bod, length p2 /= length pats = [warn "Eta reduce" loc o $ reform p2 y]
    | otherwise = []
        where reform pats2 bod2 = Match loc name pats2 typ (UnGuardedRhs bod2) (BDecls [])
lambdaDef _ = []


-- given x y, f (g x) (g y) = Just (f, g)
useOn :: Name -> Name -> Exp -> Maybe (Exp, Exp)
useOn x1 y1 (view -> App2 (Var (UnQual f)) (view -> App1 g1 x2) (view -> App1 g2 y2))
    | fromName f `elem` ["==",">=",">","!=","<","<="] && g1 == g2, map (Var . UnQual) [x1,y1] == [x2,y2]
    = Just (Var (UnQual f),g1)
useOn _ _ _ = Nothing


etaReduces :: [Pat] -> Exp -> ([Pat], Exp)
etaReduces ps x | ps /= [], PVar p <- last ps, p /= Ident "mr", Just y <- etaReduce p x = etaReduces (init ps) y
                | otherwise = (ps,x)


etaReduce :: Name -> Exp -> Maybe Exp
etaReduce x (App y (Var (UnQual z))) | x == z && x `notElem` universeBi y = Just y
etaReduce x (App y z) | not (uglyEta y z) && x `notElem` universeBi y = do
    z2 <- etaReduce x z
    return $ InfixApp y (QVarOp $ UnQual $ Symbol ".") z2
etaReduce x (view -> App2 dollar y z) | dollar ~= "$" = etaReduce x (App y z)
etaReduce x y | isParen y = etaReduce x (fromParen y)
etaReduce x _ = Nothing


-- (f (g x)) (h y), ugly if g == h
uglyEta :: Exp -> Exp -> Bool
uglyEta (fromParen -> App f (fromParen -> App g x)) (fromParen -> App h y) = g == h
uglyEta _ _ = False


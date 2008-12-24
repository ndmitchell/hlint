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
yes1 a = \x -> x + x
yes2 a = foo (\x -> True) -- const True
no1 = foo (\x -> map f [])
yes3 x = y x -- eta reduce
no2 mr = y mr
yes4 x = g $ f $ map head x
no3 z x y = f (g x) (g y)
yes5 x y = f (g x) (g y)
</TEST>
-}


module Hint.Lambda where

import HSE.Util
import Type
import Control.Monad
import Language.Haskell.Exts
import Data.Generics.PlateData
import Data.Maybe


lambdaHint :: Hint
lambdaHint x = concatMap lambdaExp (universeBi x) ++ concatMap lambdaDecl (universe x)


lambdaExp :: Exp -> [Idea]
lambdaExp o@(Lambda loc [v] y) | atom y, Just x <- f v, x `notElem` universeBi y =
        [idea "Use const" loc o res]
    where
        f (PVar x) = Just x
        f PWildCard = Just $ Ident "_"
        f _ = Nothing
        res = App (toVar "const") (hsParen y)
lambdaExp _ = []


lambdaDecl :: Decl -> [Idea]
lambdaDecl (PatBind loc (PVar x) rhs bind) = lambdaDef $ Match loc x [] rhs bind
lambdaDecl (FunBind [x]) = lambdaDef x --only apply to 1-def, because arities must be the same
lambdaDecl _ = []


lambdaDef :: Match -> [Idea]
lambdaDef o@(Match loc name pats (UnGuardedRhs bod) (BDecls []))
    | Lambda loc vs y <- bod = [idea "Lambda shift" loc o $ reform (pats++vs) y]
    | pats /= [], PVar p <- last pats, Ident _ <- name, p /= Ident "mr", Just y <- etaReduce p bod =
              [idea "Eta reduce" loc o $ reform (init pats) y]
    | [PVar x, PVar y] <- pats, Just (f,g) <- useOn x y bod =
              [idea "Use on" loc o $ reform [] (remParen $ InfixApp (addParen f) (QVarOp $ UnQual $ Ident "on") (addParen g))]
        where reform pats2 bod2 = Match loc name pats2 (UnGuardedRhs bod2) (BDecls [])
lambdaDef _ = []


-- given x y, f (g x) (g y) = Just (f, g)
useOn :: Name -> Name -> Exp -> Maybe (Exp, Exp)
useOn x1 y1 (view -> App2 f (view -> App1 g1 x2) (view -> App1 g2 y2))
    | g1 == g2, map (Var . UnQual) [x1,y1] == [x2,y2] = Just (f,g1)
useOn _ _ _ = Nothing


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


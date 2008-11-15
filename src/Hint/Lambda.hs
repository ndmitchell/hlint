{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Find and match:

    foo a = \x -> y (provided no where's outside the lambda)
    \x -> y (use const, if the variable x does not occur in y)
    foo x = y x (eta reduce)
    foo x = f $ g x (eta reduce, convert to .)
    -- Never offer to eta reduce if the variable is named mr and is the only one (Monomorphism Restriction)
-}


module Hint.Lambda where

import Util
import Type
import Language.Haskell.Exts
import Data.Generics.PlateData


lambdaHint :: Hint
lambdaHint x = concatMap lambdaExp (universeBi x) ++ concatMap lambdaDecl (universe x)


lambdaExp :: Exp -> [Idea]
lambdaExp o@(Lambda loc [v] y) | Just x <- f v, x `notElem` universeBi y =
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
    | pats /= [], PVar p <- last pats, Ident _ <- name, p /= Ident "mr", Just y <- etaReduce p (dollarRotate bod) =
              [idea "Eta reduce" loc o $ reform (init pats) y]
        where reform pats2 bod2 = Match loc name pats2 (UnGuardedRhs bod2) (BDecls [])
lambdaDef _ = []


etaReduce :: Name -> Exp -> Maybe Exp
etaReduce x (App y (Var (UnQual z))) | x == z && x `notElem` universeBi y = Just y
etaReduce x (App y z) | x `notElem` universeBi y = do
    z2 <- etaReduce x z
    return $ InfixApp y (QVarOp $ UnQual $ Symbol ".") z2
etaReduce x (view -> App2 dollar y z) | dollar ~= "$" = etaReduce x (App y z)
etaReduce x y | isParen y = etaReduce x (fromParen y)
etaReduce x _ = Nothing


-- "f $ g $ x" is parsed as "(f $ g) $ x", but should be "f $ (g $ x)"
-- this function rotates the dollars, provided there are no explicit Parens
dollarRotate :: Exp -> Exp
dollarRotate = foldr1 (\x y -> InfixApp x (QVarOp $ UnQual $ Symbol "$") y) . collect
    where
        collect (view -> App2 dollar x y) | dollar ~= "$" = collect x ++ [y]
        collect x = [x]



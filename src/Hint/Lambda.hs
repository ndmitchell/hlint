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

import Hint.Util
import Hint.Type
import Language.Haskell.Exts
import Data.Generics.PlateData


lambdaHint :: Hint
lambdaHint x = concatMap lambdaExp (universeBi x) ++ concatMap lambdaDecl (universe x)


lambdaExp :: HsExp -> [Idea]
lambdaExp o@(HsLambda loc [v] y) | Just x <- f v, x `notElem` universeBi y =
        [Idea "Use const" loc (Just $ prettyPrint o) (Just $ prettyPrint res)]
    where
        f (HsPVar x) = Just x
        f HsPWildCard = Just $ HsIdent "_"
        f _ = Nothing
        res = HsApp (toVar "const") (hsParen y)
lambdaExp _ = []


lambdaDecl :: HsDecl -> [Idea]
lambdaDecl (HsPatBind loc (HsPVar x) rhs bind) = lambdaDef $ HsMatch loc x [] rhs bind
lambdaDecl (HsFunBind [x]) = lambdaDef x --only apply to 1-def, because arities must be the same
lambdaDecl _ = []


lambdaDef :: HsMatch -> [Idea]
lambdaDef o@(HsMatch loc name pats (HsUnGuardedRhs bod) (HsBDecls []))
    | HsLambda loc vs y <- bod = [idea "Lambda shift" loc o $ reform (pats++vs) y]
    | pats /= [], HsPVar p <- last pats, p /= HsIdent "mr", Just y <- etaReduce p (dollarRotate bod) =
              [idea "Eta reduce" loc o $ reform (init pats) y]
        where reform pats2 bod2 = HsMatch loc name pats2 (HsUnGuardedRhs bod2) (HsBDecls [])
lambdaDef _ = []


etaReduce :: HsName -> HsExp -> Maybe HsExp
etaReduce x (HsApp y (HsVar (UnQual z))) | x == z && x `notElem` universeBi y = Just y
etaReduce x (view -> App2 dollar y z) | dollar ~= "$" && x `notElem` universeBi y = do
    z2 <- etaReduce x z
    return $ HsInfixApp y (HsQVarOp $ UnQual $ HsSymbol ".") z2
etaReduce x _ = Nothing


-- "f $ g $ x" is parsed as "(f $ g) $ x", but should be "f $ (g $ x)"
-- this function rotates the dollars, provided there are no explicit HsParens
dollarRotate :: HsExp -> HsExp
dollarRotate = foldr1 (\x y -> HsInfixApp x (HsQVarOp $ UnQual $ HsSymbol "$") y) . collect
    where
        collect (view -> App2 dollar x y) | dollar ~= "$" = collect x ++ [y]
        collect x = [x]



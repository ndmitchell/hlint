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
f a = \x -> x + x -- f a x = x + x
h a = f (g a ==) -- h = f . (==) . g
test a = foo (\x -> True) -- const True
test = foo (\x -> map f [])
test = 0 where f x = y x -- f = y
test mr = y mr
test = 0 where f x = g $ f $ map head x -- f = g . f . map head
test z x y = f (g x) (g y)
f x y = f (g x) (g y)
f x y = g x == g y -- f = (==) `on` g
a + b = foo a b -- (+) = foo
type Test a = Foo Char a
type Test a = Foo a Char a
type Test (a :: * -> *) = Foo Char a
yes = foo (\x -> sum x) -- sum
yes = foo (\x l -> sum x x l) -- \x -> sum x x
test = foo (\x -> y == x) -- (y ==)
test = foo (\x -> x == g y) -- (== g y)
test = foo (\x -> g x == x)
cp i = show (i - 1)
cp i = show (i + 1)
test c = 1 # 2 # c
</TEST>
-}


module Hint.Lambda where

import HSE.All
import Type
import Control.Monad
import Data.Generics.PlateData
import Data.Maybe


lambdaHint :: DeclHint
lambdaHint _ x@TypeDecl{} = lambdaType x
lambdaHint _ x = concatMap lambdaExp (universeBi x) ++ concatMap lambdaDecl (universe x)


lambdaExp :: Exp -> [Idea]
lambdaExp o@(Lambda loc [v] y) | isAtom y, Just x <- f v, x `notElem` universeBi y =
        [warn "Use const" loc o res]
    where
        f (PVar x) = Just x
        f PWildCard = Just $ Ident "_"
        f _ = Nothing
        res = App (toNamed "const") y
lambdaExp o@(Lambda loc vs x) | length vs /= length vs2 =
        [warn "Eta reduce" loc o $ if null vs2 then x2 else Lambda loc vs2 x2]
    where (vs2,x2) = etaReduces vs x
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
    | (p2,y) <- etaReduces pats bod, length p2 /= length pats = [warn "Eta reduce" loc o $ reform p2 y]
    | otherwise = []
        where reform pats2 bod2 = Match loc name pats2 typ (UnGuardedRhs bod2) (BDecls [])
lambdaDef _ = []


-- given x y, f (g x) (g y) = Just (f, g)
useOn :: Name -> Name -> Exp -> Maybe (Exp, Exp)
useOn x1 y1 (view -> App2 f (view -> App1 g1 x2) (view -> App1 g2 y2))
    | fromNamed f `elem` ["==",">=",">","!=","<","<="] && g1 == g2, map (Var . UnQual) [x1,y1] == [x2,y2]
    = Just (f,g1)
useOn _ _ _ = Nothing


etaReduces :: [Pat] -> Exp -> ([Pat], Exp)
etaReduces ps x | ps /= [], PVar p <- last ps, p /= Ident "mr", Just y <- etaReduce p x = etaReduces (init ps) y
                | otherwise = (ps,x)


etaReduce :: Name -> Exp -> Maybe Exp
etaReduce x (App y (Var (UnQual z))) | x == z && x `notElem` universeBi y = Just y
etaReduce x (InfixApp y op z) | f y z = Just $ RightSection op z
                              | f z y = Just $ LeftSection y op
    where f y z = op `notElem` map (QVarOp . UnQual . Symbol . return) "+-" &&
                  all (not . isInfixApp) [y,z] && var x == y && x `notElem` universeBi z
etaReduce x (App y z) | not (uglyEta y z) && x `notElem` universeBi y = do
    z2 <- etaReduce x z
    return $ InfixApp y (QVarOp $ UnQual $ Symbol ".") z2
etaReduce x (view -> App2 dollar y z) | dollar ~= "$" = etaReduce x (App y z)
etaReduce x (LeftSection y op) = etaReduce x $ App (opExp op) y
etaReduce x y | isParen y = etaReduce x (fromParen y)
etaReduce x y = Nothing


-- (f (g x)) (h y), ugly if g == h
uglyEta :: Exp -> Exp -> Bool
uglyEta (fromParen -> App f (fromParen -> App g x)) (fromParen -> App h y) = g == h
uglyEta _ _ = False



-- NOTE: Never perform lambda reduction on types
-- > type Foo a = Bar a
-- can be reduced for type Foo = Bar, only if Bar is
-- a data type, not a type alias - but we can't know
-- which it is
lambdaType :: Decl -> [Idea]
lambdaType o@(TypeDecl src name args typ) = []
{-
        [warn "Type eta reduce" src o t2 | i /= 0]
    where
        (i,t) = f (reverse args) typ
        t2 = TypeDecl src name (take (length args - i) args) t
    
        -- return the number you managed to delete
        f :: [TyVarBind] -> Type -> (Int, Type)
        f (UnkindedVar x:xs) (TyApp t1 (TyVar v))
            | fromNamed v == fromNamed x && x `notElem` universeBi t1
            = first (+1) $ f xs t1
        f _ t = (0,t)
-}

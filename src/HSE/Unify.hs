{-# LANGUAGE PatternGuards, ViewPatterns, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HSE.Unify(
    Subst, fromSubst,
    validSubst, substitute,
    unifyExp,
    ) where

import Control.Applicative
import Data.List.Extra
import Data.Maybe
import Data.Data
import Data.Semigroup
import Config.Type
import Hint.Type
import Control.Monad
import Data.Tuple.Extra
import Util
import Prelude


---------------------------------------------------------------------
-- SUBSTITUTION DATA TYPE

-- | A list of substitutions. A key may be duplicated, you need to call 'check'
--   to ensure the substitution is valid.
newtype Subst a = Subst [(String, a)]
    deriving (Semigroup, Monoid)

-- | Unpack the substitution
fromSubst :: Subst a -> [(String, a)]
fromSubst (Subst xs) = xs

instance Functor Subst where
    fmap f (Subst xs) = Subst $ map (second f) xs

instance Pretty a => Show (Subst a) where
    show (Subst xs) = unlines [a ++ " = " ++ prettyPrint b | (a,b) <- xs]


-- check the unification is valid and simplify it
validSubst :: (a -> a -> Bool) -> Subst a -> Maybe (Subst a)
validSubst eq = fmap Subst . mapM f . groupSort . fromSubst
    where f (x,y:ys) | all (eq y) ys = Just (x,y)
          f _ = Nothing


-- | Perform a substitution
substitute :: Subst Exp_ -> Exp_ -> Exp_
substitute (Subst bind) = transformBracketOld exp . transformBi pat . transformBi typ
    where
        exp (Var _ (fromNamed -> x)) = lookup x bind
        exp (InfixApp s lhs (fromNamed -> x) rhs) =
            (\op -> InfixApp s lhs op rhs) <$> lookupOp x
        exp (LeftSection s exp (fromNamed -> x)) =
            LeftSection s exp <$> lookupOp x
        exp (RightSection s (fromNamed -> x) exp) =
            (\op -> RightSection s op exp) <$> lookupOp x
        exp _ = Nothing

        -- Substition for operator must be an operator name
        lookupOp x = toNamed . fromNamed <$> lookup x bind

        pat (PVar _ (fromNamed -> x)) | Just y <- lookup x bind = toNamed $ fromNamed y
        pat x = x :: Pat_

        typ (TyVar _ (fromNamed -> x)) | Just (TypeApp _ y) <- lookup x bind = y
        typ x = x :: Type_

---------------------------------------------------------------------
-- UNIFICATION

type NameMatch = QName S -> QName S -> Bool

nmOp :: NameMatch -> QOp S -> QOp S -> Bool
nmOp nm (QVarOp _ x) (QVarOp _ y) = nm x y
nmOp nm (QConOp _ x) (QConOp _ y) = nm x y
nmOp nm  _ _ = False


-- | Unification, obeys the property that if @unify a b = s@, then @substitute s a = b@.
unify :: Data a => NameMatch -> Bool -> a -> a -> Maybe (Subst Exp_)
unify nm root x y
    | Just (x,y) <- cast (x,y) = unifyExp nm root x y
    | Just (x,y) <- cast (x,y) = unifyPat nm x y
    | Just (x,y) <- cast (x,y) = unifyType nm x y
    | Just (x :: S) <- cast x = Just mempty
    | otherwise = unifyDef nm x y


unifyDef :: Data a => NameMatch -> a -> a -> Maybe (Subst Exp_)
unifyDef nm x y = fmap mconcat . sequence =<< gzip (unify nm False) x y


-- App/InfixApp are analysed specially for performance reasons
-- root = True, this is the outside of the expr
-- do not expand out a dot at the root, since otherwise you get two matches because of readRule (Bug #570)
unifyExp :: NameMatch -> Bool -> Exp_ -> Exp_ -> Maybe (Subst Exp_)

-- brackets are not added when expanding $ in the users code, so tolerate them
-- in the match even if they aren't in the users code
unifyExp nm root x y | not root, isParen x, not $ isParen y = unifyExp nm root (fromParen x) y

-- don't subsitute for type apps, since no one writes rules imaginging they exist
unifyExp nm root (Var _ (fromNamed -> v)) y | isUnifyVar v, not $ isTypeApp y = Just $ Subst [(v,y)]
unifyExp nm root (Var _ x) (Var _ y) | nm x y = Just mempty

-- Match wildcard operators
unifyExp nm root (InfixApp _ lhs1 (fromNamed -> v) rhs1) (InfixApp _ lhs2 (fromNamed -> op2) rhs2)
    | isUnifyVar v =
        (Subst [(v, toNamed op2)] <>) <$>
        liftM2 (<>) (unifyExp nm False lhs1 lhs2) (unifyExp nm False rhs1 rhs2)
unifyExp nm root (LeftSection _ exp1 (fromNamed -> v)) (LeftSection _ exp2 (fromNamed -> op2))
    | isUnifyVar v =
        (Subst [(v, toNamed op2)] <>) <$> unifyExp nm False exp1 exp2
unifyExp nm root (RightSection _ (fromNamed -> v) exp1) (RightSection _ (fromNamed -> op2) exp2)
    | isUnifyVar v =
        (Subst [(v, toNamed op2)] <>) <$> unifyExp nm False exp1 exp2

-- Options: match directly, and expand through .
unifyExp nm root x@(App _ x1 x2) (App _ y1 y2) =
    liftM2 (<>) (unifyExp nm False x1 y1) (unifyExp nm False x2 y2) `mplus`
    (do guard $ not root
            -- don't expand . if at the root, otherwise you can get duplicate matches
            -- because the matching engine auto-generates hints in dot-form
        InfixApp _ y11 dot y12 <- return $ fromParen y1
        guard $ isDot dot
        unifyExp nm root x (App an y11 (App an y12 y2)))

-- Options: match directly, then expand through $, then desugar infix
unifyExp nm root x (InfixApp _ lhs2 op2 rhs2)
    | InfixApp _ lhs1 op1 rhs1 <- x = guard (nmOp nm op1 op2) >> liftM2 (<>) (unifyExp nm False lhs1 lhs2) (unifyExp nm False rhs1 rhs2)
    | isDol op2 = unifyExp nm root x $ App an lhs2 rhs2
    | otherwise = unifyExp nm root x $ App an (App an (opExp op2) lhs2) rhs2

unifyExp nm root x y | isOther x, isOther y = unifyDef nm x y
    where
        -- types that are not already handled in unify
        {-# INLINE isOther #-}
        isOther Var{} = False
        isOther App{} = False
        isOther InfixApp{} = False
        isOther _ = True

unifyExp nm root _ _ = Nothing


unifyPat :: NameMatch -> Pat_ -> Pat_ -> Maybe (Subst Exp_)
unifyPat nm (PVar _ x) (PVar _ y) = Just $ Subst [(fromNamed x, toNamed $ fromNamed y)]
unifyPat nm (PVar _ x) PWildCard{} = Just $ Subst [(fromNamed x, toNamed $ "_" ++ fromNamed x)]
unifyPat nm x y = unifyDef nm x y


unifyType :: NameMatch -> Type_ -> Type_ -> Maybe (Subst Exp_)
unifyType nm (TyVar a x) y = Just $ Subst [(fromNamed x, TypeApp a y)]
unifyType nm x y = unifyDef nm x y

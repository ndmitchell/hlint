{-# LANGUAGE PatternGuards, ViewPatterns, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Util.Unify(
    Subst', fromSubst',
    validSubst', substitute',
    unifyExp'
    ) where

import Control.Monad
import Data.Generics.Uniplate.Operations
import Data.Char
import Data.List.Extra
import Data.Data
import Data.Tuple.Extra
import Util

import HsSyn
import SrcLoc as GHC
import Outputable hiding ((<>))
import RdrName
import OccName

import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import GHC.Util.Outputable
import GHC.Util.HsExpr
import GHC.Util.Pat
import GHC.Util.RdrName
import GHC.Util.View

isUnifyVar :: String -> Bool
isUnifyVar [x] = x == '?' || isAlpha x
isUnifyVar [] = False
isUnifyVar xs = all (== '?') xs

---------------------------------------------------------------------
-- SUBSTITUTION DATA TYPE

-- A list of substitutions. A key may be duplicated, you need to call
--  'check' to ensure the substitution is valid.
newtype Subst' a = Subst' [(String, a)]
    deriving (Semigroup, Monoid)

-- Unpack the substitution.
fromSubst' :: Subst' a -> [(String, a)]
fromSubst' (Subst' xs) = xs

instance Functor Subst' where
    fmap f (Subst' xs) = Subst' $ map (second f) xs -- Interesting.

instance Outputable a => Show (Subst' a) where
    show (Subst' xs) = unlines [a ++ " = " ++ unsafePrettyPrint b | (a,b) <- xs]

-- Check the unification is valid and simplify it.
validSubst' :: (a -> a -> Bool) -> Subst' a -> Maybe (Subst' a)
validSubst' eq = fmap Subst' . mapM f . groupSort . fromSubst'
    where f (x, y : ys) | all (eq y) ys = Just (x, y)
          f _ = Nothing

-- Peform a substition.
substitute' :: Subst' (LHsExpr GhcPs) -> LHsExpr GhcPs -> LHsExpr GhcPs
substitute' (Subst' bind) = transformBracketOld' exp . transformBi pat . transformBi typ
  where
    exp :: LHsExpr GhcPs -> Maybe (LHsExpr GhcPs)
    -- Variables.
    exp (LL _ (HsVar _ x)) = lookup (rdrNameStr' x) bind
    -- Operator applications.
    exp (LL loc (OpApp _ lhs (LL _ (HsVar _ x)) rhs))
      | Just y <- lookup (rdrNameStr' x) bind = Just (cL loc (OpApp noExt lhs y rhs))
    -- Left sections.
    exp (LL loc (SectionL _ exp (LL _ (HsVar _ x))))
      | Just y <- lookup (rdrNameStr' x) bind = Just (cL loc (SectionL noExt exp y))
    -- Right sections.
    exp (LL loc (SectionR _ (LL _ (HsVar _ x)) exp))
      | Just y <- lookup (rdrNameStr' x) bind = Just (cL loc (SectionR noExt y exp))
    exp _ = Nothing

    pat :: LPat GhcPs -> LPat GhcPs
    -- Pattern variables.
    pat (LL _ (VarPat _ x))
      | Just y@(LL _ HsVar{}) <- lookup (rdrNameStr' x) bind = strToPat' (varToStr y)
    pat x = x :: LPat GhcPs

    typ :: LHsType GhcPs -> LHsType GhcPs
    -- Type variables.
    typ (LL _ (HsTyVar _ _ x))
      | Just (LL _ (HsAppType _ _ (HsWC _ y))) <- lookup (rdrNameStr' x) bind = y
    typ x = x :: LHsType GhcPs


---------------------------------------------------------------------
-- UNIFICATION

type NameMatch' = Located RdrName -> Located RdrName -> Bool

-- | Unification, obeys the property that if @unify a b = s@, then
-- @substitute s a = b@.
unify' :: Data a => NameMatch' -> Bool -> a -> a -> Maybe (Subst' (LHsExpr GhcPs))
unify' nm root x y
    | Just (x, y) <- cast (x, y) = unifyExp' nm root x y
    | Just (x, y) <- cast (x, y) = unifyPat' nm x y
    | Just (x, y) <- cast (x, y) = unifyType' nm x y
    | Just (x :: GHC.SrcSpan) <- cast x = Just mempty
    | otherwise = unifyDef' nm x y

unifyDef' :: Data a => NameMatch' -> a -> a -> Maybe (Subst' (LHsExpr GhcPs))
unifyDef' nm x y = fmap mconcat . sequence =<< gzip (unify' nm False) x y

-- App/InfixApp are analysed specially for performance reasons. If
-- 'root = True', this is the outside of the expr. Do not expand out a
-- dot at the root, since otherwise you get two matches because of
-- 'readRule' (Bug #570).
unifyExp' :: NameMatch' -> Bool -> LHsExpr GhcPs -> LHsExpr GhcPs -> Maybe (Subst' (LHsExpr GhcPs) )
-- Brackets are not added when expanding '$' in user code, so tolerate
-- them in the match even if they aren't in the user code.
unifyExp' nm root x y | not root, isPar x, not $ isPar y = unifyExp' nm root (fromParen' x) y
-- Don't subsitute for type apps, since no one writes rules imaginging
-- they exist.
unifyExp' nm root (LL _ (HsVar _ (rdrNameStr' -> v))) y | isUnifyVar v, not $ isTypeApp y = Just $ Subst' [(v, y)]
unifyExp' nm root (LL _ (HsVar _ x)) (LL _ (HsVar _ y)) | nm x y = Just mempty

-- Match wildcard operators.
unifyExp' nm root (LL _ (OpApp _ lhs1 (LL _ (HsVar _ (rdrNameStr' -> v))) rhs1))
                  (LL _ (OpApp _ lhs2 (LL _ (HsVar _ (rdrNameStr' -> op2))) rhs2))
    | isUnifyVar v =
        (Subst' [(v, strToVar op2)] <>) <$>
        liftM2 (<>) (unifyExp' nm False lhs1 lhs2) (unifyExp' nm False rhs1 rhs2)
unifyExp' nm root (LL _ (SectionL _ exp1 (LL _ (HsVar _ (rdrNameStr' -> v)))))
                  (LL _ (SectionL _ exp2 (LL _ (HsVar _ (rdrNameStr' -> op2)))))
    | isUnifyVar v = (Subst' [(v, strToVar op2)] <>) <$> unifyExp' nm False exp1 exp2
unifyExp' nm root (LL _ (SectionR _ (LL _ (HsVar _ (rdrNameStr' -> v))) exp1))
                  (LL _ (SectionR _ (LL _ (HsVar _ (rdrNameStr' -> op2))) exp2))
    | isUnifyVar v = (Subst' [(v, strToVar op2)] <>) <$> unifyExp' nm False exp1 exp2

-- Options: match directly, and expand through '.'
unifyExp' nm root x@(LL _ (HsApp _ x1 x2)) (LL _ (HsApp _ y1 y2)) =
    liftM2 (<>) (unifyExp' nm False x1 y1) (unifyExp' nm False x2 y2) `mplus`
    (do guard $ not root
            -- Don't expand '.' f at the root, otherwise you can get
            -- duplicate matches because the matching engine
            -- auto-generates hints in dot-form.
        (LL _ (OpApp _ y11 dot y12)) <- return $ fromParen' y1
        guard $ isDot dot
        unifyExp' nm root x (noLoc (HsApp noExt y11 (noLoc (HsApp noExt y12 y2))))
    )

-- Options: match directly, then expand through '$', then desugar infix.
unifyExp' nm root x (LL _ (OpApp _ lhs2 op2@(LL _ (HsVar _ op2')) rhs2))
    | (LL _ (OpApp _ lhs1 op1@(LL _ (HsVar _ op1')) rhs1)) <- x = guard (nm op1' op2') >> liftM2 (<>) (unifyExp' nm False lhs1 lhs2) (unifyExp' nm False rhs1 rhs2)
    | isDol op2 = unifyExp' nm root x $ noLoc (HsApp noExt lhs2 rhs2)
    | otherwise  = unifyExp' nm root x $ noLoc (HsApp noExt (noLoc (HsApp noExt op2 lhs2)) rhs2)

unifyExp' nm root x y | isOther x, isOther y = unifyDef' nm x y
    where
        -- Types that are not already handled in unify.
        {-# INLINE isOther #-}
        isOther :: LHsExpr GhcPs -> Bool
        isOther (LL _ HsVar{}) = False
        isOther (LL _ HsApp{}) = False
        isOther (LL _ OpApp{}) = False
        isOther _ = True

unifyExp' _ _ _ _ = Nothing


unifyPat' :: NameMatch' -> LPat GhcPs -> LPat GhcPs -> Maybe (Subst' (LHsExpr GhcPs))
unifyPat' nm (LL _ (VarPat _ x)) (LL _ (VarPat _ y)) =
  Just $ Subst' [(rdrNameStr' x, strToVar(rdrNameStr' y))]
unifyPat' nm (LL _ (VarPat _ x)) (LL _ (WildPat _)) =
  let s = rdrNameStr' x in Just $ Subst' [(s, strToVar("_" ++ s))]
unifyPat' nm (LL _ (ConPatIn x _)) (LL _ (ConPatIn y _)) | rdrNameStr' x /= rdrNameStr' y =
  Nothing
unifyPat' nm x y =
  unifyDef' nm x y

unifyType' :: NameMatch' -> LHsType GhcPs -> LHsType GhcPs -> Maybe (Subst' (LHsExpr GhcPs))
unifyType' nm (LL loc (HsTyVar _ _ x)) y =
  let wc = HsWC noExt y :: LHsWcType (NoGhcTc GhcPs)
      unused = noLoc (HsVar noExt (noLoc $ mkRdrUnqual (mkVarOcc "__unused__"))) :: LHsExpr GhcPs
      appType = cL loc (HsAppType noExt unused wc) :: LHsExpr GhcPs
 in Just $ Subst' [(rdrNameStr' x, appType)]
unifyType' nm x y = unifyDef' nm x y

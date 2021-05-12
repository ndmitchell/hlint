{-# LANGUAGE PatternGuards, ViewPatterns, FlexibleContexts, ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

module GHC.Util.Unify(
    Subst(..), fromSubst,
    validSubst, removeParens, substitute,
    unifyExp
    ) where

import Control.Applicative
import Control.Monad
import Data.Generics.Uniplate.DataOnly
import Data.Char
import Data.Data
import Data.List.Extra
import Util

import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Utils.Outputable hiding ((<>))
import GHC.Types.Name.Reader

import Language.Haskell.GhclibParserEx.GHC.Hs.Pat
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
import GHC.Util.HsExpr
import GHC.Util.View
import Data.Maybe
import GHC.Data.FastString

isUnifyVar :: String -> Bool
isUnifyVar [x] = x == '?' || isAlpha x
isUnifyVar [] = False
isUnifyVar xs = all (== '?') xs

---------------------------------------------------------------------
-- SUBSTITUTION DATA TYPE

-- A list of substitutions. A key may be duplicated, you need to call
--  'check' to ensure the substitution is valid.
newtype Subst a = Subst [(String, a)]
    deriving (Semigroup, Monoid, Functor)

-- Unpack the substitution.
fromSubst :: Subst a -> [(String, a)]
fromSubst (Subst xs) = xs

instance Outputable a => Show (Subst a) where
    show (Subst xs) = unlines [a ++ " = " ++ unsafePrettyPrint b | (a,b) <- xs]

-- Check the unification is valid and simplify it.
validSubst :: (a -> a -> Bool) -> Subst a -> Maybe (Subst a)
validSubst eq = fmap Subst . mapM f . groupSort . fromSubst
    where f (x, y : ys) | all (eq y) ys = Just (x, y)
          f _ = Nothing

-- Remove unnecessary brackets from a Subst. The first argument is a list of unification variables
-- for which brackets should be removed from their substitutions.
removeParens :: [String] -> Subst (LHsExpr GhcPs) -> Subst (LHsExpr GhcPs)
removeParens noParens (Subst xs) = Subst $
  map (\(x, y) -> if x `elem` noParens then (x, fromParen y) else (x, y)) xs

-- Peform a substition.
-- Returns (suggested replacement, (refactor template, no bracket vars)). It adds/removes brackets
-- for both the suggested replacement and the refactor template appropriately. The "no bracket vars"
-- is a list of substituation variables which, when expanded, should have the brackets stripped.
--
-- Examples:
--   (traverse foo (bar baz), (traverse f (x), []))
--   (zipWith foo bar baz, (f a b, [f]))
substitute :: Subst (LHsExpr GhcPs) -> LHsExpr GhcPs -> (LHsExpr GhcPs, (LHsExpr GhcPs, [String]))
substitute (Subst bind) = transformBracketOld exp . transformBi pat . transformBi typ
  where
    exp :: LHsExpr GhcPs -> Maybe (LHsExpr GhcPs)
    -- Variables.
    exp (L _ (HsVar _ x)) = lookup (rdrNameStr x) bind
    -- Operator applications.
    exp (L loc (OpApp _ lhs (L _ (HsVar _ x)) rhs))
      | Just y <- lookup (rdrNameStr x) bind = Just (L loc (OpApp EpAnnNotUsed lhs y rhs))
    -- Left sections.
    exp (L loc (SectionL _ exp (L _ (HsVar _ x))))
      | Just y <- lookup (rdrNameStr x) bind = Just (L loc (SectionL EpAnnNotUsed exp y))
    -- Right sections.
    exp (L loc (SectionR _ (L _ (HsVar _ x)) exp))
      | Just y <- lookup (rdrNameStr x) bind = Just (L loc (SectionR EpAnnNotUsed y exp))
    exp _ = Nothing

    pat :: LPat GhcPs -> LPat GhcPs
    -- Pattern variables.
    pat (L _ (VarPat _ x))
      | Just y@(L _ HsVar{}) <- lookup (rdrNameStr x) bind = strToPat $ varToStr y
    pat x = x :: LPat GhcPs

    typ :: LHsType GhcPs -> LHsType GhcPs
    -- Type variables.
    typ (L _ (HsTyVar _ _ x))
      | Just (L _ (HsAppType _ _ (HsWC _ y))) <- lookup (rdrNameStr x) bind = y
    typ x = x :: LHsType GhcPs


---------------------------------------------------------------------
-- UNIFICATION

type NameMatch = LocatedN RdrName -> LocatedN RdrName -> Bool

-- | Unification, obeys the property that if @unify a b = s@, then
-- @substitute s a = b@.
unify' :: Data a => NameMatch -> Bool -> a -> a -> Maybe (Subst (LHsExpr GhcPs))
unify' nm root x y
    | Just (x, y) <- cast (x, y) = unifyExp' nm root x y
    | Just (x, y) <- cast (x, y) = unifyPat' nm x y
    | Just (x, y) <- cast (x, y) = unifyType' nm x y
    | Just (x, y) <- cast (x, y) = if (x :: FastString) == y then Just mempty else Nothing

    -- We need some type magic to reduce this.
    | Just (x :: EpAnn AnnsModule) <- cast x = Just mempty
    | Just (x :: EpAnn NameAnn) <- cast x = Just mempty
    | Just (x :: EpAnn AnnListItem) <- cast x = Just mempty
    | Just (x :: EpAnn AnnList) <- cast x = Just mempty
    | Just (x :: EpAnn AnnPragma) <- cast x = Just mempty
    | Just (x :: EpAnn AnnContext) <- cast x = Just mempty
    | Just (x :: EpAnn AnnParen) <- cast x = Just mempty
    | Just (x :: EpAnn Anchor) <- cast x = Just mempty
    | Just (x :: EpAnn NoEpAnns) <- cast x = Just mempty
    | Just (x :: EpAnn GrhsAnn) <- cast x = Just mempty
    | Just (x :: EpAnn [AddEpAnn]) <- cast x = Just mempty
    | Just (x :: EpAnn EpAnnHsCase) <- cast x = Just mempty
    | Just (x :: EpAnn EpAnnUnboundVar) <- cast x = Just mempty
    | Just (x :: EpAnn AnnExplicitSum) <- cast x = Just mempty
    | Just (x :: EpAnn AnnsLet) <- cast x = Just mempty
    | Just (x :: EpAnn AnnProjection) <- cast x = Just mempty
    | Just (x :: EpAnn Anchor) <- cast x = Just mempty
    | Just (x :: EpAnn EpaLocation) <- cast x = Just mempty
    | Just (x :: EpAnn AnnFieldLabel) <- cast x = Just mempty
    | Just (x :: EpAnn EpAnnSumPat) <- cast x = Just mempty
    | Just (x :: EpAnn AnnSig) <- cast x = Just mempty
    | Just (x :: EpAnn HsRuleAnn) <- cast x = Just mempty
    | Just (x :: EpAnn EpAnnImportDecl) <- cast x = Just mempty
    | Just (x :: EpAnn (AddEpAnn, AddEpAnn)) <- cast x = Just mempty
    | Just (y :: SrcSpan) <- cast y = Just mempty

    | otherwise = unifyDef' nm x y

unifyDef' :: Data a => NameMatch -> a -> a -> Maybe (Subst (LHsExpr GhcPs))
unifyDef' nm x y =
  fmap mconcat . sequence =<< gzip (unify' nm False) x y

unifyComposed' :: NameMatch
               -> LHsExpr GhcPs
               -> LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
               -> Maybe (Subst (LHsExpr GhcPs), Maybe (LHsExpr GhcPs))
unifyComposed' nm x1 y11 dot y12 =
  ((, Just y11) <$> unifyExp' nm False x1 y12)
    <|> case y12 of
          (L _ (OpApp _ y121 dot' y122)) | isDot dot' ->
            unifyComposed' nm x1 (noLocA (OpApp EpAnnNotUsed y11 dot y121)) dot' y122
          _ -> Nothing

-- unifyExp handles the cases where both x and y are HsApp, or y is OpApp. Otherwise,
-- delegate to unifyExp'. These are the cases where we potentially need to call
-- unifyComposed' to handle left composition.
--
-- y is allowed to partially match x (the lhs of the hint), if y is a function application where
-- the function is a composition of functions. In this case the second component of the result is
-- the unmatched part of y, which will be attached to the rhs of the hint after substitution.
--
-- Example:
--   x = head (drop n x)
--   y = foo . bar . baz . head $ drop 2 xs
--   result = (Subst [(n, 2), (x, xs)], Just (foo . bar . baz))
unifyExp :: NameMatch -> Bool -> LHsExpr GhcPs -> LHsExpr GhcPs -> Maybe (Subst (LHsExpr GhcPs), Maybe (LHsExpr GhcPs))
-- Match wildcard operators.
unifyExp nm root (L _ (OpApp _ lhs1 (L _ (HsVar _ (rdrNameStr -> v))) rhs1))
                 (L _ (OpApp _ lhs2 (L _ (HsVar _ (rdrNameStr -> op2))) rhs2))
    | isUnifyVar v =
        (, Nothing) . (Subst [(v, strToVar op2)] <>) <$>
        liftA2 (<>) (unifyExp' nm False lhs1 lhs2) (unifyExp' nm False rhs1 rhs2)

-- Options: match directly, and expand through '.'
unifyExp nm root x@(L _ (HsApp _ x1 x2)) (L _ (HsApp _ y1 y2)) =
    ((, Nothing) <$> liftA2 (<>) (unifyExp' nm False x1 y1) (unifyExp' nm False x2 y2)) <|> unifyComposed
  where
    -- Unify a function application where the function is a composition of functions.
    unifyComposed
      | (L _ (OpApp _ y11 dot y12)) <- fromParen y1, isDot dot =
          if not root then
              -- Attempt #1: rewrite '(fun1 . fun2) arg' as 'fun1 (fun2 arg)', and unify it with 'x'.
              -- The guard ensures that you don't get duplicate matches because the matching engine
              -- auto-generates hints in dot-form.
              (, Nothing) <$> unifyExp' nm root x (noLocA (HsApp EpAnnNotUsed y11 (noLocA (HsApp EpAnnNotUsed y12 y2))))
          else do
              -- Attempt #2: rewrite '(fun1 . fun2 ... funn) arg' as 'fun1 $ (fun2 ... funn) arg',
              -- 'fun1 . fun2 $ (fun3 ... funn) arg', 'fun1 . fun2 . fun3 $ (fun4 ... funn) arg',
              -- and so on, unify the rhs of '$' with 'x', and store the lhs of '$' into 'extra'.
              -- You can only add to extra if you are at the root (otherwise 'extra' has nowhere to go).
              rhs <- unifyExp' nm False x2 y2
              (lhs, extra) <- unifyComposed' nm x1 y11 dot y12
              pure (lhs <> rhs, extra)
      | otherwise = Nothing

-- Options: match directly, then expand through '$', then desugar infix.
unifyExp nm root x (L _ (OpApp _ lhs2 op2@(L _ (HsVar _ op2')) rhs2))
    | (L _ (OpApp _ lhs1 op1@(L _ (HsVar _ op1')) rhs1)) <- x =
        guard (nm op1' op2') >> (, Nothing) <$> liftA2 (<>) (unifyExp' nm False lhs1 lhs2) (unifyExp' nm False rhs1 rhs2)
    | isDol op2 = unifyExp nm root x $ noLocA (HsApp EpAnnNotUsed lhs2 rhs2)
    | isAmp op2 = unifyExp nm root x $ noLocA (HsApp EpAnnNotUsed rhs2 lhs2)
    | otherwise  = unifyExp nm root x $ noLocA (HsApp EpAnnNotUsed (noLocA (HsApp EpAnnNotUsed op2 (addPar lhs2))) (addPar rhs2))
        where
          -- add parens around when desugaring the expression, if necessary
          addPar :: LHsExpr GhcPs -> LHsExpr GhcPs
          addPar x = if isAtom x then x else addParen x

unifyExp nm root x y = (, Nothing) <$> unifyExp' nm root x y

isAmp :: LHsExpr GhcPs -> Bool
isAmp (L _ (HsVar _ x)) = rdrNameStr x == "&"
isAmp _ = False

-- | If we "throw away" the extra than we have no where to put it, and the substitution is wrong
noExtra :: Maybe (Subst (LHsExpr GhcPs), Maybe (LHsExpr GhcPs)) -> Maybe (Subst (LHsExpr GhcPs))
noExtra (Just (x, Nothing)) = Just x
noExtra _ = Nothing

-- App/InfixApp are analysed specially for performance reasons. If
-- 'root = True', this is the outside of the expr. Do not expand out a
-- dot at the root, since otherwise you get two matches because of
-- 'readRule' (Bug #570).
unifyExp' :: NameMatch -> Bool -> LHsExpr GhcPs -> LHsExpr GhcPs -> Maybe (Subst (LHsExpr GhcPs))
-- Don't subsitute for type apps, since no one writes rules imagining
-- they exist.
unifyExp' nm root (L _ (HsVar _ (rdrNameStr -> v))) y | isUnifyVar v, not $ isTypeApp y = Just $ Subst [(v, y)]
unifyExp' nm root (L _ (HsVar _ x)) (L _ (HsVar _ y)) | nm x y = Just mempty

-- Brackets are not added when expanding '$' in user code, so tolerate
-- them in the match even if they aren't in the user code.
-- Also, allow the user to put in more brackets than they strictly need (e.g. with infix).
unifyExp' nm root x y | not root, isJust x2 || isJust y2 = unifyExp' nm root (fromMaybe x x2) (fromMaybe y y2)
    where
        -- Make sure we deal with the weird brackets that can't be removed around sections
        x2 = remParen x
        y2 = remParen y

unifyExp' nm root x@(L _ (OpApp _ lhs1 (L _ (HsVar _ (rdrNameStr -> v))) rhs1))
                  y@(L _ (OpApp _ lhs2 (L _ (HsVar _ op2)) rhs2)) =
  noExtra $ unifyExp nm root x y
unifyExp' nm root (L _ (SectionL _ exp1 (L _ (HsVar _ (rdrNameStr -> v)))))
                  (L _ (SectionL _ exp2 (L _ (HsVar _ (rdrNameStr -> op2)))))
    | isUnifyVar v = (Subst [(v, strToVar op2)] <>) <$> unifyExp' nm False exp1 exp2
unifyExp' nm root (L _ (SectionR _ (L _ (HsVar _ (rdrNameStr -> v))) exp1))
                  (L _ (SectionR _ (L _ (HsVar _ (rdrNameStr -> op2))) exp2))
    | isUnifyVar v = (Subst [(v, strToVar op2)] <>) <$> unifyExp' nm False exp1 exp2

unifyExp' nm root x@(L _ (HsApp _ x1 x2)) y@(L _ (HsApp _ y1 y2)) =
  noExtra $ unifyExp nm root x y

unifyExp' nm root x y@(L _ (OpApp _ lhs2 op2@(L _ (HsVar _ op2')) rhs2)) =
  noExtra $ unifyExp nm root x y

unifyExp' nm root (L _ (HsBracket _ (VarBr _ b0 (occNameStr . unLoc -> v1))))
                  (L _ (HsBracket _ (VarBr _ b1 (occNameStr . unLoc -> v2))))
    | b0 == b1 && isUnifyVar v1 = Just (Subst [(v1, strToVar v2)])

unifyExp' nm root x y | isOther x, isOther y = unifyDef' nm x y
    where
        -- Types that are not already handled in unify.
        {-# INLINE isOther #-}
        isOther :: LHsExpr GhcPs -> Bool
        isOther (L _ HsVar{}) = False
        isOther (L _ HsApp{}) = False
        isOther (L _ OpApp{}) = False
        isOther _ = True

unifyExp' _ _ _ _ = Nothing


unifyPat' :: NameMatch -> LPat GhcPs -> LPat GhcPs -> Maybe (Subst (LHsExpr GhcPs))
unifyPat' nm (L _ (VarPat _ x)) (L _ (VarPat _ y)) =
  Just $ Subst [(rdrNameStr x, strToVar(rdrNameStr y))]
unifyPat' nm (L _ (VarPat _ x)) (L _ (WildPat _)) =
  let s = rdrNameStr x in Just $ Subst [(s, strToVar("_" ++ s))]
unifyPat' nm (L _ (ConPat _ x _)) (L _ (ConPat _ y _)) | rdrNameStr x /= rdrNameStr y =
  Nothing
unifyPat' nm x y =
  unifyDef' nm x y

unifyType' :: NameMatch -> LHsType GhcPs -> LHsType GhcPs -> Maybe (Subst (LHsExpr GhcPs))
unifyType' nm (L loc (HsTyVar _ _ x)) y =
  let wc = HsWC noExtField y :: LHsWcType (NoGhcTc GhcPs)
      unused = strToVar "__unused__" :: LHsExpr GhcPs
      appType = L loc (HsAppType noSrcSpan unused wc) :: LHsExpr GhcPs
 in Just $ Subst [(rdrNameStr x, appType)]
unifyType' nm x y = unifyDef' nm x y

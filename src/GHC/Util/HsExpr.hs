{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module GHC.Util.HsExpr (
    dotApps, lambda
  , simplifyExp, niceLambda, niceLambdaR
  , Brackets(..)
  , rebracket1, appsBracket, transformAppsM, fromApps, apps, universeApps, universeParentExp
  , paren
  , replaceBranches
  , needBracketOld, transformBracketOld, fromParen1
  , allowLeftSection, allowRightSection
) where

import GHC.Hs
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Data.FastString
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence
import GHC.Data.Bag(bagToList)

import GHC.Util.Brackets
import GHC.Util.FreeVars
import GHC.Util.View

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer.CPS

import Data.Data
import Data.Generics.Uniplate.DataOnly
import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe

import Refact (substVars, toSSA)
import Refact.Types hiding (SrcSpan, Match)
import qualified Refact.Types as R (SrcSpan)

import Language.Haskell.GhclibParserEx.GHC.Hs.Pat
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader

-- | 'dotApp a b' makes 'a . b'.
dotApp :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
dotApp x y = noLocA $ OpApp EpAnnNotUsed x (noLocA $ HsVar noExtField (noLocA $ mkVarUnqual (fsLit "."))) y

dotApps :: [LHsExpr GhcPs] -> LHsExpr GhcPs
dotApps [] = error "GHC.Util.HsExpr.dotApps', does not work on an empty list"
dotApps [x] = x
dotApps (x : xs) = dotApp x (dotApps xs)

-- | @lambda [p0, p1..pn] body@ makes @\p1 p1 .. pn -> body@
lambda :: [LPat GhcPs] -> LHsExpr GhcPs -> LHsExpr GhcPs
lambda vs body = noLocA $ HsLam noExtField (MG noExtField (noLocA [noLocA $ Match EpAnnNotUsed LambdaExpr vs (GRHSs emptyComments [noLocA $ GRHS EpAnnNotUsed [] body] (EmptyLocalBinds noExtField))]) Generated)

-- | 'paren e' wraps 'e' in parens if 'e' is non-atomic.
paren :: LHsExpr GhcPs -> LHsExpr GhcPs
paren x
  | isAtom x  = x
  | otherwise = addParen x

universeParentExp :: Data a => a -> [(Maybe (Int, LHsExpr GhcPs), LHsExpr GhcPs)]
universeParentExp xs = concat [(Nothing, x) : f x | x <- childrenBi xs]
    where f p = concat [(Just (i,p), c) : f c | (i,c) <- zipFrom 0 $ children p]


apps :: [LHsExpr GhcPs] -> LHsExpr GhcPs
apps = foldl1' mkApp where mkApp x y = noLocA (HsApp EpAnnNotUsed x y)

fromApps :: LHsExpr GhcPs  -> [LHsExpr GhcPs]
fromApps (L _ (HsApp _ x y)) = fromApps x ++ [y]
fromApps x = [x]

childrenApps :: LHsExpr GhcPs -> [LHsExpr GhcPs]
childrenApps (L _ (HsApp _ x y)) = childrenApps x ++ [y]
childrenApps x = children x

universeApps :: LHsExpr GhcPs -> [LHsExpr GhcPs]
universeApps x = x : concatMap universeApps (childrenApps x)

descendAppsM :: Monad m => (LHsExpr GhcPs  -> m (LHsExpr GhcPs)) -> LHsExpr GhcPs -> m (LHsExpr GhcPs)
descendAppsM f (L l (HsApp _ x y)) = liftA2 (\x y -> L l $ HsApp EpAnnNotUsed x y) (descendAppsM f x) (f y)
descendAppsM f x = descendM f x

transformAppsM :: Monad m => (LHsExpr GhcPs -> m (LHsExpr GhcPs)) -> LHsExpr GhcPs -> m (LHsExpr GhcPs)
transformAppsM f x = f =<< descendAppsM (transformAppsM f) x

descendIndex :: Data a => (Int -> a -> a) -> a -> a
descendIndex f = fst . descendIndex' (\x a -> writer (f x a, ()))

descendIndex' :: (Data a, Monoid w) => (Int -> a -> Writer w a) -> a -> (a, w)
descendIndex' f x = runWriter $ flip evalStateT 0 $ flip descendM x $ \y -> do
    i <- get
    modify (+1)
    lift $ f i y

--  There are differences in pretty-printing between GHC and HSE. This
--  version never removes brackets.
descendBracket :: (LHsExpr GhcPs -> (Bool, LHsExpr GhcPs)) -> LHsExpr GhcPs -> LHsExpr GhcPs
descendBracket op x = descendIndex g x
    where
        g i y = if a then f i b else b
            where (a, b) = op y
        f i y = if needBracket i x y then addParen y else y

-- Add brackets as suggested 'needBracket at 1-level of depth.
rebracket1 :: LHsExpr GhcPs -> LHsExpr GhcPs
rebracket1 = descendBracket (True, )

-- A list of application, with any necessary brackets.
appsBracket :: [LHsExpr GhcPs] -> LHsExpr GhcPs
appsBracket = foldl1 mkApp
  where mkApp x y = rebracket1 (noLocA $ HsApp EpAnnNotUsed x y)

simplifyExp :: LHsExpr GhcPs -> LHsExpr GhcPs
-- Replace appliciations 'f $ x' with 'f (x)'.
simplifyExp (L l (OpApp _ x op y)) | isDol op = L l (HsApp EpAnnNotUsed x (nlHsPar y))
simplifyExp e@(L _ (HsLet _ _ ((HsValBinds _ (ValBinds _ binds []))) _ z)) =
  -- An expression of the form, 'let x = y in z'.
  case bagToList binds of
    [L _ (FunBind _ _ (MG _ (L _ [L _ (Match _(FunRhs (L _ x) _ _) [] (GRHSs _[L _ (GRHS _ [] y)] ((EmptyLocalBinds _))))]) _) _)]
         -- If 'x' is not in the free variables of 'y', beta-reduce to
         -- 'z[(y)/x]'.
      | occNameStr x `notElem` vars y && length [() | Unqual a <- universeBi z, a == rdrNameOcc x] <= 1 ->
          transform f z
          where f (view -> Var_ x') | occNameStr x == x' = paren y
                f x = x
    _ -> e
simplifyExp e = e

-- Rewrite '($) . b' as 'b'.
niceDotApp :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
niceDotApp (L _ (HsVar _ (L _ r))) b | occNameStr r == "$" = b
niceDotApp a b = dotApp a b

-- Generate a lambda expression but prettier if possible.
niceLambda :: [String] -> LHsExpr GhcPs -> LHsExpr GhcPs
niceLambda ss e = fst (niceLambdaR ss e)-- We don't support refactorings yet.

allowRightSection :: String -> Bool
allowRightSection x = x `notElem` ["-","#"]
allowLeftSection :: String -> Bool
allowLeftSection x = x /= "#"

-- Implementation. Try to produce special forms (e.g. sections,
-- compositions) where we can.
niceLambdaR :: [String]
            -> LHsExpr GhcPs
            -> (LHsExpr GhcPs, R.SrcSpan -> [Refactoring R.SrcSpan])
-- Rewrite @\ -> e@ as @e@
-- These are encountered as recursive calls.
niceLambdaR xs (SimpleLambda [] x) = niceLambdaR xs x

-- Rewrite @\xs -> (e)@ as @\xs -> e@.
niceLambdaR xs (L _ (HsPar _ _ x _)) = niceLambdaR xs x

-- @\vs v -> ($) e v@ ==> @\vs -> e@
-- @\vs v -> e $ v@ ==> @\vs -> e@
niceLambdaR (unsnoc -> Just (vs, v)) (view -> App2 f e (view -> Var_ v'))
  | isDol f
  , v == v'
  , vars e `disjoint` [v]
  = niceLambdaR vs e

-- @\v -> thing + v@ ==> @\v -> (thing +)@  (heuristic: @v@ must be a single
-- lexeme, or it all gets too complex)
niceLambdaR [v] (L _ (OpApp _ e f (view -> Var_ v')))
  | isLexeme e
  , v == v'
  , vars e `disjoint` [v]
  , L _ (HsVar _ (L _ fname)) <- f
  , isSymOcc $ rdrNameOcc fname
  = let res = nlHsPar $ noLocA $ SectionL EpAnnNotUsed e f
     in (res, \s -> [Replace Expr s [] (unsafePrettyPrint res)])

-- @\vs v -> f x v@ ==> @\vs -> f x@
niceLambdaR (unsnoc -> Just (vs, v)) (L _ (HsApp _ f (view -> Var_ v')))
  | v == v'
  , vars f `disjoint` [v]
  = niceLambdaR vs f

-- @\vs v -> (v `f`)@ ==> @\vs -> f@
niceLambdaR (unsnoc -> Just (vs, v)) (L _ (SectionL _ (view -> Var_ v') f))
  | v == v' = niceLambdaR vs f

-- Strip one variable pattern from the end of a lambdas match, and place it in our list of factoring variables.
niceLambdaR xs (SimpleLambda ((view -> PVar_ v):vs) x)
  | v `notElem` xs = niceLambdaR (xs++[v]) $ lambda vs x

-- Rewrite @\x -> x + a@ as @(+ a)@ (heuristic: @a@ must be a single
-- lexeme, or it all gets too complex).
niceLambdaR [x] (view -> App2 op@(L _ (HsVar _ (L _ tag))) l r)
  | isLexeme r, view l == Var_ x, x `notElem` vars r, allowRightSection (occNameStr tag) =
      let e = rebracket1 $ addParen (noLocA $ SectionR EpAnnNotUsed op r)
      in (e, \s -> [Replace Expr s [] (unsafePrettyPrint e)])
-- Rewrite (1) @\x -> f (b x)@ as @f . b@, (2) @\x -> f $ b x@ as @f . b@.
niceLambdaR [x] y
  | Just (z, subts) <- factor y, x `notElem` vars z = (z, \s -> [mkRefact subts s])
  where
    -- Factor the expression with respect to x.
    factor :: LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, [LHsExpr GhcPs])
    factor (L _ (HsApp _ ini lst)) | view lst == Var_ x = Just (ini, [ini])
    factor (L _ (HsApp _ ini lst)) | Just (z, ss) <- factor lst
      = let r = niceDotApp ini z
        in if astEq r z then Just (r, ss) else Just (r, ini : ss)
    factor (L _ (OpApp _ y op (factor -> Just (z, ss))))| isDol op
      = let r = niceDotApp y z
        in if astEq r z then Just (r, ss) else Just (r, y : ss)
    factor (L _ (HsPar _ _ y@(L _ HsApp{}) _)) = factor y
    factor _ = Nothing
    mkRefact :: [LHsExpr GhcPs] -> R.SrcSpan -> Refactoring R.SrcSpan
    mkRefact subts s =
      let tempSubts = zipWith (\a b -> (a, toSSA b)) substVars subts
          template = dotApps (map (strToVar . fst) tempSubts)
      in Replace Expr s tempSubts (unsafePrettyPrint template)
-- Rewrite @\x y -> x + y@ as @(+)@.
niceLambdaR [x,y] (L _ (OpApp _ (view -> Var_ x1) op@(L _ HsVar {}) (view -> Var_ y1)))
    | x == x1, y == y1, vars op `disjoint` [x, y] = (op, \s -> [Replace Expr s [] (unsafePrettyPrint op)])
-- Rewrite @\x y -> f y x@ as @flip f@.
niceLambdaR [x, y] (view -> App2 op (view -> Var_ y1) (view -> Var_ x1))
  | x == x1, y == y1, vars op `disjoint` [x, y] =
      ( gen op
      , \s -> [Replace Expr s [("x", toSSA op)] (unsafePrettyPrint $ gen (strToVar "x"))]
      )
  where
    gen :: LHsExpr GhcPs -> LHsExpr GhcPs
    gen = noLocA . HsApp EpAnnNotUsed (strToVar "flip")
        . if isAtom op then id else addParen

-- We're done factoring, but have no variables left, so we shouldn't make a lambda.
-- @\ -> e@ ==> @e@
niceLambdaR [] e = (e, \s -> [Replace Expr s [("a", toSSA e)] "a"])
-- Base case. Just a good old fashioned lambda.
niceLambdaR ss e =
  let grhs = noLocA $ GRHS EpAnnNotUsed [] e :: LGRHS GhcPs (LHsExpr GhcPs)
      grhss = GRHSs {grhssExt = emptyComments, grhssGRHSs=[grhs], grhssLocalBinds=EmptyLocalBinds noExtField}
      match = noLocA $ Match {m_ext=EpAnnNotUsed, m_ctxt=LambdaExpr, m_pats=map strToPat ss, m_grhss=grhss} :: LMatch GhcPs (LHsExpr GhcPs)
      matchGroup = MG {mg_ext=noExtField, mg_origin=Generated, mg_alts=noLocA [match]}
  in (noLocA $ HsLam noExtField matchGroup, const [])


-- 'case' and 'if' expressions have branches, nothing else does (this
-- doesn't consider 'HsMultiIf' perhaps it should?).
replaceBranches :: LHsExpr GhcPs -> ([LHsExpr GhcPs], [LHsExpr GhcPs] -> LHsExpr GhcPs)
replaceBranches (L l (HsIf _ a b c)) = ([b, c], \[b, c] -> L l (HsIf EpAnnNotUsed a b c))

replaceBranches (L s (HsCase _ a (MG _ (L l bs) FromSource))) =
  (concatMap f bs, \xs -> L s (HsCase EpAnnNotUsed a (MG noExtField (L l (g bs xs)) Generated)))
  where
    f :: LMatch GhcPs (LHsExpr GhcPs) -> [LHsExpr GhcPs]
    f (L _ (Match _ CaseAlt _ (GRHSs _ xs _))) = [x | (L _ (GRHS _ _ x)) <- xs]
    f _ = error "GHC.Util.HsExpr.replaceBranches: unexpected XMatch"

    g :: [LMatch GhcPs (LHsExpr GhcPs)] -> [LHsExpr GhcPs] -> [LMatch GhcPs (LHsExpr GhcPs)]
    g (L s1 (Match _ CaseAlt a (GRHSs _ ns b)) : rest) xs =
      L s1 (Match EpAnnNotUsed CaseAlt a (GRHSs emptyComments [L a (GRHS EpAnnNotUsed gs x) | (L a (GRHS _ gs _), x) <- zip ns as] b)) : g rest bs
      where  (as, bs) = splitAt (length ns) xs
    g [] [] = []
    g _ _ = error "GHC.Util.HsExpr.replaceBranches': internal invariant failed, lists are of differing lengths"

replaceBranches x = ([], \[] -> x)


-- Like needBracket, but with a special case for 'a . b . b', which was
-- removed from haskell-src-exts-util-0.2.2.
needBracketOld :: Int -> LHsExpr GhcPs -> LHsExpr GhcPs -> Bool
needBracketOld i parent child
  | isDotApp parent, isDotApp child, i == 2 = False
  | otherwise = needBracket i parent child

transformBracketOld :: (LHsExpr GhcPs -> Maybe (LHsExpr GhcPs))
                    -> LHsExpr GhcPs
                    -> (LHsExpr GhcPs, (LHsExpr GhcPs, [String]))
transformBracketOld op = first snd . g
  where
    g = first f . descendBracketOld g
    f x = maybe (False, x) (True, ) (op x)

-- Descend, and if something changes then add/remove brackets
-- appropriately. Returns (suggested replacement, (refactor template, no bracket vars)),
-- where "no bracket vars" is a list of substitution variables which, when expanded,
-- should have the brackets stripped.
descendBracketOld :: (LHsExpr GhcPs -> ((Bool, LHsExpr GhcPs), (LHsExpr GhcPs, [String])))
                  -> LHsExpr GhcPs
                  -> (LHsExpr GhcPs, (LHsExpr GhcPs, [String]))
descendBracketOld op x = (descendIndex g1 x, descendIndex' g2 x)
  where
    g i y = if a then (f1 i b z w, f2 i b z w) else (b, (z, w))
      where ((a, b), (z, w)) = op y

    g1 a b = fst (g a b)
    g2 a b = writer $ snd (g a b)

    f i (L _ (HsPar _ _ y _)) z w
      | not $ needBracketOld i x y = (y, removeBracket z)
      where
        -- If the template expr is a Var, record it so that we can remove the brackets
        -- later when expanding it. Otherwise, remove the enclosing brackets (if any).
        removeBracket = \case
          var@(L _ HsVar{}) -> (z, varToStr var : w)
          other -> (fromParen z, w)
    f i y z w
      | needBracketOld i x y = (addParen y, (addParen z, w))
      -- https://github.com/mpickering/apply-refact/issues/7
      | isOp y = (y, (addParen z, w))
    f _ y z w = (y, (z, w))

    f1 a b c d = fst (f a b c d)
    f2 a b c d = snd (f a b c d)

    isOp = \case
      L _ (HsVar _ (L _ name)) -> isSymbolRdrName name
      _ -> False

fromParen1 :: LHsExpr GhcPs -> LHsExpr GhcPs
fromParen1 x = fromMaybe x $ remParen x

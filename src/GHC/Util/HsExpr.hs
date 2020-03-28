{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns, MultiParamTypeClasses , FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module GHC.Util.HsExpr (
    dotApps', lambda
  , simplifyExp', niceLambda', niceLambdaR'
  , Brackets'(..)
  , rebracket1', appsBracket', transformAppsM', fromApps', apps', universeApps', universeParentExp'
  , paren'
  , replaceBranches'
  , needBracketOld', transformBracketOld', fromParen1'
  , allowLeftSection, allowRightSection
) where

import HsSyn
import BasicTypes
import SrcLoc
import FastString
import RdrName
import OccName
import Bag(bagToList)

import GHC.Util.Brackets
import GHC.Util.View
import GHC.Util.FreeVars
import GHC.Util.Outputable (unsafePrettyPrint)

import Control.Applicative
import Control.Monad.Trans.State

import Data.Data
import Data.Generics.Uniplate.Data
import Data.List.Extra
import Data.Tuple.Extra

import Refact (toSS')
import Refact.Types hiding (SrcSpan, Match)
import qualified Refact.Types as R (SrcSpan)

import Language.Haskell.GhclibParserEx.GHC.Hs.Pat
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances

-- | 'dotApp a b' makes 'a . b'.
dotApp' :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
dotApp' x y = noLoc $ OpApp noExt x (noLoc $ HsVar noExt (noLoc $ mkVarUnqual (fsLit "."))) y

dotApps' :: [LHsExpr GhcPs] -> LHsExpr GhcPs
dotApps' [] = error "GHC.Util.HsExpr.dotApps', does not work on an empty list"
dotApps' [x] = x
dotApps' (x : xs) = dotApp' x (dotApps' xs)

-- | @lambda [p0, p1..pn] body@ makes @\p1 p1 .. pn -> body@
lambda :: [Pat GhcPs] -> LHsExpr GhcPs -> LHsExpr GhcPs
lambda vs body = noLoc $ HsLam noExt (MG noExt (noLoc [noLoc $ Match noExt LambdaExpr vs (GRHSs noExt [noLoc $ GRHS noExt [] body] (noLoc $ EmptyLocalBinds noExt))]) Generated)

-- | 'paren e' wraps 'e' in parens if 'e' is non-atomic.
paren' :: LHsExpr GhcPs -> LHsExpr GhcPs
paren' x
  | isAtom' x  = x
  | otherwise = addParen' x

universeParentExp' :: Data a => a -> [(Maybe (Int, LHsExpr GhcPs), LHsExpr GhcPs)]
universeParentExp' xs = concat [(Nothing, x) : f x | x <- childrenBi xs]
    where f p = concat [(Just (i,p), c) : f c | (i,c) <- zipFrom 0 $ children p]


apps' :: [LHsExpr GhcPs] -> LHsExpr GhcPs
apps' = foldl1' mkApp where mkApp x y = noLoc (HsApp noExt x y)

fromApps' :: LHsExpr GhcPs  -> [LHsExpr GhcPs]
fromApps' (L _ (HsApp _ x y)) = fromApps' x ++ [y]
fromApps' x = [x]

childrenApps' :: LHsExpr GhcPs -> [LHsExpr GhcPs]
childrenApps' (L _ (HsApp _ x y)) = childrenApps' x ++ [y]
childrenApps' x = children x

universeApps' :: LHsExpr GhcPs -> [LHsExpr GhcPs]
universeApps' x = x : concatMap universeApps' (childrenApps' x)

descendAppsM' :: Monad m => (LHsExpr GhcPs  -> m (LHsExpr GhcPs)) -> LHsExpr GhcPs -> m (LHsExpr GhcPs)
descendAppsM' f (L l (HsApp _ x y)) = liftA2 (\x y -> L l $ HsApp noExt x y) (descendAppsM' f x) (f y)
descendAppsM' f x = descendM f x

transformAppsM' :: Monad m => (LHsExpr GhcPs -> m (LHsExpr GhcPs)) -> LHsExpr GhcPs -> m (LHsExpr GhcPs)
transformAppsM' f x = f =<< descendAppsM' (transformAppsM' f) x

descendIndex' :: Data a => (Int -> a -> a) -> a -> a
descendIndex' f x = flip evalState 0 $ flip descendM x $ \y -> do
    i <- get
    modify (+1)
    pure $ f i y

--  There are differences in pretty-printing between GHC and HSE. This
--  version never removes brackets.
descendBracket' :: (LHsExpr GhcPs -> (Bool, LHsExpr GhcPs)) -> LHsExpr GhcPs -> LHsExpr GhcPs
descendBracket' op x = descendIndex' g x
    where
        g i y = if a then f i b else b
            where (a, b) = op y
        f i y@(L _ e) | needBracket' i x y = addParen' y
        f _ y = y

-- Add brackets as suggested 'needBracket' at 1-level of depth.
rebracket1' :: LHsExpr GhcPs -> LHsExpr GhcPs
rebracket1' = descendBracket' (True, )

-- A list of application, with any necessary brackets.
appsBracket' :: [LHsExpr GhcPs] -> LHsExpr GhcPs
appsBracket' = foldl1 mkApp
  where mkApp x y = rebracket1' (noLoc $ HsApp noExt x y)


simplifyExp' :: LHsExpr GhcPs -> LHsExpr GhcPs
-- Replace appliciations 'f $ x' with 'f (x)'.
simplifyExp' (L l (OpApp _ x op y)) | isDol op = L l (HsApp noExt x (noLoc (HsPar noExt y)))
simplifyExp' e@(L _ (HsLet _ (L _ (HsValBinds _ (ValBinds _ binds []))) z)) =
  -- An expression of the form, 'let x = y in z'.
  case bagToList binds of
    [L _ (FunBind _ _(MG _ (L _ [L _ (Match _(FunRhs (L _ x) _ _) [] (GRHSs _[L _ (GRHS _ [] y)] (L _ (EmptyLocalBinds _))))]) _) _ _)]
         -- If 'x' is not in the free variables of 'y', beta-reduce to
         -- 'z[(y)/x]'.
      | occNameString (rdrNameOcc x) `notElem` vars' y && length [() | Unqual a <- universeBi z, a == rdrNameOcc x] <= 1 ->
          transform f z
          where f (view' -> Var_' x') | occNameString (rdrNameOcc x) == x' = paren' y
                f x = x
    _ -> e
simplifyExp' e = e

-- Rewrite '($) . b' as 'b'.
niceDotApp' :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
niceDotApp' (L _ (HsVar _ (L _ r))) b | occNameString (rdrNameOcc r) == "$" = b
niceDotApp' a b = dotApp' a b


-- Generate a lambda expression but prettier if possible.
niceLambda' :: [String] -> LHsExpr GhcPs -> LHsExpr GhcPs
niceLambda' ss e = fst (niceLambdaR' ss e)-- We don't support refactorings yet.

allowRightSection :: String -> Bool
allowRightSection x = x `notElem` ["-","#"]
allowLeftSection :: String -> Bool
allowLeftSection x = x /= "#"

-- Implementation. Try to produce special forms (e.g. sections,
-- compositions) where we can.
niceLambdaR' :: [String]
             -> LHsExpr GhcPs
             -> (LHsExpr GhcPs, R.SrcSpan
             -> [Refactoring R.SrcSpan])
-- Rewrite @\ -> e@ as @e@
-- These are encountered as recursive calls.
niceLambdaR' xs (SimpleLambda [] x) = niceLambdaR' xs x

-- Rewrite @\xs -> (e)@ as @\xs -> e@.
niceLambdaR' xs (L _ (HsPar _ x)) = niceLambdaR' xs x

-- @\vs v -> ($) e v@ ==> @\vs -> e@
-- @\vs v -> e $ v@ ==> @\vs -> e@
niceLambdaR' (unsnoc -> Just (vs, v)) (view' -> App2' f e (view' -> Var_' v'))
  | isDol f
  , v == v'
  , vars' e `disjoint` [v]
  = niceLambdaR' vs e

-- @\v -> thing + v@ ==> @\v -> (thing +)@  (heuristic: @v@ must be a single
-- lexeme, or it all gets too complex)
niceLambdaR' [v] (L _ (OpApp _ e f (view' -> Var_' v')))
  | isLexeme e
  , v == v'
  , vars' e `disjoint` [v]
  , L _ (HsVar _ (L _ fname)) <- f
  , isSymOcc $ rdrNameOcc fname
  = (noLoc $ HsPar noExt $ noLoc $ SectionL noExt e f, \s -> [Replace Expr s [] (unsafePrettyPrint e)])

-- @\vs v -> f x v@ ==> @\vs -> f x@
niceLambdaR' (unsnoc -> Just (vs, v)) (L _ (HsApp _ (L _ (HsApp _ f e)) (view' -> Var_' v')))
  | v == v'
  , vars' e `disjoint` [v]
  = niceLambdaR' vs $ apps' [f, e]

-- @\vs v -> (v `f`)@ ==> @\vs -> f@
niceLambdaR' (unsnoc -> Just (vs, v)) (L _ (SectionL _ (view' -> Var_' v') f))
  | v == v' = niceLambdaR' vs f

-- Strip one variable pattern from the end of a lambdas match, and place it in our list of factoring variables.
niceLambdaR' xs (SimpleLambda ((view' -> PVar_' v):vs) x)
  | v `notElem` xs = niceLambdaR' (xs++[v]) $ lambda vs x

-- Rewrite @\x -> x + a@ as @(+ a)@ (heuristic: @a@ must be a single
-- lexeme, or it all gets too complex).
niceLambdaR' [x] (view' -> App2' op@(L _ (HsVar _ (L _ tag))) l r)
  | isLexeme r, view' l == Var_' x, x `notElem` vars' r, allowRightSection (occNameString $ rdrNameOcc tag) =
      let e = rebracket1' $ addParen' (noLoc $ SectionR noExt op r)
      in (e, \s -> [Replace Expr s [] (unsafePrettyPrint e)])
-- Rewrite (1) @\x -> f (b x)@ as @f . b@, (2) @\x -> f $ b x@ as @f . b@.
niceLambdaR' [x] y
  | Just (z, subts) <- factor y, x `notElem` vars' z = (z, \s -> [mkRefact subts s])
  where
    -- Factor the expression with respect to x.
    factor :: LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, [LHsExpr GhcPs])
    factor y@(L _ (HsApp _ ini lst)) | view' lst == Var_' x = Just (ini, [ini])
    factor y@(L _ (HsApp _ ini lst)) | Just (z, ss) <- factor lst
      = let r = niceDotApp' ini z
        in if astEq r z then Just (r, ss) else Just (r, ini : ss)
    factor (L _ (OpApp _ y op (factor -> Just (z, ss))))| isDol op
      = let r = niceDotApp' y z
        in if astEq r z then Just (r, ss) else Just (r, y : ss)
    factor (L _ (HsPar _ y@(L _ HsApp{}))) = factor y
    factor _ = Nothing
    mkRefact :: [LHsExpr GhcPs] -> R.SrcSpan -> Refactoring R.SrcSpan
    mkRefact subts s =
      let tempSubts = zipWith (\a b -> ([a], toSS' b)) ['a' .. 'z'] subts
          template = dotApps' (map (strToVar . fst) tempSubts)
      in Replace Expr s tempSubts (unsafePrettyPrint template)
-- Rewrite @\x y -> x + y@ as @(+)@.
niceLambdaR' [x,y] (L _ (OpApp _ (view' -> Var_' x1) op@(L _ HsVar {}) (view' -> Var_' y1)))
    | x == x1, y == y1, vars' op `disjoint` [x, y] = (op, \s -> [Replace Expr s [] (unsafePrettyPrint op)])
-- Rewrite @\x y -> f y x@ as @flip f@.
niceLambdaR' [x, y] (view' -> App2' op (view' -> Var_' y1) (view' -> Var_' x1))
  | x == x1, y == y1, vars' op `disjoint` [x, y] =
      ( gen op
      , \s -> [Replace Expr s [("x", toSS' op)] (unsafePrettyPrint $ gen (strToVar "x"))]
      )
  where
    gen = noLoc . HsApp noExt (strToVar "flip")

-- We're done factoring, but have no variables left, so we shouldn't make a lambda.
-- @\ -> e@ ==> @e@
niceLambdaR' [] e = (e, const [])
-- Base case. Just a good old fashioned lambda.
niceLambdaR' ss e =
  let grhs = noLoc $ GRHS noExt [] e :: LGRHS GhcPs (LHsExpr GhcPs)
      grhss = GRHSs {grhssExt = noExt, grhssGRHSs=[grhs], grhssLocalBinds=noLoc $ EmptyLocalBinds noExt}
      match = noLoc $ Match {m_ext=noExt, m_ctxt=LambdaExpr, m_pats=map strToPat ss, m_grhss=grhss} :: LMatch GhcPs (LHsExpr GhcPs)
      matchGroup = MG {mg_ext=noExt, mg_origin=Generated, mg_alts=noLoc [match]}
  in (noLoc $ HsLam noExt matchGroup, const [])


-- 'case' and 'if' expressions have branches, nothing else does (this
-- doesn't consider 'HsMultiIf' perhaps it should?).
replaceBranches' :: LHsExpr GhcPs -> ([LHsExpr GhcPs], [LHsExpr GhcPs] -> LHsExpr GhcPs)
replaceBranches' (L l (HsIf _ _ a b c)) = ([b, c], \[b, c] -> cL l (HsIf noExt Nothing a b c))

replaceBranches' (L s (HsCase _ a (MG _ (L l bs) FromSource))) =
  (concatMap f bs, \xs -> cL s (HsCase noExt a (MG noExt (cL l (g bs xs)) Generated)))
  where
    f :: LMatch GhcPs (LHsExpr GhcPs) -> [LHsExpr GhcPs]
    f (L _ (Match _ CaseAlt _ (GRHSs _ xs _))) = [x | (L _ (GRHS _ _ x)) <- xs]
    f _ = error "GHC.Util.HsExpr.replaceBranches: unexpected XMatch"

    g :: [LMatch GhcPs (LHsExpr GhcPs)] -> [LHsExpr GhcPs] -> [LMatch GhcPs (LHsExpr GhcPs)]
    g (L s1 (Match _ CaseAlt a (GRHSs _ ns b)) : rest) xs =
      cL s1 (Match noExt CaseAlt a (GRHSs noExt [cL a (GRHS noExt gs x) | (L a (GRHS _ gs _), x) <- zip ns as] b)) : g rest bs
      where  (as, bs) = splitAt (length ns) xs
    g [] [] = []
    g _ _ = error "GHC.Util.HsExpr.replaceBranches': internal invariant failed, lists are of differing lengths"

replaceBranches' x = ([], \[] -> x)


-- Like needBracket', but with a special case for 'a . b . b', which was
-- removed from haskell-src-exts-util-0.2.2.
needBracketOld' :: Int -> LHsExpr GhcPs -> LHsExpr GhcPs -> Bool
needBracketOld' i parent child
  | isDotApp parent, isDotApp child, i == 2 = False
  | otherwise = needBracket' i parent child

transformBracketOld' :: (LHsExpr GhcPs -> Maybe (LHsExpr GhcPs)) -> LHsExpr GhcPs -> (LHsExpr GhcPs, LHsExpr GhcPs)
transformBracketOld' op = first snd . g
  where
    g = first f . descendBracketOld' g
    f x = maybe (False, x) (True, ) (op x)

-- Descend, and if something changes then add/remove brackets
-- appropriately. Returns (suggested replacement, refactor template).
-- Whenever a bracket is added to the suggested replacement, a
-- corresponding bracket is added to the refactor template.
descendBracketOld' :: (LHsExpr GhcPs -> ((Bool, LHsExpr GhcPs), LHsExpr GhcPs))
                   -> LHsExpr GhcPs
                   -> (LHsExpr GhcPs, LHsExpr GhcPs)
descendBracketOld' op x = (descendIndex' g1 x, descendIndex' g2 x)
  where
    g i y = if a then (f1 i b z, f2 i b z) else (b, z)
      where ((a, b), z) = op y

    g1 = (fst .) . g
    g2 = (snd .) . g

    f i (L _ (HsPar _ y)) z | not $ needBracketOld' i x y = (y, y `ifNoLocElse` z)
    f i y z                 | needBracketOld' i x y = (addParen' y, addParen' (y `ifNoLocElse` z))
    f _ y z                 = (y, y `ifNoLocElse` z)

    f1 = ((fst .) .) . f
    f2 = ((snd .) .) . f

    ifNoLocElse y z = if getLoc y == noSrcSpan then y else z

fromParen1' :: LHsExpr GhcPs -> LHsExpr GhcPs
fromParen1' (L _ (HsPar _ x)) = x
fromParen1' x = x

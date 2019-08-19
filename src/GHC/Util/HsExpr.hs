{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns, MultiParamTypeClasses , FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

 --  Keep until 'isDotApp', 'descendApps', 'transformApps' and
 -- 'allowLeftSection' are used.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module GHC.Util.HsExpr (
    noSyntaxExpr'
  , isDol', isDot', isAtom', isSection', isApp', isAnyApp', isForD', isLexeme', isReturn'
  , addParen', dotApp'
  , simplifyExp', niceLambda', niceDotApp'
  , rebracket1', appsBracket', transformAppsM', fromApps', apps', universeApps'
  , varToStr', strToVar'
) where

import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" BasicTypes
import "ghc-lib-parser" SrcLoc
import "ghc-lib-parser" FastString
import "ghc-lib-parser" RdrName
import "ghc-lib-parser" OccName
import "ghc-lib-parser" Bag(bagToList)
import "ghc-lib-parser" TysWiredIn
import "ghc-lib-parser" TcEvidence
import "ghc-lib-parser" Name

import GHC.Util.View
import GHC.Util.FreeVars
import GHC.Util.W
import GHC.Util.Pat

import Control.Applicative
import Control.Monad.Trans.State

import Data.Data
import Data.Generics.Uniplate.Data
import Data.List.Extra

import Refact.Types hiding (Match)
import qualified Refact.Types as R (SrcSpan)

--

noSyntaxExpr' :: SyntaxExpr GhcPs
noSyntaxExpr' =
  SyntaxExpr
    (HsLit noExt
    (HsString NoSourceText (fsLit "noSyntaxExpr")))
    [] WpHole

-- | 'dotApp a b' makes 'a . b'.
dotApp' :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
dotApp' x y = noLoc $ OpApp noExt x (noLoc $ HsVar noExt (noLoc $ mkVarUnqual (fsLit "."))) y

-- | 'addParen e' wraps 'e' in parens.
addParen' :: LHsExpr GhcPs -> LHsExpr GhcPs
addParen' e = noLoc $ HsPar noExt e

-- | 'paren e' wraps 'e' in parens if 'e' is non-atomic.
paren' :: LHsExpr GhcPs -> LHsExpr GhcPs
paren' x
  | isAtom' (unLoc x)  = x
  | otherwise = addParen' x

-- | 'isApp e' if 'e' is a (regular) application.
isApp' :: HsExpr GhcPs -> Bool
isApp' x = case x of
  HsApp {}  -> True
  _         -> False

-- | 'isOpApp e' if 'e' is an operator application.
isOpApp' :: HsExpr GhcPs -> Bool
isOpApp' x = case x of
  OpApp {}   -> True
  _          -> False

-- | 'isAnyApp e' if 'e' is either an application or operator
-- application.
isAnyApp' :: HsExpr GhcPs -> Bool
isAnyApp' x = isApp' x || isOpApp' x

-- | 'isDot e'  if 'e' is the unqualifed variable '.'.
isDot' :: HsExpr GhcPs -> Bool
isDot' (HsVar _ (L _ ident)) = ident == mkVarUnqual (fsLit ".")
isDot' _ = False

-- | 'isDol e'  if 'e' is the unqualifed variable '$'.
isDol' :: HsExpr GhcPs -> Bool
isDol' (HsVar _ (L _ ident)) = ident == mkVarUnqual (fsLit "$")
isDol' _ = False

-- | 'isSection e' if 'e' is a section.
isSection' :: HsExpr GhcPs -> Bool
isSection' x = case x of
  SectionL {} -> True
  SectionR {} -> True
  _           -> False

-- | 'isForD d' if 'd' is a foreign declaration.
isForD' :: HsDecl GhcPs -> Bool
isForD' ForD{} = True
isForD' _ = False

-- | 'isDotApp e' if 'e' is dot application.
isDotApp' :: HsExpr GhcPs -> Bool
isDotApp' (OpApp _ _ (dL -> L _ op) _) = isDot' op
isDotApp' _ = False

-- | 'isAtom e' if 'e' requires no bracketing ever.
isAtom' :: HsExpr GhcPs -> Bool
isAtom' x = case x of
  HsVar {}          -> True
  HsUnboundVar {}   -> True
  HsRecFld {}       -> True
  HsOverLabel {}    -> True
  HsIPVar {}        -> True
  HsPar {}          -> True
  SectionL {}       -> True
  SectionR {}       -> True
  ExplicitTuple {}  -> True
  ExplicitSum {}    -> True
  ExplicitList {}   -> True
  RecordCon {}      -> True
  RecordUpd {}      -> True
  ArithSeq {}       -> True
  HsBracket {}      -> True
  HsRnBracketOut {} -> True
  HsTcBracketOut {} -> True
  HsSpliceE {}      -> True
  HsLit _ x     | not $ isNegativeLit x     -> True
  HsOverLit _ x | not $ isNegativeOverLit x -> True
  _                 -> False
  where
    isNegativeLit (HsInt _ i) = il_neg i
    isNegativeLit (HsRat _ f _) = fl_neg f
    isNegativeLit (HsFloatPrim _ f) = fl_neg f
    isNegativeLit (HsDoublePrim _ f) = fl_neg f
    isNegativeLit (HsIntPrim _ x) = x < 0
    isNegativeLit (HsInt64Prim _ x) = x < 0
    isNegativeLit (HsInteger _ x _) = x < 0
    isNegativeLit _ = False

    isNegativeOverLit OverLit {ol_val=HsIntegral i} = il_neg i
    isNegativeOverLit OverLit {ol_val=HsFractional f} = fl_neg f
    isNegativeOverLit _ = False

isLexeme' :: HsExpr GhcPs -> Bool
isLexeme' HsVar{} = True
isLexeme' HsOverLit{} = True
isLexeme' HsLit{} = True
isLexeme' _ = False

-- Allow both pure and return, as they have the same semantics.
isReturn' :: LHsExpr GhcPs -> Bool
isReturn' (LL _ (HsVar _ (LL _ (Unqual x)))) = occNameString x == "return" || occNameString x == "pure"
isReturn' _ = False

--

apps' :: [LHsExpr GhcPs] -> LHsExpr GhcPs
apps' = foldl1' mkApp where mkApp x y = noLoc (HsApp noExt x y)

fromApps' :: LHsExpr GhcPs  -> [LHsExpr GhcPs]
fromApps' (LL _ (HsApp _ x y)) = fromApps' x ++ [y]
fromApps' x = [x]

childrenApps' :: LHsExpr GhcPs -> [LHsExpr GhcPs]
childrenApps' (LL _ (HsApp _ x y)) = childrenApps' x ++ [y]
childrenApps' x = children x

universeApps' :: LHsExpr GhcPs -> [LHsExpr GhcPs]
universeApps' x = x : concatMap universeApps' (childrenApps' x)

descendApps' :: (LHsExpr GhcPs -> LHsExpr GhcPs) -> LHsExpr GhcPs -> LHsExpr GhcPs
descendApps' f (LL l (HsApp _ x y)) = LL l $ HsApp noExt (descendApps' f x) (f y)
descendApps' f x = descend f x

descendAppsM' :: Monad m => (LHsExpr GhcPs  -> m (LHsExpr GhcPs)) -> LHsExpr GhcPs -> m (LHsExpr GhcPs)
descendAppsM' f (LL l (HsApp _ x y)) = liftA2 (\x y -> LL l $ HsApp noExt x y) (descendAppsM' f x) (f y)
descendAppsM' f x = descendM f x

transformApps' :: (LHsExpr GhcPs -> LHsExpr GhcPs) -> LHsExpr GhcPs -> LHsExpr GhcPs
transformApps' f = f . descendApps' (transformApps' f)

transformAppsM' :: Monad m => (LHsExpr GhcPs -> m (LHsExpr GhcPs)) -> LHsExpr GhcPs -> m (LHsExpr GhcPs)
transformAppsM' f x = f =<< descendAppsM' (transformAppsM' f) x

descendIndex' :: Data a => (Int -> a -> a) -> a -> a
descendIndex' f x = flip evalState 0 $ flip descendM x $ \y -> do
    i <- get
    modify (+1)
    return $ f i y

-- | Is the child safe free from brackets in the parent position. Err
-- on the side of caution, True = don't know
needBracket :: Int -> LHsExpr GhcPs -> LHsExpr GhcPs -> Bool
needBracket i (LL _ parent) (LL _ child) -- Note: i is the index in children, not in the AST.
 | isAtom' child = False
 | isSection' parent, HsApp{} <- child = False
 | OpApp{} <- parent, HsApp{} <- child = False
 | HsLet{} <- parent, HsApp{} <- child = False
 | HsDo{} <- parent = False
 | ExplicitList{} <- parent = False
 | ExplicitTuple{} <- parent = False
 | HsIf{} <- parent, isAnyApp' child = False
 | HsApp{} <- parent, i == 0, HsApp{} <- child = False
 | ExprWithTySig{} <- parent, i == 0, isApp' child = False
 | HsPar{} <- parent = False
 | RecordCon{} <- parent = False
 | RecordUpd{} <- parent, i /= 0 = False
 | HsCase{} <- parent, i /= 0 || isAnyApp' child = False
 | HsLam{} <- parent = False -- might be either the RHS of a PViewPat, or the lambda body (neither needs brackets)
 | otherwise = True

--  There are differences in pretty-printing between GHC and HSE. This
--  version never removes brackets.
descendBracket' :: (LHsExpr GhcPs -> (Bool, LHsExpr GhcPs)) -> LHsExpr GhcPs -> LHsExpr GhcPs
descendBracket' op x = descendIndex' g x
    where
        g i y = if a then f i b else b
            where (a, b) = op y
        f i y@(LL _ e) | needBracket i x y = addParen' y
        f _ y = y

-- Add brackets as suggested 'needBracket' at 1-level of depth.
rebracket1' :: LHsExpr GhcPs -> LHsExpr GhcPs
rebracket1' = descendBracket' (True, )

-- A list of application, with any necessary brackets.
appsBracket' :: [LHsExpr GhcPs] -> LHsExpr GhcPs
appsBracket' = foldl1 mkApp
  where mkApp x y = rebracket1' (noLoc $ HsApp noExt x y)

--

varToStr' :: LHsExpr GhcPs -> String
varToStr' (LL _ (HsVar _ (L _ n)))
  | n == consDataCon_RDR = ":"
  | n == nameRdrName nilDataConName = "[]"
  | n == nameRdrName (getName (tupleDataCon Boxed 0)) = "()"
  | otherwise = occNameString (rdrNameOcc n)
varToStr' _ = ""

strToVar' :: String -> LHsExpr GhcPs
strToVar' x = noLoc $ HsVar noExt (noLoc $ mkRdrUnqual (mkVarOcc x))

--

simplifyExp' :: LHsExpr GhcPs -> LHsExpr GhcPs
-- Replace appliciations 'f $ x' with 'f (x)'.
simplifyExp' (LL l (OpApp _ x (LL _ op) y)) | isDol' op = LL l (HsApp noExt x (noLoc (HsPar noExt y)))
simplifyExp' e@(LL _ (HsLet _ (LL _ (HsValBinds _ (ValBinds _ binds []))) z)) =
  -- An expression of the form, 'let x = y in z'.
  case bagToList binds of
    [LL _ (FunBind _ _(MG _ (LL _ [LL _ (Match _(FunRhs (LL _ x) _ _) [] (GRHSs _[LL _ (GRHS _ [] y)] (LL _ (EmptyLocalBinds _))))]) _) _ _)]
         -- If 'x' is not in the free variables of 'y', beta-reduce to
         -- 'z[(y)/x]'.
      | occNameString (rdrNameOcc x) `notElem` vars' y && length [() | Unqual a <- universeBi z, a == rdrNameOcc x] <= 1 ->
          transform f z
          where f (view' -> Var_' x') | occNameString (rdrNameOcc x) == x' = paren' y
                f x = x
    _ -> e
simplifyExp' e = e

-- ($) . b ==> b
niceDotApp' :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
niceDotApp' (LL _ (HsVar _ (L _ r))) b | occNameString (rdrNameOcc r) == "$" = b
niceDotApp' a b = dotApp' a b

--
-- Generate a lambda expression but prettier if possible.
niceLambda' :: [String] -> LHsExpr GhcPs -> LHsExpr GhcPs
niceLambda' ss e = fst (niceLambdaR' ss e)-- We don't support refactorings yet.

allowRightSection x = x `notElem` ["-","#"]
allowLeftSection x = x /= "#"

-- Implementation. Try to produce special forms (e.g. sections,
-- compositions) where we can.
niceLambdaR' :: [String]
             -> LHsExpr GhcPs
             -> (LHsExpr GhcPs, R.SrcSpan
             -> [Refactoring R.SrcSpan])
-- Rewrite '\xs -> (e)' as '\xs -> e'.
niceLambdaR' xs (LL _ (HsPar _ x)) = niceLambdaR' xs x
-- Rewrite '\x -> x + a' as '(+ a)' (heuristic: 'a' must be a single
-- lexeme, or it all gets too complex).
niceLambdaR' [x] (view' -> App2' op@(LL _ (HsVar _ (L _ tag))) l r)
  | isLexeme' (unLoc r), view' l == Var_' x, x `notElem` vars' r, allowRightSection (occNameString $ rdrNameOcc tag) =
      let e = rebracket1' $ addParen' (noLoc $ SectionR noExt op r)
      in (e, const [])
-- Rewrite (1) '\x -> f (b x)' as 'f . b', (2) '\x -> f $ b x' as 'f . b'.
niceLambdaR' [x] y
  | Just (z, subts) <- factor y, x `notElem` vars' z = (z, const [])
  where
    -- Factor the expression with respect to x.
    factor :: LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, [LHsExpr GhcPs])
    factor y@(LL _ (HsApp _ ini lst)) | view' lst == Var_' x = Just (ini, [ini])
    factor y@(LL _ (HsApp _ ini lst)) | Just (z, ss) <- factor lst
      = let r = niceDotApp' ini z
        in if eqLoc' r z then Just (r, ss) else Just (r, ini : ss)
    factor (LL _ (OpApp _ y op@(LL _ tag) (factor -> Just (z, ss))))| isDol' tag
      = let r = niceDotApp' y z
        in if eqLoc' r z then Just (r, ss) else Just (r, y : ss)
    factor (LL _ (HsPar _ y@(LL _ HsApp{}))) = factor y
    factor _ = Nothing
-- Rewrite '\x y -> x + y' as '(+)'.
niceLambdaR' [x,y] (LL _ (OpApp _ (view' -> Var_' x1) op@(LL _ HsVar {}) (view' -> Var_' y1)))
    | x == x1, y == y1, vars' op `disjoint` [x, y] = (op, const [])
-- Rewrite '\x y -> f y x' as 'flip f'.
niceLambdaR' [x, y] (view' -> App2' op (view' -> Var_' y1) (view' -> Var_' x1))
  | x == x1, y == y1, vars' op `disjoint` [x, y] = (noLoc $ HsApp noExt (strToVar' "flip") op, const [])
-- Base case. Just a good old fashioned lambda.
niceLambdaR' ss e =
  let grhs = noLoc $ GRHS noExt [] e :: LGRHS GhcPs (LHsExpr GhcPs)
      grhss = GRHSs {grhssExt = noExt, grhssGRHSs=[grhs], grhssLocalBinds=noLoc $ EmptyLocalBinds noExt}
      match = noLoc $ Match {m_ext=noExt, m_ctxt=LambdaExpr, m_pats=map strToPat ss, m_grhss=grhss} :: LMatch GhcPs (LHsExpr GhcPs)
      matchGroup = MG {mg_ext=noExt, mg_origin=Generated, mg_alts=noLoc [match]}
  in (noLoc $ HsLam noExt matchGroup, const [])

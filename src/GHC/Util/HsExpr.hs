{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns, MultiParamTypeClasses , FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

--  Keep until 'descendApps', 'transformApps' and 'allowLeftSection'
-- are used.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module GHC.Util.HsExpr (
    noSyntaxExpr'
  , isTag', isDol', isDot', isSection', isRecConstr', isRecUpdate', isVar', isPar', isApp', isAnyApp', isLexeme', isLambda', isQuasiQuote', isTypeApp', isWHNF',isReturn'
  , dotApp', dotApps'
  , simplifyExp', niceLambda', niceDotApp'
  , Brackets'(..)
  , rebracket1', appsBracket', transformAppsM', fromApps', apps', universeApps', universeParentExp'
  , varToStr', strToVar'
  , paren', fromChar'
  , replaceBranches'
  , needBracketOld', transformBracketOld', descendBracketOld', reduce', reduce1', fromParen1'
  , hasFieldsDotDot', isFieldPun'
) where

import HsSyn
import BasicTypes
import SrcLoc
import FastString
import RdrName
import OccName
import Bag(bagToList)
import TysWiredIn
import TcEvidence
import Name

import GHC.Util.Brackets
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


noSyntaxExpr' :: SyntaxExpr GhcPs
noSyntaxExpr' =
  SyntaxExpr
    (HsLit noExt
    (HsString NoSourceText (fsLit "noSyntaxExpr")))
    [] WpHole

-- | 'dotApp a b' makes 'a . b'.
dotApp' :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
dotApp' x y = noLoc $ OpApp noExt x (noLoc $ HsVar noExt (noLoc $ mkVarUnqual (fsLit "."))) y

dotApps' :: [LHsExpr GhcPs] -> LHsExpr GhcPs
dotApps' [] = error "GHC.Util.HsExpr.dotApps', does not work on an empty list"
dotApps' [x] = x
dotApps' (x : xs) = dotApp' x (dotApps' xs)

-- | 'paren e' wraps 'e' in parens if 'e' is non-atomic.
paren' :: LHsExpr GhcPs -> LHsExpr GhcPs
paren' x
  | isAtom' x  = x
  | otherwise = addParen' x

-- 'True' if the provided expression is a variable with name 'tag'.
isTag' :: LHsExpr GhcPs -> String -> Bool
isTag' (LL _ (HsVar _ (L _ s))) tag = occNameString (rdrNameOcc s) == tag
isTag' _ _ = False

isVar',isReturn',isLexeme',isLambda',isQuasiQuote',isTypeApp',isDotApp',isRecUpdate',isRecConstr',isDol',isDot' :: LHsExpr GhcPs -> Bool
isPar' (LL _ HsPar{}) = True; isPar' _ = False
isVar' (LL _ HsVar{}) = True; isVar' _ = False
isDot' x = isTag' x "."
isDol' x = isTag' x "$"
isReturn' x = isTag' x "return" || isTag' x "pure" -- Allow both 'pure' and 'return' as they have the same semantics.
isRecConstr' (LL _ RecordCon{}) = True; isRecConstr' _ = False
isRecUpdate' (LL _ RecordUpd{}) = True; isRecUpdate' _ = False
isDotApp' (LL _ (OpApp _ _ op _)) = isDot' op; isDotApp' _ = False
isLexeme' (LL _ HsVar{}) = True;isLexeme' (LL _ HsOverLit{}) = True;isLexeme' (LL _ HsLit{}) = True;isLexeme' _ = False
isTypeApp' (LL _ HsAppType{}) = True; isTypeApp' _ = False
isLambda' (LL _ HsLam{}) = True; isLambda' _ = False
isQuasiQuote' (LL _ (HsSpliceE _ HsQuasiQuote{})) = True; isQuasiQuote' _ = False

isWHNF' :: LHsExpr GhcPs -> Bool
isWHNF' (LL _ (HsVar _ (L _ x))) = isRdrDataCon x
isWHNF' (LL _ (HsLit _ x)) = case x of HsString{} -> False; HsInt{} -> False; HsRat{} -> False; _ -> True
isWHNF' (LL _ HsLam{}) = True
isWHNF' (LL _ ExplicitTuple{}) = True
isWHNF' (LL _ ExplicitList{}) = True
isWHNF' (LL _ (HsPar _ x)) = isWHNF' x
isWHNF' (LL _ (ExprWithTySig _ x _)) = isWHNF' x
-- Other (unknown) constructors may have bang patterns in them, so
-- approximate.
isWHNF' (LL _ (HsApp _ (LL _ (HsVar _ (L _ x))) _))| occNameString (rdrNameOcc x) `elem` ["Just", "Left", "Right"] = True
isWHNF' _ = False


-- Contains a '..' as in 'Foo{..}'
hasFieldsDotDot' :: HsRecFields GhcPs (LHsExpr GhcPs) -> Bool
hasFieldsDotDot' HsRecFields {rec_dotdot=Just _} = True
hasFieldsDotDot' _ = False

-- Field is punned e.g. '{foo}'.
isFieldPun' :: LHsRecField GhcPs (LHsExpr GhcPs) -> Bool
isFieldPun' (LL _ HsRecField {hsRecPun=True}) = True
isFieldPun' _ = False


universeParentExp' :: Data a => a -> [(Maybe (Int, LHsExpr GhcPs), LHsExpr GhcPs)]
universeParentExp' xs = concat [(Nothing, x) : f x | x <- childrenBi xs]
    where f p = concat [(Just (i,p), c) : f c | (i,c) <- zip [0..] $ children p]


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

--  There are differences in pretty-printing between GHC and HSE. This
--  version never removes brackets.
descendBracket' :: (LHsExpr GhcPs -> (Bool, LHsExpr GhcPs)) -> LHsExpr GhcPs -> LHsExpr GhcPs
descendBracket' op x = descendIndex' g x
    where
        g i y = if a then f i b else b
            where (a, b) = op y
        f i y@(LL _ e) | needBracket' i x y = addParen' y
        f _ y = y

-- Add brackets as suggested 'needBracket' at 1-level of depth.
rebracket1' :: LHsExpr GhcPs -> LHsExpr GhcPs
rebracket1' = descendBracket' (True, )

-- A list of application, with any necessary brackets.
appsBracket' :: [LHsExpr GhcPs] -> LHsExpr GhcPs
appsBracket' = foldl1 mkApp
  where mkApp x y = rebracket1' (noLoc $ HsApp noExt x y)


varToStr' :: LHsExpr GhcPs -> String
varToStr' (LL _ (HsVar _ (L _ n)))
  | n == consDataCon_RDR = ":"
  | n == nameRdrName nilDataConName = "[]"
  | n == nameRdrName (getName (tupleDataCon Boxed 0)) = "()"
  | otherwise = occNameString (rdrNameOcc n)
varToStr' _ = ""

strToVar' :: String -> LHsExpr GhcPs
strToVar' x = noLoc $ HsVar noExt (noLoc $ mkRdrUnqual (mkVarOcc x))


simplifyExp' :: LHsExpr GhcPs -> LHsExpr GhcPs
-- Replace appliciations 'f $ x' with 'f (x)'.
simplifyExp' (LL l (OpApp _ x op y)) | isDol' op = LL l (HsApp noExt x (noLoc (HsPar noExt y)))
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

-- Rewrite '($) . b' as 'b'.
niceDotApp' :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
niceDotApp' (LL _ (HsVar _ (L _ r))) b | occNameString (rdrNameOcc r) == "$" = b
niceDotApp' a b = dotApp' a b


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
  | isLexeme' r, view' l == Var_' x, x `notElem` vars' r, allowRightSection (occNameString $ rdrNameOcc tag) =
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
    factor (LL _ (OpApp _ y op (factor -> Just (z, ss))))| isDol' op
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
      match = noLoc $ Match {m_ext=noExt, m_ctxt=LambdaExpr, m_pats=map strToPat' ss, m_grhss=grhss} :: LMatch GhcPs (LHsExpr GhcPs)
      matchGroup = MG {mg_ext=noExt, mg_origin=Generated, mg_alts=noLoc [match]}
  in (noLoc $ HsLam noExt matchGroup, const [])


fromChar' :: LHsExpr GhcPs -> Maybe Char
fromChar' (LL _ (HsLit _ (HsChar _ x))) = Just x
fromChar' _ = Nothing


-- 'case' and 'if' expressions have branches, nothing else does (this
-- doesn't consider 'HsMultiIf' perhaps it should?).
replaceBranches' :: LHsExpr GhcPs -> ([LHsExpr GhcPs], [LHsExpr GhcPs] -> LHsExpr GhcPs)
replaceBranches' (LL l (HsIf _ _ a b c)) = ([b, c], \[b, c] -> cL l (HsIf noExt Nothing a b c))

replaceBranches' (LL s (HsCase _ a (MG _ (L l bs) FromSource))) =
  (concatMap f bs, \xs -> cL s (HsCase noExt a (MG noExt (cL l (g bs xs)) Generated)))
  where
    f :: LMatch GhcPs (LHsExpr GhcPs) -> [LHsExpr GhcPs]
    f (LL _ (Match _ CaseAlt _ (GRHSs _ xs _))) = [x | (LL _ (GRHS _ _ x)) <- xs]
    f _ = undefined -- {-# COMPLETE LL #-}

    g :: [LMatch GhcPs (LHsExpr GhcPs)] -> [LHsExpr GhcPs] -> [LMatch GhcPs (LHsExpr GhcPs)]
    g (LL s1 (Match _ CaseAlt a (GRHSs _ ns b)) : rest) xs =
      cL s1 (Match noExt CaseAlt a (GRHSs noExt [cL a (GRHS noExt gs x) | (LL a (GRHS _ gs _), x) <- zip ns as] b)) : g rest bs
      where  (as, bs) = splitAt (length ns) xs
    g [] [] = []
    g _ _ = error "GHC.Util.HsExpr.replaceBranches': internal invariant failed, lists are of differing lengths"

replaceBranches' x = ([], \[] -> x)


-- Like needBracket, but with a special case for 'a . b . b', which was
-- removed from haskell-src-exts-util-0.2.2.
needBracketOld' :: Int -> LHsExpr GhcPs -> LHsExpr GhcPs -> Bool
needBracketOld' i parent child
  | isDotApp' parent, isDotApp' child, i == 2 = False
  | otherwise = needBracket' i parent child

transformBracketOld' :: (LHsExpr GhcPs -> Maybe (LHsExpr GhcPs)) -> LHsExpr GhcPs -> LHsExpr GhcPs
transformBracketOld' op = snd . g
  where
    g = f . descendBracketOld' g
    f x = maybe (False, x) (True, ) (op x)

-- Descend, and if something changes then add/remove brackets
-- appropriately
descendBracketOld' :: (LHsExpr GhcPs -> (Bool, LHsExpr GhcPs)) -> LHsExpr GhcPs -> LHsExpr GhcPs
descendBracketOld' op x = descendIndex' g x
  where
    g i y = if a then f i b else b
      where (a, b) = op y

    f i (LL _ (HsPar _ y)) | not $ needBracketOld' i x y = y
    f i y                  | needBracketOld' i x y = addParen' y
    f _ y                  = y

reduce' :: LHsExpr GhcPs -> LHsExpr GhcPs
reduce' = fromParen' . transform reduce1'

reduce1' :: LHsExpr GhcPs -> LHsExpr GhcPs
reduce1' (LL loc (HsApp _ len (LL _ (HsLit _ (HsString _ xs)))))
  | varToStr' len == "length" = cL loc $ HsLit noExt (HsInt noExt (IL NoSourceText False n))
  where n = fromIntegral $ length (unpackFS xs)
reduce1' (LL loc (HsApp _ len (LL _ (ExplicitList _ _ xs))))
  | varToStr' len == "length" = cL loc $ HsLit noExt (HsInt noExt (IL NoSourceText False n))
  where n = fromIntegral $ length xs
reduce1' (view' -> App2' op (LL _ (HsLit _ x)) (LL _ (HsLit _ y))) | varToStr' op == "==" = strToVar' (show (eqLoc' x y))
reduce1' (view' -> App2' op (LL _ (HsLit _ (HsInt _ x))) (LL _ (HsLit _ (HsInt _ y)))) | varToStr' op == ">=" = strToVar' $ show (x >= y)
reduce1' (view' -> App2' op x y)
    | varToStr' op == "&&" && varToStr' x == "True"  = y
    | varToStr' op == "&&" && varToStr' x == "False" = x
reduce1' (LL _ (HsPar _ x)) | isAtom' x = x
reduce1' x = x


fromParen1' :: LHsExpr GhcPs -> LHsExpr GhcPs
fromParen1' (LL _ (HsPar _ x)) = x
fromParen1' x = x

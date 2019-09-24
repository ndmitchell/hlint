{-# LANGUAGE ViewPatterns, PatternGuards, FlexibleContexts #-}
{-
    Find and match:

<TEST>
yes = 1:2:[] -- [1,2]
yes = ['h','e','l','l','o']
yes (1:2:[]) = 1 -- [1,2]
yes ['h','e'] = 1

-- [a]++b -> a : b, but only if not in a chain of ++'s
yes = [x] ++ xs -- x : xs
no = "x" ++ xs
no = [x] ++ xs ++ ys
no = xs ++ [x] ++ ys
yes = [if a then b else c] ++ xs -- (if a then b else c) : xs
yes = [1] : [2] : [3] : [4] : [5] : [] -- [[1], [2], [3], [4], [5]]
yes = if x == e then l2 ++ xs else [x] ++ check_elem xs -- x : check_elem xs
data Yes = Yes (Maybe [Char]) -- Maybe String
yes = y :: [Char] -> a -- String -> a
instance C [Char]
foo = [a b] ++ xs -- a b : xs
foo = [myexpr | True, a] -- [myexpr | a]
foo = [myexpr | False] -- []
foo = map f [x + 1 | x <- [1..10]] -- [f (x + 1) | x <- [1..10]]
foo = [x + 1 | x <- [1..10], feature] -- [x + 1 | feature, x <- [1..10]]
foo = [x + 1 | x <- [1..10], even x]
foo = [x + 1 | x <- [1..10], even x, dont_reoder_guards]
foo = [x + 1 | x <- [1..10], let y = even x, y]
foo = [x + 1 | x <- [1..10], let q = even 1, q] -- [x + 1 | let q = even 1, q, x <- [1..10]]
foo = [fooValue | Foo{..} <- y, fooField]
issue619 = [pkgJobs | Pkg{pkgGpd, pkgJobs} <- pkgs, not $ null $ C.condTestSuites pkgGpd]
</TEST>
-}

module Hint.List(listHint) where

import Control.Applicative
import Data.Generics.Uniplate.Operations
import Data.List.Extra
import Data.Maybe
import Prelude

import Hint.Type(DeclHint',Idea,suggest',toSS')

import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R

import HsSyn
import SrcLoc
import BasicTypes
import RdrName
import OccName
import FastString
import TysWiredIn

import GHC.Util

listHint :: DeclHint'
listHint _ _ = listDecl

listDecl :: LHsDecl GhcPs -> [Idea]
listDecl x =
  concatMap (listExp False) (childrenBi x) ++
  stringType x ++
  concatMap listPat (childrenBi x) ++
  concatMap listComp (universeBi x)

listComp :: LHsExpr GhcPs -> [Idea]
listComp o@(LL _ (HsDo _ ListComp (L _ stmts))) =
  let revs = reverse stmts
      e@(LL _ LastStmt{}) = head revs -- In a ListComp, this is always last.
      xs = reverse (tail revs) in
  list_comp_aux e xs
  where
    list_comp_aux e xs
      | "False" `elem` cons =  [suggest' "Short-circuited list comprehension" o o' (suggestExpr o o')]
      | "True" `elem` cons = [suggest' "Redundant True guards" o o2 (suggestExpr o o2)]
      | not (eqNoLocLists' xs ys) = [suggest' "Move guards forward" o o3 (suggestExpr o o3)]
      | otherwise = []
      where
        ys = moveGuardsForward xs
        o' = noLoc $ ExplicitList noExt Nothing []
        o2 = noLoc $ HsDo noExt ListComp (noLoc (filter ((/= Just "True") . qualCon) xs ++ [e]))
        o3 = noLoc $ HsDo noExt ListComp (noLoc $ ys ++ [e])
        cons = mapMaybe qualCon xs
        qualCon :: ExprLStmt GhcPs -> Maybe String
        qualCon (L _ (BodyStmt _ (LL _ (HsVar _ (L _ x))) _ _)) = Just (occNameString . rdrNameOcc $ x)
        qualCon _ = Nothing
listComp o@(view' -> App2' mp f (LL _ (HsDo _ ListComp (L _ stmts)))) | varToStr' mp == "map" =
    [suggest' "Move map inside list comprehension" o o2 (suggestExpr o o2)]
    where
      revs = reverse stmts
      LL _ (LastStmt _ body b s) = head revs -- In a ListComp, this is always last.
      last = noLoc $ LastStmt noExt (noLoc $ HsApp noExt (paren' f) (paren' body)) b s
      o2 =noLoc $ HsDo noExt ListComp (noLoc $ reverse (tail revs) ++ [last])
listComp _ = []

suggestExpr :: LHsExpr GhcPs -> LHsExpr GhcPs -> [Refactoring R.SrcSpan]
suggestExpr o o2 = [Replace Expr (toSS' o) [] (unsafePrettyPrint o2)]

moveGuardsForward :: [ExprLStmt GhcPs] -> [ExprLStmt GhcPs]
moveGuardsForward = reverse . f [] . reverse
  where
    f guards (x@(L _ (BindStmt _ p _ _ _)) : xs) = reverse stop ++ x : f move xs
      where (move, stop) =
              span (if any hasPFieldsDotDot' (universeBi x)
                       || any isPFieldWildcard' (universeBi x)
                      then const False
                      else \x -> pvars' p `disjoint` vars_ x) guards
    f guards (x@(L _ BodyStmt{}):xs) = f (x:guards) xs
    f guards (x@(L _ LetStmt{}):xs) = f (x:guards) xs
    f guards xs = reverse guards ++ xs

    -- Fake something that works
    vars_ x = [unsafePrettyPrint a | HsVar _ (LL _ a) <- universeBi x :: [HsExpr GhcPs]]

listExp :: Bool -> LHsExpr GhcPs -> [Idea]
listExp b (fromParen' -> x) =
  if null res then concatMap (listExp $ isAppend x) $ children x else [head res]
  where
    res = [suggest' name x x2 [r]
          | (name, f) <- checks
          , Just (x2, subts, temp) <- [f b x]
          , let r = Replace Expr (toSS' x) subts temp ]

listPat :: Pat GhcPs -> [Idea]
listPat x = if null res then concatMap listPat $ children x else [head res]
    where res = [suggest' name x x2 [r]
                  | (name, f) <- pchecks
                  , Just (x2, subts, temp) <- [f x]
                  , let r = Replace Pattern (toSS' x) subts temp ]
isAppend :: View' a App2' => a -> Bool
isAppend (view' -> App2' op _ _) = varToStr' op == "++"
isAppend _ = False

checks ::[(String, Bool -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, [(String, R.SrcSpan)], String))]
checks = let (*) = (,) in drop 1 -- see #174
  [ "Use string literal" * useString
  , "Use list literal" * useList
  , "Use :" * useCons
  ]

pchecks :: [(String, Pat GhcPs -> Maybe (Pat GhcPs, [(String, R.SrcSpan)], String))]
pchecks = let (*) = (,) in drop 1 -- see #174
    [ "Use string literal pattern" * usePString
    , "Use list literal pattern" * usePList
    ]

usePString :: Pat GhcPs -> Maybe (Pat GhcPs, [a], String)
usePString (LL _ (ListPat _ xs)) | not $ null xs, Just s <- mapM fromPChar' xs =
  let literal = noLoc $ LitPat noExt (HsString NoSourceText (fsLit (show s)))
  in Just (literal, [], unsafePrettyPrint literal)
usePString _ = Nothing

usePList :: Pat GhcPs -> Maybe (Pat GhcPs, [(String, R.SrcSpan)], String)
usePList =
  fmap  ( (\(e, s) ->
             (noLoc (ListPat noExt e)
             , map (fmap toSS') s
             , unsafePrettyPrint (noLoc $ ListPat noExt (map snd s) :: Pat GhcPs))
          )
          . unzip
        )
  . f True ['a'..'z']
  where
    f first _ x | patToStr' x == "[]" = if first then Nothing else Just []
    f first (ident:cs) (view' -> PApp_' ":" [a, b]) = ((a, g ident a) :) <$> f False cs b
    f first _ _ = Nothing

    g :: Char -> Pat GhcPs -> (String, Pat GhcPs)
    g c p = ([c], VarPat noExt (noLoc $ mkVarUnqual (fsLit [c])))

useString :: p -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, [a], String)
useString b (LL _ (ExplicitList _ _ xs)) | not $ null xs, Just s <- mapM fromChar' xs =
  let literal = noLoc (HsLit noExt (HsString NoSourceText (fsLit (show s))))
  in Just (literal, [], unsafePrettyPrint literal)
useString _ _ = Nothing

useList :: p -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, [(String, R.SrcSpan)], String)
useList b =
  fmap  ( (\(e, s) ->
             (noLoc (ExplicitList noExt Nothing e)
             , map (fmap toSS') s
             , unsafePrettyPrint (noLoc $ ExplicitList noExt Nothing (map snd s) :: LHsExpr GhcPs))
          )
          . unzip
        )
  . f True ['a'..'z']
  where
    f first _ x | varToStr' x == "[]" = if first then Nothing else Just []
    f first (ident:cs) (view' -> App2' c a b) | varToStr' c == ":" =
          ((a, g ident a) :) <$> f False cs b
    f first _ _ = Nothing

    g :: Char -> LHsExpr GhcPs -> (String, LHsExpr GhcPs)
    g c p = ([c], strToVar' [c])

useCons :: View' a App2' => Bool -> a -> Maybe (LHsExpr GhcPs, [(String, R.SrcSpan)], String)
useCons False (view' -> App2' op x y) | varToStr' op == "++"
                                       , Just (x2, build) <- f x
                                       , not $ isAppend y =
    Just (gen (build x2) y
         , [("x", toSS' x2), ("xs", toSS' y)]
         , unsafePrettyPrint $ gen (build $ strToVar' "x") (strToVar' "xs")
         )
  where
    f :: LHsExpr GhcPs ->
      Maybe (LHsExpr GhcPs, LHsExpr GhcPs -> LHsExpr GhcPs)
    f (LL _ (ExplicitList _ _ [x]))=
      Just (x, \v -> if isApp' x then v else paren' v)
    f _ = Nothing

    gen :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
    gen x = noLoc . OpApp noExt x (noLoc (HsVar noExt  (noLoc consDataCon_RDR)))
useCons _ _ = Nothing

typeListChar :: LHsType GhcPs
typeListChar =
  noLoc $ HsListTy noExt
    (noLoc (HsTyVar noExt NotPromoted (noLoc (mkVarUnqual (fsLit "Char")))))

typeString :: LHsType GhcPs
typeString =
  noLoc $ HsTyVar noExt NotPromoted (noLoc (mkVarUnqual (fsLit "String")))

stringType :: LHsDecl GhcPs  -> [Idea]
stringType (LL _ x) = case x of
  InstD _ ClsInstD{
    cid_inst=
        ClsInstDecl{cid_binds=x, cid_tyfam_insts=y, cid_datafam_insts=z}} ->
    f x ++ f y ++ f z -- Pretty much everthing but the instance type.
  _ -> f x
  where
    f x = concatMap g $ childrenBi x

    g :: LHsType GhcPs -> [Idea]
    g e@(fromTyParen' -> x) = [suggest' "Use String" x (transform f x)
                              rs | not . null $ rs]
      where f x = if eqNoLoc' x typeListChar then typeString else x
            rs = [Replace Type (toSS' t) [] (unsafePrettyPrint typeString) | t <- universe x, eqNoLoc' t typeListChar]
stringType _ = [] -- {-# COMPLETE LL #-}

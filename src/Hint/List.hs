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
{-# LANGUAGE MonadComprehensions #-}\
foo = [x | False, x <- [1 .. 10]] -- []
foo = [_ | x <- _, let _ = A{x}]
issue1039 = foo (map f [1 | _ <- []]) -- [f 1 | _ <- []]
{-# LANGUAGE OverloadedLists #-} \
issue114 = True:[]
</TEST>
-}

module Hint.List(listHint) where

import Control.Applicative
import Data.Generics.Uniplate.DataOnly
import Data.List.Extra
import Data.Maybe
import Prelude

import Hint.Type(DeclHint,Idea,suggest,ignore,substVars,toRefactSrcSpan,toSSA,modComments)

import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R

import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Types.Basic hiding (Pattern)
import GHC.Types.SourceText
import GHC.Types.Name.Reader
import GHC.Data.FastString
import GHC.Builtin.Types

import GHC.Util
import Language.Haskell.GhclibParserEx.GHC.Hs.Pat
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Hs.Type
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader


listHint :: DeclHint
listHint _ modu = listDecl overloadedListsOn
  where
    exts = concatMap snd (languagePragmas (pragmas (modComments modu)))
    overloadedListsOn = "OverloadedLists" `elem` exts

listDecl :: Bool -> LHsDecl GhcPs -> [Idea]
listDecl overloadedListsOn x =
  concatMap (listExp overloadedListsOn False) (childrenBi x) ++
  stringType x ++
  concatMap listPat (childrenBi x) ++
  concatMap listComp (universeBi x)

-- Refer to https://github.com/ndmitchell/hlint/issues/775 for the
-- structure of 'listComp'.

listComp :: LHsExpr GhcPs -> [Idea]
listComp o@(L _ (HsDo _ ListComp (L _ stmts))) =
  listCompCheckGuards o ListComp stmts
listComp o@(L _ (HsDo _ MonadComp (L _ stmts))) =
  listCompCheckGuards o MonadComp stmts
listComp (L _ HsPar{}) = [] -- App2 "sees through" paren, which causes duplicate hints with universeBi
listComp o@(view -> App2 mp f (L _ (HsDo _ ListComp (L _ stmts)))) =
  listCompCheckMap o mp f ListComp stmts
listComp o@(view -> App2 mp f (L _ (HsDo _ MonadComp (L _ stmts)))) =
  listCompCheckMap o mp f MonadComp stmts
listComp _ = []

listCompCheckGuards :: LHsExpr GhcPs -> HsDoFlavour -> [ExprLStmt GhcPs] -> [Idea]
listCompCheckGuards o ctx stmts =
  let revs = reverse stmts
      e@(L _ LastStmt{}) = head revs -- In a ListComp, this is always last.
      xs = reverse (tail revs) in
  list_comp_aux e xs
  where
    list_comp_aux e xs
      | "False" `elem` cons =  [suggest "Short-circuited list comprehension" (reLoc o) (reLoc o') (suggestExpr o o')]
      | "True" `elem` cons = [suggest "Redundant True guards" (reLoc o) (reLoc o2) (suggestExpr o o2)]
      | not (astListEq xs ys) = [suggest "Move guards forward" (reLoc o) (reLoc o3) (suggestExpr o o3)]
      | otherwise = []
      where
        ys = moveGuardsForward xs
        o' = noLocA $ ExplicitList EpAnnNotUsed []
        o2 = noLocA $ HsDo EpAnnNotUsed ctx (noLocA (filter ((/= Just "True") . qualCon) xs ++ [e]))
        o3 = noLocA $ HsDo EpAnnNotUsed ctx (noLocA $ ys ++ [e])
        cons = mapMaybe qualCon xs
        qualCon :: ExprLStmt GhcPs -> Maybe String
        qualCon (L _ (BodyStmt _ (L _ (HsVar _ (L _ x))) _ _)) = Just (occNameStr x)
        qualCon _ = Nothing

listCompCheckMap ::
  LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs -> HsDoFlavour -> [ExprLStmt GhcPs] -> [Idea]
listCompCheckMap o mp f ctx stmts  | varToStr mp == "map" =
    [suggest "Move map inside list comprehension" (reLoc o) (reLoc o2) (suggestExpr o o2)]
    where
      revs = reverse stmts
      L _ (LastStmt _ body b s) = head revs -- In a ListComp, this is always last.
      last = noLocA $ LastStmt noExtField (noLocA $ HsApp EpAnnNotUsed (paren f) (paren body)) b s
      o2 =noLocA $ HsDo EpAnnNotUsed ctx (noLocA $ reverse (tail revs) ++ [last])
listCompCheckMap _ _ _ _ _ = []

suggestExpr :: LHsExpr GhcPs -> LHsExpr GhcPs -> [Refactoring R.SrcSpan]
suggestExpr o o2 = [Replace Expr (toSSA o) [] (unsafePrettyPrint o2)]

moveGuardsForward :: [ExprLStmt GhcPs] -> [ExprLStmt GhcPs]
moveGuardsForward = reverse . f [] . reverse
  where
    f guards (x@(L _ (BindStmt _ p _)) : xs) = reverse stop ++ x : f move xs
      where (move, stop) =
              span (if any hasPFieldsDotDot (universeBi x)
                       || any isPFieldWildcard (universeBi x)
                      then const False
                      else \x ->
                       let pvs = pvars p in
                       -- See this code from 'RdrHsSyn.hs' (8.10.1):
                       --   plus_RDR, pun_RDR :: RdrName
                       --   plus_RDR = mkUnqual varName (fsLit "+") -- Hack
                       --   pun_RDR  = mkUnqual varName (fsLit "pun-right-hand-side")
                       -- Todo (SF, 2020-03-28): Try to make this better somehow.
                       pvs `disjoint` varss x && "pun-right-hand-side" `notElem` pvs
                   ) guards
    f guards (x@(L _ BodyStmt{}):xs) = f (x:guards) xs
    f guards (x@(L _ LetStmt{}):xs) = f (x:guards) xs
    f guards xs = reverse guards ++ xs

listExp :: Bool -> Bool -> LHsExpr GhcPs -> [Idea]
listExp overloadedListsOn b (fromParen -> x) =
  if null res
    then concatMap (listExp overloadedListsOn $ isAppend x) $ children x
    else [head res]
  where
    res = [suggest name (reLoc x) (reLoc x2) [r]
          | (name, f) <- checks overloadedListsOn
          , Just (x2, subts, temp) <- [f b x]
          , let r = Replace Expr (toSSA x) subts temp ]

listPat :: LPat GhcPs -> [Idea]
listPat x = if null res then concatMap listPat $ children x else [head res]
    where res = [suggest name (reLoc x) (reLoc x2) [r]
                  | (name, f) <- pchecks
                  , Just (x2, subts, temp) <- [f x]
                  , let r = Replace Pattern (toSSA x) subts temp ]

isAppend :: View a App2 => a -> Bool
isAppend (view -> App2 op _ _) = varToStr op == "++"
isAppend _ = False

checks :: Bool -> [(String, Bool -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, [(String, R.SrcSpan)], String))]
checks overloadedListsOn = let (*) = (,) in drop1 -- see #174
  [ "Use string literal" * useString
  , "Use :" * useCons
  ]
  <> ["Use list literal" * useList | not overloadedListsOn ] -- see #114

pchecks :: [(String, LPat GhcPs -> Maybe (LPat GhcPs, [(String, R.SrcSpan)], String))]
pchecks = let (*) = (,) in drop1 -- see #174
    [ "Use string literal pattern" * usePString
    , "Use list literal pattern" * usePList
    ]

usePString :: LPat GhcPs -> Maybe (LPat GhcPs, [a], String)
usePString (L _ (ListPat _ xs)) | not $ null xs, Just s <- mapM fromPChar xs =
  let literal = noLocA $ LitPat noExtField (HsString NoSourceText (fsLit (show s))) :: LPat GhcPs
  in Just (literal, [], unsafePrettyPrint literal)
usePString _ = Nothing

usePList :: LPat GhcPs -> Maybe (LPat GhcPs, [(String, R.SrcSpan)], String)
usePList =
  fmap  ( (\(e, s) ->
             (noLocA (ListPat EpAnnNotUsed e)
             , map (fmap toRefactSrcSpan . fst) s
             , unsafePrettyPrint (noLocA $ ListPat EpAnnNotUsed (map snd s) :: LPat GhcPs))
          )
          . unzip
        )
  . f True substVars
  where
    f first _ x | patToStr x == "[]" = if first then Nothing else Just []
    f first (ident:cs) (view -> PApp_ ":" [a, b]) = ((a, g ident a) :) <$> f False cs b
    f first _ _ = Nothing

    g :: String -> LPat GhcPs -> ((String, SrcSpan), LPat GhcPs)
    g s (locA . getLoc -> loc) = ((s, loc), noLocA $ VarPat noExtField (noLocA $ mkVarUnqual (fsLit s)))

useString :: p -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, [a], String)
useString b (L _ (ExplicitList _ xs)) | not $ null xs, Just s <- mapM fromChar xs =
  let literal = noLocA (HsLit EpAnnNotUsed (HsString NoSourceText (fsLit (show s)))) :: LHsExpr GhcPs
  in Just (literal, [], unsafePrettyPrint literal)
useString _ _ = Nothing

useList :: p -> LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, [(String, R.SrcSpan)], String)
useList b =
  fmap  ( (\(e, s) ->
             (noLocA (ExplicitList EpAnnNotUsed e)
             , map (fmap toSSA) s
             , unsafePrettyPrint (noLocA $ ExplicitList EpAnnNotUsed (map snd s) :: LHsExpr GhcPs))
          )
          . unzip
        )
  . f True substVars
  where
    f first _ x | varToStr x == "[]" = if first then Nothing else Just []
    f first (ident:cs) (view -> App2 c a b) | varToStr c == ":" =
          ((a, g ident a) :) <$> f False cs b
    f first _ _ = Nothing

    g :: String -> LHsExpr GhcPs -> (String, LHsExpr GhcPs)
    g s p = (s, L (getLoc p) (unLoc $ strToVar s))

useCons :: View a App2 => Bool -> a -> Maybe (LHsExpr GhcPs, [(String, R.SrcSpan)], String)
useCons False (view -> App2 op x y) | varToStr op == "++"
                                    , Just (newX, tplX, spanX) <- f x
                                    , not $ isAppend y =
    Just (gen newX y
         , [("x", spanX), ("xs", toSSA y)]
         , unsafePrettyPrint $ gen tplX (strToVar "xs")
         )
  where
    f :: LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, LHsExpr GhcPs, R.SrcSpan)
    f (L _ (ExplicitList _ [x]))
      | isAtom x || isApp x = Just (x, strToVar "x", toSSA x)
      | otherwise = Just (addParen x, addParen (strToVar "x"), toSSA x)
    f _ = Nothing

    gen :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
    gen x = noLocA . OpApp EpAnnNotUsed x (noLocA (HsVar noExtField  (noLocA consDataCon_RDR)))
useCons _ _ = Nothing

typeListChar :: LHsType GhcPs
typeListChar =
  noLocA $ HsListTy EpAnnNotUsed
    (noLocA (HsTyVar EpAnnNotUsed NotPromoted (noLocA (mkVarUnqual (fsLit "Char")))))

typeString :: LHsType GhcPs
typeString =
  noLocA $ HsTyVar EpAnnNotUsed NotPromoted (noLocA (mkVarUnqual (fsLit "String")))

stringType :: LHsDecl GhcPs  -> [Idea]
stringType (L _ x) = case x of
  InstD _ ClsInstD{
    cid_inst=
        ClsInstDecl{cid_binds=x, cid_tyfam_insts=y, cid_datafam_insts=z}} ->
    f x ++ f y ++ f z -- Pretty much everthing but the instance type.
  _ -> f x
  where
    f x = concatMap g $ childrenBi x

    g :: LHsType GhcPs -> [Idea]
    g e@(fromTyParen -> x) = [ignore "Use String" (reLoc x) (reLoc (transform f x))
                              rs | not . null $ rs]
      where f x = if astEq x typeListChar then typeString else x
            rs = [Replace Type (toSSA t) [] (unsafePrettyPrint typeString) | t <- universe x, astEq t typeListChar]

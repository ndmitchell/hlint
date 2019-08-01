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
import Hint.Type
import Data.List.Extra
import Data.Maybe
import Prelude
import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R


listHint :: DeclHint
listHint _ _ = listDecl

listDecl :: Decl_ -> [Idea]
listDecl x =
    concatMap (listExp False) (childrenBi x) ++
    stringType x ++
    concatMap listPat (childrenBi x) ++
    concatMap listComp (universeBi x)

listComp :: Exp_ -> [Idea]
listComp o@(ListComp a e xs)
    | "False" `elem` cons = [suggest "Short-circuited list comprehension" o o' (suggestExpr o o')]
    | "True" `elem` cons = [suggest "Redundant True guards" o o2 (suggestExpr o o2)]
    | xs /= ys = [suggest "Move guards forward" o o3 (suggestExpr o o3)]
    where
        ys = moveGuardsForward xs
        o' = List an []
        o2 = ListComp a e $ filter ((/= Just "True") . qualCon) xs
        o3 = ListComp a e ys
        cons = mapMaybe qualCon xs
        qualCon (QualStmt _ (Qualifier _ (Con _ x))) = Just $ fromNamed x
        qualCon _ = Nothing
listComp o@(view -> App2 mp f (ListComp a e xs)) | mp ~= "map" =
    [suggest "Move map inside list comprehension" o o2 (suggestExpr o o2)]
    where o2 = ListComp a (App an (paren f) (paren e)) xs
listComp _ = []

suggestExpr :: Exp_ -> Exp_ -> [Refactoring R.SrcSpan]
suggestExpr o o2 = [Replace Expr (toSS o) [] (prettyPrint o2)]

-- Move all the list comp guards as far forward as they can go
moveGuardsForward :: [QualStmt S] -> [QualStmt S]
moveGuardsForward = reverse . f [] . reverse
    where
        f guards (x@(QualStmt _ (Generator _ p _)):xs) = reverse stop ++ x : f move xs
            where (move, stop) = span (if any isPFieldWildcard (universeS x) then const False else \x -> pvars p `disjoint` vars' x) guards
        f guards (x@(QualStmt _ Qualifier{}):xs) = f (x:guards) xs
        f guards (x@(QualStmt _ LetStmt{}):xs) = f (x:guards) xs
        f guards xs = reverse guards ++ xs

        -- the type QualStmt doesn't have a Var instance, so fake something that works
        vars' x = [prettyPrint a | Var _ a <- universeS x]


-- boolean = are you in a ++ chain
listExp :: Bool -> Exp_ -> [Idea]
listExp b (fromParen -> x) =
        if null res then concatMap (listExp $ isAppend x) $ children x else [head res]
    where
        res = [suggest name x x2 [r] | (name,f) <- checks
                                  , Just (x2, subts, temp) <- [f b x]
                                  , let r = Replace Expr (toSS x) subts temp ]

listPat :: Pat_ -> [Idea]
listPat x = if null res then concatMap listPat $ children x else [head res]
    where res = [suggest name x x2 [r]
                  | (name,f) <- pchecks
                  , Just (x2, subts, temp) <- [f x]
                  , let r = Replace Pattern (toSS x) subts temp ]

isAppend :: View a App2 => a -> Bool
isAppend (view -> App2 op _ _) = op ~= "++"
isAppend _ = False


checks :: [(String, Bool -> Exp S -> Maybe (Exp SrcSpanInfo, [(String, R.SrcSpan)], String))]
checks = let (*) = (,) in drop 1 -- see #174
    ["Use string literal" * useString
    ,"Use list literal" * useList
    ,"Use :" * useCons
    ]

pchecks :: [(String, Pat S -> Maybe (Pat SrcSpanInfo, [(String, R.SrcSpan)], String))]
pchecks = let (*) = (,) in drop 1 -- see #174
    ["Use string literal pattern" * usePString
    ,"Use list literal pattern" * usePList
    ]


usePString :: Pat S -> Maybe (Pat SrcSpanInfo, [a], String)
usePString (PList _ xs) | xs /= [], Just s <- mapM fromPChar xs =
    let literal = PLit an (Signless an) $ String an s (show s)
    in Just (literal, [], prettyPrint literal)
usePString _ = Nothing

usePList :: Pat_ -> Maybe (Pat SrcSpanInfo, [(String, R.SrcSpan)], String)
usePList =
        fmap  ( (\(e, s) -> (PList an e, map (fmap toSS) s, prettyPrint (PList an (map snd s))))
              . unzip
              )
        . f True ['a'..'z']
    where
        f first _ x | x ~= "[]" = if first then Nothing else Just []
        f first (ident: cs) (view -> PApp_ ":" [a,b]) =
          ((a, g ident a) :) <$> f False cs b
        f first _ _ = Nothing

        g :: Char -> Pat_ -> (String, Pat_)
        g c p = ([c], PVar (ann p) (toNamed [c]))

useString :: p -> Exp S -> Maybe (Exp SrcSpanInfo, [a], String)
useString b (List _ xs) | xs /= [], Just s <- mapM fromChar xs =
  let literal = Lit an $ String an s (show s)
  in Just (literal , [], prettyPrint literal)
useString b _ = Nothing

useList :: p -> Exp_ -> Maybe (Exp SrcSpanInfo, [(String, R.SrcSpan)], String)
useList b =
        fmap  ( (\(e, s) -> (List an e, map (fmap toSS) s, prettyPrint (List an (map snd s))))
              . unzip
              )
        . f True ['a'..'z']
    where
        f first _ x | x ~= "[]" = if first then Nothing else Just []
        f first (ident:cs) (view -> App2 c a b) | c ~= ":" =
          ((a, g ident a) :) <$> f False cs b
        f first _ _ = Nothing

        g :: Char -> Exp_ -> (String, Exp_)
        g c p = ([c], toNamed [c])

useCons :: View a App2 =>
           Bool -> a -> Maybe (Exp SrcSpanInfo, [(String, R.SrcSpan)], String)
useCons False (view -> App2 op x y) | op ~= "++"
                                    , Just (x2, build) <- f x
                                    , not $ isAppend y =
  Just (gen (build x2) y
       , [("x", toSS x2), ("xs", toSS y)]
       , prettyPrint $ gen (build $ toNamed "x") (toNamed "xs"))
    where
        f (List _ [x]) = Just (x, \v -> if isApp x then v else paren v)
        f _ = Nothing


        gen x = InfixApp an x (QConOp an $ list_cons_name an)
useCons _ _ = Nothing



typeListChar :: Type SrcSpanInfo
typeListChar = TyList an (TyCon an (toNamed "Char"))

typeString :: Type SrcSpanInfo
typeString = TyCon an (toNamed "String")


stringType :: Decl_ -> [Idea]
stringType x = case x of
    InstDecl _ _ _ x -> f x
    _ -> f x
    where
        f x = concatMap g $ childrenBi x

        g :: Type_ -> [Idea]
        g e@(fromTyParen -> x) = [suggest "Use String" x (transform f x)
                                    rs | not . null $ rs]
            where f x = if x =~= typeListChar then typeString else x
                  rs = [Replace Type (toSS t) [] (prettyPrint typeString) | t <- universe x, t =~= typeListChar]

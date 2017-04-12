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
</TEST>
-}

module Hint.List(listHint) where

import Control.Applicative
import Hint.Type
import Data.Maybe
import Prelude
import Refact.Types


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
    | "False" `elem` cons = [suggest "Short-circuited list comprehension" o (List an []) []]
    | "True" `elem` cons = [suggest "Redundant True guards" o o2 []]
    where
        o2 = ListComp a e $ filter ((/= Just "True") . qualCon) xs
        cons = mapMaybe qualCon xs
        qualCon (QualStmt _ (Qualifier _ (Con _ x))) = Just $ fromNamed x
        qualCon _ = Nothing
listComp _ = []

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

isAppend (view -> App2 op _ _) = op ~= "++"
isAppend _ = False


checks = let (*) = (,) in drop 1 -- see #174
         ["Use string literal" * useString
         ,"Use list literal" * useList
         ,"Use :" * useCons
         ]

pchecks = let (*) = (,) in drop 1 -- see #174
          ["Use string literal pattern" * usePString
          ,"Use list literal pattern" * usePList
          ]


usePString (PList _ xs) | xs /= [], Just s <- mapM fromPChar xs =
  let literal = PLit an (Signless an) $ String an s (show s)
  in Just (literal, [], prettyPrint literal)
usePString _ = Nothing

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

useString b (List _ xs) | xs /= [], Just s <- mapM fromChar xs =
  let literal = Lit an $ String an s (show s)
  in Just (literal , [], prettyPrint literal)
useString b _ = Nothing

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



typeListChar = TyList an (TyCon an (toNamed "Char"))
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

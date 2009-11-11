{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Improve the structure of code

<TEST>
yes x y = if a then b else if c then d else e -- yes x y ; | a = b ; | c = d ; | otherwise = e
no x y = if a then b else c
yes x = case x of {True -> a ; False -> b} -- if x then a else b
yes x = case x of {False -> a ; _ -> b} -- if x then b else a
foo b | c <- f b = c -- foo (f -> c) = c
foo x y b z | c:cs <- f g b = c -- foo x y (f g -> c:cs) z = c
foo b | c <- f b = c + b
foo b | c <- f b = c where f = here
foo b | c <- f b = c where foo = b
foo b | c <- f b = c \
      | c <- f b = c
</TEST>
-}


module Hint.Structure where

import HSE.All
import Type
import Util
import Data.List
import Data.Maybe


structureHint :: DeclHint
structureHint _ x = concat [concatMap useGuards xs | FunBind xs <- [x]] ++
                    concatMap useIf (universeExp nullSrcLoc x)


useGuards :: Match -> [Idea]
useGuards x@(Match a b c d (UnGuardedRhs bod) e) 
    | length guards > 2 = [warn "Use guards" a x x2]
    where
        guards = asGuards bod
        x2 = Match a b c d (GuardedRhss guards) e

useGuards o@(Match sl b pats d (GuardedRhss [GuardedRhs _ [Generator _ pat (App op (Var (UnQual p)))] bod]) (BDecls decs))
    | Just i <- findIndex (== PVar p) pats
    , p `notElem` (vr bod ++ vr decs)
    , vr op `disjoint` decsBind, pvr pats `disjoint` vr op, pvr pat `disjoint` pvr pats
    = [warn "Use view patterns" sl o $
       Match sl b (take i pats ++ [PParen $ PViewPat op pat] ++ drop (i+1) pats) d (UnGuardedRhs bod) (BDecls decs)]
    where
        decsBind = nub $ concatMap declBind decs

        vr x = [y | Var (UnQual y) <- universeBi x]
        pvr x = [y | PVar y <- universeBi x]

useGuards _ = []


asGuards :: Exp -> [GuardedRhs]
asGuards (Paren x) = asGuards x
asGuards (If a b c) = GuardedRhs nullSrcLoc [Qualifier a] b : asGuards c
asGuards x = [GuardedRhs nullSrcLoc [Qualifier $ toNamed "otherwise"] x]


useIf :: (SrcLoc,Exp) -> [Idea]
useIf (loc,x@(Case on [simpAlt -> Just (as,av), simpAlt -> Just (bs,bv)]))
    | as == "True"  && bs `elem` ["False","_"] = iff av bv
    | as == "False" && bs `elem` ["True" ,"_"] = iff bv av
    where
        iff t f = [warn "Use if" loc x (If on t f)]
useIf _ = []

simpAlt (Alt _ p (UnGuardedAlt x) (BDecls [])) = Just (prettyPrint p, x)
simpAlt _ = Nothing


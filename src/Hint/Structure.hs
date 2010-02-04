{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Improve the structure of code

<TEST>
yes x y = if a then b else if c then d else e -- yes x y ; | a = b ; | c = d ; | otherwise = e
no x y = if a then b else c
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
import Hint
import Util
import Data.List


structureHint :: DeclHint
structureHint _ _ x = concat [concatMap useGuards xs | FunBind _ xs <- [x]]


useGuards :: Match S -> [Idea]
useGuards x@(Match a b c (UnGuardedRhs d bod) e) 
    | length guards > 2 = [warn "Use guards" x x2]
    where
        guards = asGuards bod
        x2 = Match a b c (GuardedRhss d guards) e

useGuards o@(Match sl b pats (GuardedRhss _ [GuardedRhs _ [Generator _ pat (App _ op (view -> Var_ p))] bod]) decs)
    | Just i <- findIndex (=~= (toNamed p :: Pat_)) pats
    , p `notElem` (vars bod ++ vars decs)
    , vars op `disjoint` decsBind, pvars pats `disjoint` vars op, pvars pat `disjoint` pvars pats
    = [warn "Use view patterns" o $
       Match sl b (take i pats ++ [PParen an $ PViewPat an op pat] ++ drop (i+1) pats) (UnGuardedRhs an bod) decs]
    where
        decsBind = nub $ concatMap declBind $ childrenBi decs

useGuards _ = []


asGuards :: Exp_ -> [GuardedRhs S]
asGuards (Paren _ x) = asGuards x
asGuards (If _ a b c) = GuardedRhs an [Qualifier an a] b : asGuards c
asGuards x = [GuardedRhs an [Qualifier an $ toNamed "otherwise"] x]

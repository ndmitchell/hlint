{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Improve the structure of code

<TEST>
yes x y = if a then b else if c then d else e where res = "yes x y\n  | a = b\n  | c = d\n  | otherwise = e"
no x y = if a then b else c
</TEST>
-}


module Hint.Structure where

import HSE.All
import Type
import Data.List
import Data.Char
import Data.Maybe


structureHint :: Hint
structureHint (FunBind xs) = concatMap useGuards xs
structureHint _ = []


useGuards :: Match -> [Idea]
useGuards x@(Match a b c d (UnGuardedRhs bod) e) 
    | length guards > 2 = [warn "Use guards" a x x2]
    where
        guards = asGuards bod
        x2 = Match a b c d (GuardedRhss guards) e
useGuards _ = []


asGuards :: Exp -> [GuardedRhs]
asGuards (Paren x) = asGuards x
asGuards (If a b c) = GuardedRhs nullSrcLoc [Qualifier a] b : asGuards c
asGuards x = [GuardedRhs nullSrcLoc [Qualifier $ toNamed "otherwise"] x]

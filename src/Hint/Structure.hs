{-# LANGUAGE ViewPatterns #-}

{-
    Improve the structure of code

<TEST>
yes x y = if a then b else if c then d else e -- yes x y ; | a = b ; | c = d ; | otherwise = e
x `yes` y = if a then b else if c then d else e -- yes x y ; | a = b ; | c = d ; | otherwise = e
no x y = if a then b else c
-- foo b | c <- f b = c -- foo (f -> c) = c
-- foo x y b z | c:cs <- f g b = c -- foo x y (f g -> c:cs) z = c
foo b | c <- f b = c + b
foo b | c <- f b = c where f = here
foo b | c <- f b = c where foo = b
foo b | c <- f b = c \
      | c <- f b = c
foo x = yes x x where yes x y = if a then b else if c then d else e -- yes x y ; | a = b ; | c = d ; | otherwise = e
foo x | otherwise = y -- foo x = y
-- FIXME: #358 foo x = x + x where -- foo x = x + x
foo x | a = b | True = d -- foo x | a = b ; | otherwise = d
foo (Bar _ _ _ _) = x -- Bar{}
foo (Bar _ x _ _) = x
foo (Bar _ _) = x
foo = case f v of _ -> x -- x
foo = case v of v -> x -- x
foo = case v of z -> z
foo = case v of _ | False -> x
foo = case v of !True -> x -- True
foo = case v of !(Just x) -> x -- (Just x)
foo = case v of !(x : xs) -> x -- (x:xs)
foo = case v of !1 -> x -- 1
foo = case v of !x -> x
foo = let ~x = 1 in y -- x
foo = let ~(x:xs) = y in z
</TEST>
-}


module Hint.Structure where

import Hint.Type
import Util


structureHint :: DeclHint
structureHint _ _ x =
    concatMap (uncurry hints . swap) (asPattern x) ++
    concatMap patHint (universeBi x) ++
    concatMap expHint (universeBi x)


hints :: (String -> Pattern -> Idea) -> Pattern -> [Idea]
hints gen (Pattern pat (UnGuardedRhs d bod) bind)
    | length guards > 2 = [gen "Use guards" $ Pattern pat (GuardedRhss d guards) bind]
    where guards = asGuards bod

{-
-- Do not suggest view patterns, they aren't something everyone likes sufficiently
hints gen (Pattern pats (GuardedRhss _ [GuardedRhs _ [Generator _ pat (App _ op (view -> Var_ p))] bod]) bind)
    | Just i <- findIndex (=~= (toNamed p :: Pat_)) pats
    , p `notElem` (vars bod ++ vars bind)
    , vars op `disjoint` decsBind, pvars pats `disjoint` vars op, pvars pat `disjoint` pvars pats
    = [gen "Use view patterns" $
       Pattern (take i pats ++ [PParen an $ PViewPat an op pat] ++ drop (i+1) pats) (UnGuardedRhs an bod) bind]
    where
        decsBind = nub $ concatMap declBind $ childrenBi bind
-}

hints gen (Pattern pats (GuardedRhss _ [GuardedRhs _ [test] bod]) bind)
    | prettyPrint test `elem` ["otherwise","True"]
    = [gen "Redundant guard" $ Pattern pats (UnGuardedRhs an bod) bind]

hints gen (Pattern pats bod (Just bind)) | f bind && False -- disabled due to bug 358
    = [gen "Redundant where" $ Pattern pats bod Nothing]
    where
        f (BDecls _ x) = null x
        f (IPBinds _ x) = null x

hints gen (Pattern pats (GuardedRhss _ (unsnoc -> (gs, GuardedRhs _ [test] bod))) bind)
    | prettyPrint test == "True"
    = [gen "Use otherwise" $ Pattern pats (GuardedRhss an $ gs ++ [GuardedRhs an [Qualifier an $ toNamed "otherwise"] bod]) bind]

hints _ _ = []


asGuards :: Exp_ -> [GuardedRhs S]
asGuards (Paren _ x) = asGuards x
asGuards (If _ a b c) = GuardedRhs an [Qualifier an a] b : asGuards c
asGuards x = [GuardedRhs an [Qualifier an $ toNamed "otherwise"] x]


data Pattern = Pattern [Pat_] (Rhs S) (Maybe (Binds S))

-- Invariant: Number of patterns may not change
asPattern :: Decl_ -> [(Pattern, String -> Pattern -> Idea)]
asPattern x = concatMap decl (universeBi x) ++ concatMap alt (universeBi x)
    where
        decl o@(PatBind a pat b rhs bind) = [(Pattern [pat] rhs bind, \msg (Pattern [pat] rhs bind) -> warn msg o $ PatBind a pat b rhs bind)]
        decl (FunBind _ xs) = map match xs
        decl _ = []
        match o@(Match a b pat rhs bind) = (Pattern pat rhs bind, \msg (Pattern pat rhs bind) -> warn msg o $ Match a b pat rhs bind)
        match o@(InfixMatch a p b ps rhs bind) = (Pattern (p:ps) rhs bind, \msg (Pattern (p:ps) rhs bind) -> warn msg o $ InfixMatch a p b ps rhs bind)
        alt o@(Alt a pat rhs bind) = [(Pattern [pat] (fromGuardedAlts rhs) bind, \msg (Pattern [pat] rhs bind) -> warn msg o $ Alt a pat (toGuardedAlts rhs) bind)]



-- Should these hints be in the same module? They are less structure, and more about pattern matching
-- Or perhaps the entire module should be renamed Pattern, since it's all about patterns
patHint :: Pat_ -> [Idea]
patHint o@(PApp _ name args) | length args >= 3 && all isPWildCard args = [warn "Use record patterns" o $ PRec an name []]
patHint o@(PBangPat _ x) | f x = [err "Redundant bang pattern" o x]
    where f (PParen _ x) = f x
          f (PAsPat _ _ x) = f x
          f PLit{} = True
          f PApp{} = True
          f PInfixApp{} = True
          f _ = False
patHint o@(PIrrPat _ x) | f x = [err "Redundant irrefutable pattern" o x]
    where f (PParen _ x) = f x
          f (PAsPat _ _ x) = f x
          f PWildCard{} = True
          f PVar{} = True
          f _ = False
patHint _ = []


expHint :: Exp_ -> [Idea]
expHint o@(Case _ _ [Alt _ PWildCard{} (UnGuardedAlt _ e) Nothing]) = [warn "Redundant case" o e]
expHint o@(Case _ (Var _ x) [Alt _ (PVar _ y) (UnGuardedAlt _ e) Nothing])
    | x =~= UnQual an y = [warn "Redundant case" o e]
expHint _ = []

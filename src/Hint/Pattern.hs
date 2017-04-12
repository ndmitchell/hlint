{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Improve the structure of code

<TEST>
yes x y = if a then b else if c then d else e -- yes x y ; | a = b ; | c = d ; | otherwise = e
x `yes` y = if a then b else if c then d else e -- x `yes` y ; | a = b ; | c = d ; | otherwise = e
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
foo x = x + x where -- foo x = x + x
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
foo = case v of !(I# x) -> y -- (I# x)
foo = let ~x = 1 in y -- x
foo = let ~(x:xs) = y in z
foo = let !x = undefined in y
foo = let !(I# x) = 4 in x
foo = let !(Just x) = Nothing in 3
foo = 1 where f !False = 2 -- False
foo = 1 where !False = True
foo = 1 where g (Just !True) = Nothing -- True
foo = 1 where Just !True = Nothing
foo otherwise = 1 -- _
foo ~x = y -- x
{-# LANGUAGE Strict #-} foo ~x = y
foo !(x, y) = x -- (x, y)
foo ![x] = x -- [x]
foo !Bar { bar = x } = x -- Bar { bar = x }
l !(() :: ()) = x -- (() :: ())
</TEST>
-}


module Hint.Pattern(patternHint) where

import Hint.Type
import Data.Function
import Data.List.Extra
import Data.Tuple
import Data.Maybe
import Data.Either
import Refact.Types hiding (RType(Pattern, Match))
import qualified Refact.Types as R (RType(Pattern, Match), SrcSpan)


patternHint :: DeclHint
patternHint _ modu x =
    concatMap (uncurry hints . swap) (asPattern x) ++
    -- PatBind (used in Let and Where) contains lazy-by-default patterns, everything else is strict
    concatMap (patHint strict False) (universeBi [p | PatBind _ p _ _ <- universe x]) ++
    concatMap (patHint strict True) (universeBi $ transform noPatBind x) ++
    concatMap expHint (universeBi x)
    where
        noPatBind (PatBind a _ b c) = PatBind a (PWildCard a) b c
        noPatBind x = x

        strict = "Strict" `elem` [n | LanguagePragma _ ns <- modulePragmas modu, Ident _ n <- ns]


hints :: (String -> Pattern -> [Refactoring R.SrcSpan] -> Idea) -> Pattern -> [Idea]
hints gen (Pattern l rtype pat (UnGuardedRhs d bod) bind)
    | length guards > 2 = [gen "Use guards" (Pattern l rtype pat (GuardedRhss d guards) bind) [refactoring]]
    where rawGuards = asGuards bod
          mkGuard a = GuardedRhs an [Qualifier an a]
          guards = map (uncurry mkGuard) rawGuards
          (lhs, rhs) = unzip rawGuards
          mkTemplate c ps =
            -- Check if the expression has been injected or is natural
            let checkAn p v = if ann p == an then Left p else Right ( c ++ [v], toSS p)
            in zipWith checkAn ps ['1' .. '9']
          patSubts = case pat of
                       [p] -> [Left p] -- Substitution doesn't work properly for PatBinds
                                       -- This will probably produce
                                       -- unexpected results if the pattern
                                       -- contains any template variables
                       ps  -> mkTemplate "p100" ps
          guardSubts = mkTemplate "g100" lhs
          exprSubts  = mkTemplate "e100" rhs
          templateGuards = zipWith (mkGuard `on` toString) guardSubts exprSubts
          toString (Left e) = e
          toString (Right (v, _)) = toNamed v
          template = fromMaybe "" $ ideaTo (gen "" (Pattern l rtype (map toString patSubts) (GuardedRhss d templateGuards) bind) [])
          f :: [Either a (String, R.SrcSpan)] -> [(String, R.SrcSpan)]
          f = rights
          refactoring = Replace rtype (toRefactSrcSpan $ srcInfoSpan l) (f patSubts ++ f guardSubts ++ f exprSubts) template

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

hints gen (Pattern l t pats (GuardedRhss _ [GuardedRhs _ [test] bod]) bind)
    | prettyPrint test `elem` ["otherwise","True"]
    = [gen "Redundant guard" (Pattern l t pats (UnGuardedRhs an bod) bind) [Delete Stmt (toSS test)]]

hints gen (Pattern l t pats bod (Just bind)) | f bind
    = [gen "Redundant where" (Pattern l t pats bod Nothing) []]
    where
        f (BDecls _ x) = null x
        f (IPBinds _ x) = null x

hints gen (Pattern l t pats (GuardedRhss _ (unsnoc -> Just (gs, GuardedRhs _ [test] bod))) bind)
    | prettyPrint test == "True"
    = [gen "Use otherwise" (Pattern l t pats (GuardedRhss an $ gs ++ [GuardedRhs an [Qualifier an $ toNamed "otherwise"] bod]) bind) [Replace Expr (toSS test) [] "otherwise"]]

hints _ _ = []


asGuards :: Exp_ -> [(Exp S, Exp S)]
asGuards (Paren _ x) = asGuards x
asGuards (If _ a b c) = (a, b) : asGuards c
asGuards x = [(toNamed "otherwise", x)]


data Pattern = Pattern S R.RType [Pat_] (Rhs S) (Maybe (Binds S))

-- Invariant: Number of patterns may not change
asPattern :: Decl_ -> [(Pattern, String -> Pattern -> [Refactoring R.SrcSpan] -> Idea)]
asPattern x = concatMap decl (universeBi x) ++ concatMap alt (universeBi x)
    where
        decl o@(PatBind a pat rhs bind) = [(Pattern a Bind [pat] rhs bind, \msg (Pattern _ _ [pat] rhs bind) rs -> suggest msg o (PatBind a pat rhs bind) rs)]
        decl (FunBind _ xs) = map match xs
        decl _ = []
        match o@(Match a b pat rhs bind) = (Pattern a R.Match pat rhs bind, \msg (Pattern _ _ pat rhs bind) rs -> suggest msg o (Match a b pat rhs bind) rs)
        match o@(InfixMatch a p b ps rhs bind) = (Pattern a R.Match (p:ps) rhs bind, \msg (Pattern _ _ (p:ps) rhs bind) rs -> suggest msg o (InfixMatch a p b ps rhs bind) rs)
        alt o@(Alt a pat rhs bind) = [(Pattern a R.Match [pat] rhs bind, \msg (Pattern _ _ [pat] rhs bind) rs -> suggest msg o (Alt a pat rhs bind) [])]


-- First Bool is if Strict is a language extension
-- Second Bool is if this pattern in this context is going to be evaluated strictly
patHint :: Bool -> Bool -> Pat_ -> [Idea]
patHint lang strict o@(PApp _ name args) | length args >= 3 && all isPWildCard args =
  [suggest "Use record patterns" o (PRec an name []) [Replace R.Pattern (toSS o) [] (prettyPrint $ PRec an name [])] ]
patHint lang strict o@(PVar _ v) | prettyPrint v == "otherwise" = [warn "Used otherwise as a pattern" o (PWildCard an) []]

patHint lang strict o@(PBangPat _ x) | strict, f x = [warn "Redundant bang pattern" o x [r]]
    where f (PParen _ x) = f x
          f (PAsPat _ _ x) = f x
          f PLit{} = True
          f PApp{} = True
          f PInfixApp{} = True
          f PTuple{} = True
          f PList{} = True
          f PRec{} = True
          f (PatTypeSig _ x _) = f x
          f _ = False
          r = Replace R.Pattern (toSS o) [("x", toSS x)] "x"
patHint False strict o@(PIrrPat _ x) | f x = [warn "Redundant irrefutable pattern" o x [r]]
    where f (PParen _ x) = f x
          f (PAsPat _ _ x) = f x
          f PWildCard{} = True
          f PVar{} = True
          f _ = False
          r = Replace R.Pattern (toSS o) [("x", toSS x)] "x"
patHint _ _ _ = []


expHint :: Exp_ -> [Idea]
expHint o@(Case _ _ [Alt _ PWildCard{} (UnGuardedRhs _ e) Nothing]) =
  [suggest "Redundant case" o e [r]]
  where
    r = Replace Expr (toSS o) [("x", toSS e)] "x"
expHint o@(Case _ (Var _ x) [Alt _ (PVar _ y) (UnGuardedRhs _ e) Nothing])
    | x =~= UnQual an y =
      [suggest "Redundant case" o e [r]]
  where
    r = Replace Expr (toSS o) [("x", toSS e)] "x"
expHint _ = []

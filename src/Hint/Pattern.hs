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
foo x@_ = x -- x
foo x@Foo = x
</TEST>
-}


module Hint.Pattern(patternHint) where

import Hint.Type(DeclHint',Idea,ghcAnnotations,ideaTo,toSS',toRefactSrcSpan,ghcSpanToHSE,suggest',warn')
import Data.Generics.Uniplate.Operations
import Data.Function
import Data.List.Extra
import Data.Tuple
import Data.Maybe
import Data.Either
import Refact.Types hiding (RType(Pattern, Match), SrcSpan)
import qualified Refact.Types as R (RType(Pattern, Match), SrcSpan)

import HsSyn
import SrcLoc
import RdrName
import OccName
import Bag

import GHC.Util

patternHint :: DeclHint'
patternHint _scope modu x =
    concatMap (uncurry hints . swap) (asPattern x) ++
    -- PatBind (used in 'let' and 'where') contains lazy-by-default
    -- patterns, everything else is strict.
    concatMap (patHint strict False) (located [p | PatBind _ p _ _ <- universeBi x :: [HsBind GhcPs]]) ++
    concatMap (patHint strict True) (located (universeBi $ transformBi noPatBind x)) ++
    concatMap expHint (universeBi x)
  where
    located ps = [p | p@XPat{} <- ps]  -- restrict attention to patterns with locs
    exts = nubOrd $ concatMap snd (langExts (pragmas (ghcAnnotations modu))) -- language extensions enabled at source
    strict = "Strict" `elem` exts

    noPatBind :: LHsBind GhcPs -> LHsBind GhcPs
    noPatBind (LL loc a@PatBind{}) = cL loc a{pat_lhs=noLoc (WildPat noExt)}
    noPatBind x = x

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

hints :: (String -> Pattern -> [Refactoring R.SrcSpan] -> Idea) -> Pattern -> [Idea]
hints gen (Pattern l rtype pat (GRHSs _ [LL _ (GRHS _ [] bod)] bind))
  | length guards > 2 = [gen "Use guards" (Pattern l rtype pat (GRHSs noExt guards bind)) [refactoring]]
  where
    rawGuards :: [(LHsExpr GhcPs, LHsExpr GhcPs)]
    rawGuards = asGuards bod

    mkGuard :: LHsExpr GhcPs -> (LHsExpr GhcPs -> GRHS GhcPs (LHsExpr GhcPs))
    mkGuard a = GRHS noExt [noLoc $ BodyStmt noExt a noSyntaxExpr' noSyntaxExpr']

    guards :: [LGRHS GhcPs (LHsExpr GhcPs)]
    guards = map (noLoc . uncurry mkGuard) rawGuards

    (lhs, rhs) = unzip rawGuards

    mkTemplate c ps =
      -- Check if the expression has been injected or is natural.
      zipWith checkLoc ps ['1' .. '9']
      where
        checkLoc p@(LL l _) v = if l == noSrcSpan then Left p else Right (c ++ [v], toSS' p)
        checkLoc _ v = undefined -- {-# COMPLETE LL #-}

    patSubts =
      case pat of
        [p] -> [Left p] -- Substitution doesn't work properly for PatBinds.
                        -- This will probably produce unexpected results if the pattern contains any template variables.
        ps  -> mkTemplate "p100" ps
    guardSubts = mkTemplate "g100" lhs
    exprSubts  = mkTemplate "e100" rhs
    templateGuards = map noLoc (zipWith (mkGuard `on` toString) guardSubts exprSubts)

    toString (Left e) = e
    toString (Right (v, _)) = strToVar' v
    toString' (Left e) = e
    toString' (Right (v, _)) = strToPat' v

    template = fromMaybe "" $ ideaTo (gen "" (Pattern l rtype (map toString' patSubts) (GRHSs noExt templateGuards bind)) [])

    f :: [Either a (String, R.SrcSpan)] -> [(String, R.SrcSpan)]
    f = rights
    refactoring = Replace rtype (toRefactSrcSpan$ ghcSpanToHSE l) (f patSubts ++ f guardSubts ++ f exprSubts) template
hints gen (Pattern l t pats o@(GRHSs _ [LL _ (GRHS _ [test] bod)] bind))
  | unsafePrettyPrint test `elem` ["otherwise", "True"]
  = [gen "Redundant guard" (Pattern l t pats o{grhssGRHSs=[noLoc (GRHS noExt [] bod)]}) [Delete Stmt (toSS' test)]]
hints gen (Pattern l t pats bod@(GRHSs _ _ binds)) | f binds
  = [gen "Redundant where" (Pattern l t pats bod{grhssLocalBinds=noLoc (EmptyLocalBinds noExt)}) []]
  where
    f :: LHsLocalBinds GhcPs -> Bool
    f (LL _ (HsValBinds _ (ValBinds _ bag _))) = isEmptyBag bag
    f (LL _ (HsIPBinds _ (IPBinds _ l))) = null l
    f _ = False
hints gen (Pattern l t pats o@(GRHSs _ (unsnoc -> Just (gs, LL _ (GRHS _ [test] bod))) binds))
  | unsafePrettyPrint test == "True"
  = let tag = noLoc (mkRdrUnqual $ mkVarOcc "otherwise")
        otherwise_ = noLoc $ BodyStmt noExt (noLoc (HsVar noExt tag)) noSyntaxExpr' noSyntaxExpr' in
      [gen "Use otherwise" (Pattern l t pats o{grhssGRHSs = gs ++ [noLoc (GRHS noExt [otherwise_] bod)]}) [Replace Expr (toSS' test) [] "otherwise"]]
hints _ _ = []

asGuards :: LHsExpr GhcPs -> [(LHsExpr GhcPs, LHsExpr GhcPs)]
asGuards (LL _ (HsPar _ x)) = asGuards x
asGuards (LL _ (HsIf _ _ a b c)) = (a, b) : asGuards c
asGuards x = [(noLoc (HsVar noExt (noLoc (mkRdrUnqual $ mkVarOcc "otherwise"))), x)]

data Pattern = Pattern SrcSpan R.RType [Pat GhcPs] (GRHSs GhcPs (LHsExpr GhcPs))

-- Invariant: Number of patterns may not change
asPattern :: LHsDecl GhcPs  -> [(Pattern, String -> Pattern -> [Refactoring R.SrcSpan] -> Idea)]
asPattern (LL loc x) = concatMap decl (universeBi x)
  where
    decl :: HsBind GhcPs -> [(Pattern, String -> Pattern -> [Refactoring R.SrcSpan] -> Idea)]
    decl o@(PatBind _ pat rhs _) = [(Pattern loc Bind [pat] rhs, \msg (Pattern _ _ [pat] rhs) rs -> suggest' msg (cL loc o :: LHsBind GhcPs) (noLoc (PatBind noExt pat rhs ([], [])) :: LHsBind GhcPs) rs)]
    decl (FunBind _ _ (MG _ (LL _ xs) _) _ _) = map match xs
    decl _ = []

    match :: LMatch GhcPs (LHsExpr GhcPs) -> (Pattern, String -> Pattern -> [Refactoring R.SrcSpan] -> Idea)
    match o@(LL loc (Match _ ctx pats grhss)) = (Pattern loc R.Match pats grhss, \msg (Pattern _ _ pats grhss) rs -> suggest' msg o (noLoc (Match noExt ctx  pats grhss) :: LMatch GhcPs (LHsExpr GhcPs)) rs)
    match _ = undefined -- {-# COMPLETE LL #-}
asPattern _ = [] -- {-# COMPLETE LL #-}

-- First Bool is if 'Strict' is a language extension. Second Bool is
-- if this pattern in this context is going to be evaluated strictly.
patHint :: Bool -> Bool -> Pat GhcPs -> [Idea]
patHint _ _ o@(LL _ (ConPatIn name (PrefixCon args)))
  | length args >= 3 && all isPWildCard' args =
  let rec_fields = HsRecFields [] Nothing :: HsRecFields GhcPs (Pat GhcPs)
      new        = ConPatIn name (RecCon rec_fields) :: Pat GhcPs
  in
  [suggest' "Use record patterns" o new [Replace R.Pattern (toSS' o) [] (unsafePrettyPrint new)]]
patHint _ _ o@(LL _ (VarPat _ (L _ name)))
  | occNameString (rdrNameOcc name) == "otherwise" =
    [warn' "Used otherwise as a pattern" o (noLoc (WildPat noExt) :: Pat GhcPs) []]
patHint lang strict o@(LL _ (BangPat _ (LL _ x)))
  | strict, f x = [warn' "Redundant bang pattern" o x [r]]
  where
    f :: Pat GhcPs -> Bool
    f (ParPat _ (LL _ x)) = f x
    f (AsPat _ _ (LL _ x)) = f x
    f LitPat {} = True
    f NPat {} = True
    f ConPatIn {} = True
    f TuplePat {} = True
    f ListPat {} = True
    f (SigPat _ (LL _ p) _) = f p
    f _ = False
    r = Replace R.Pattern (toSS' o) [("x", toSS' x)] "x"
patHint False _ o@(LL _ (LazyPat _ (LL _ x)))
  | f x = [warn' "Redundant irrefutable pattern" o x [r]]
  where
    f :: Pat GhcPs -> Bool
    f (ParPat _ (LL _ x)) = f x
    f (AsPat _ _ (LL _ x)) = f x
    f WildPat{} = True
    f VarPat{} = True
    f _ = False
    r = Replace R.Pattern (toSS' o) [("x", toSS' x)] "x"
patHint _ _ o@(LL _ (AsPat _ v (LL _ (WildPat _)))) =
  [warn' "Redundant as-pattern" o v []]
patHint _ _ _ = []

expHint :: LHsExpr GhcPs -> [Idea]
expHint o@(LL _ (HsCase _ _ (MG _ (L _ [LL _ (Match _ CaseAlt [LL _ (WildPat _)] (GRHSs _ [LL _ (GRHS _ [] e)] (LL  _ (EmptyLocalBinds _)))) ]) _ ))) =
  [suggest' "Redundant case" o e [r]]
  where
    r = Replace Expr (toSS' o) [("x", toSS' e)] "x"
expHint o@(LL _ (HsCase _ (LL _ (HsVar _ (L _ x))) (MG _ (L _ [LL _ (Match _ CaseAlt [LL _ (VarPat _ (L _ y))] (GRHSs _ [LL _ (GRHS _ [] e)] (LL  _ (EmptyLocalBinds _)))) ]) _ )))
  | occNameString (rdrNameOcc x) == occNameString (rdrNameOcc y) =
      [suggest' "Redundant case" o e [r]]
  where
    r = Replace Expr (toSS' o) [("x", toSS' e)] "x"
expHint _ = []

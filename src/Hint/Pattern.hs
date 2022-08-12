{-# LANGUAGE ViewPatterns, PatternGuards, TypeFamilies #-}

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
foo x = x + x where --
foo x | a = b | True = d -- foo x | a = b ; | otherwise = d
foo (Bar _ _ _ _) = x -- Bar{}
foo (Bar _ x _ _) = x
foo (Bar _ _) = x
foo = case f v of _ -> x -- x
foo = case v of v -> x -- x
foo = case v of z -> z
foo = case v of _ | False -> x
foo x | x < -2 * 3 = 4
foo = case v of !True -> x -- True
{-# LANGUAGE BangPatterns #-}; foo = case v of !True -> x -- True
{-# LANGUAGE BangPatterns #-}; foo = case v of !(Just x) -> x -- (Just x)
{-# LANGUAGE BangPatterns #-}; foo = case v of !(x : xs) -> x -- (x:xs)
{-# LANGUAGE BangPatterns #-}; foo = case v of !1 -> x -- 1
{-# LANGUAGE BangPatterns #-}; foo = case v of !x -> x
{-# LANGUAGE BangPatterns #-}; foo = case v of !(I# x) -> y -- (I# x)
foo = let ~x = 1 in y -- x
foo = let ~(x:xs) = y in z
{-# LANGUAGE BangPatterns #-}; foo = let !x = undefined in y
{-# LANGUAGE BangPatterns #-}; foo = let !(I# x) = 4 in x
{-# LANGUAGE BangPatterns #-}; foo = let !(Just x) = Nothing in 3
{-# LANGUAGE BangPatterns #-}; foo = 1 where f !False = 2 -- False
{-# LANGUAGE BangPatterns #-}; foo = 1 where !False = True
{-# LANGUAGE BangPatterns #-}; foo = 1 where g (Just !True) = Nothing -- True
{-# LANGUAGE BangPatterns #-}; foo = 1 where Just !True = Nothing
foo otherwise = 1 -- _
foo ~x = y -- x
{-# LANGUAGE Strict #-} foo ~x = y
{-# LANGUAGE BangPatterns #-}; foo !(x, y) = x -- (x, y)
{-# LANGUAGE BangPatterns #-}; foo ![x] = x -- [x]
foo !Bar { bar = x } = x -- Bar { bar = x }
{-# LANGUAGE BangPatterns #-}; l !(() :: ()) = x -- (() :: ())
foo x@_ = x -- x
foo x@Foo = x
otherwise = True
</TEST>
-}


module Hint.Pattern(patternHint) where

import Hint.Type(DeclHint,Idea,modComments,ideaTo,toSSA,toRefactSrcSpan,suggest,suggestRemove,warn)
import Data.Generics.Uniplate.DataOnly
import Data.Function
import Data.List.Extra
import Data.Tuple
import Data.Maybe
import Data.Either
import Refact.Types hiding (RType(Pattern, Match), SrcSpan)
import qualified Refact.Types as R (RType(Pattern, Match), SrcSpan)

import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence
import GHC.Data.Bag
import GHC.Types.Basic hiding (Pattern)
import qualified GHC.Data.Strict

import GHC.Util
import Language.Haskell.GhclibParserEx.GHC.Hs.Pat
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader

patternHint :: DeclHint
patternHint _scope modu x =
    concatMap (uncurry hints . swap) (asPattern x) ++
    -- PatBind (used in 'let' and 'where') contains lazy-by-default
    -- patterns, everything else is strict.
    concatMap (patHint strict False) [p | PatBind _ p _ _ <- universeBi x :: [HsBind GhcPs]] ++
    concatMap (patHint strict True) (universeBi $ transformBi noPatBind x) ++
    concatMap expHint (universeBi x)
  where
    exts = nubOrd $ concatMap snd (languagePragmas (pragmas (modComments modu))) -- language extensions enabled at source
    strict = "Strict" `elem` exts

    noPatBind :: LHsBind GhcPs -> LHsBind GhcPs
    noPatBind (L loc a@PatBind{}) = L loc a{pat_lhs=noLocA (WildPat noExtField)}
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
hints gen (Pattern l rtype pat (GRHSs _ [L _ (GRHS _ [] bod)] bind))
  | length guards > 2 = [gen "Use guards" (Pattern l rtype pat (GRHSs emptyComments guards bind)) [refactoring]]
  where
    rawGuards :: [(LHsExpr GhcPs, LHsExpr GhcPs)]
    rawGuards = asGuards bod

    mkGuard :: LHsExpr GhcPs -> (LHsExpr GhcPs -> GRHS GhcPs (LHsExpr GhcPs))
    mkGuard a = GRHS EpAnnNotUsed [noLocA $ BodyStmt noExtField a noSyntaxExpr noSyntaxExpr]

    guards :: [LGRHS GhcPs (LHsExpr GhcPs)]
    guards = map (noLocA . uncurry mkGuard) rawGuards

    (lhs, rhs) = unzip rawGuards

    mkTemplate c ps =
      -- Check if the expression has been injected or is natural.
      zipWith checkLoc ps ['1' .. '9']
      where
        checkLoc p@(L l _) v = if locA l == noSrcSpan then Left p else Right (c ++ [v], toSSA p)

    patSubts =
      case pat of
        [p] -> [Left p] -- Substitution doesn't work properly for PatBinds.
                        -- This will probably produce unexpected results if the pattern contains any template variables.
        ps  -> mkTemplate "p100" ps
    guardSubts = mkTemplate "g100" lhs
    exprSubts  = mkTemplate "e100" rhs
    templateGuards = map noLocA (zipWith (mkGuard `on` toString) guardSubts exprSubts)

    toString (Left e) = e
    toString (Right (v, _)) = strToVar v
    toString' (Left e) = e
    toString' (Right (v, _)) = strToPat v

    template = fromMaybe "" $ ideaTo (gen "" (Pattern l rtype (map toString' patSubts) (GRHSs emptyComments templateGuards bind)) [])

    f :: [Either a (String, R.SrcSpan)] -> [(String, R.SrcSpan)]
    f = rights
    refactoring = Replace rtype (toRefactSrcSpan l) (f patSubts ++ f guardSubts ++ f exprSubts) template
hints gen (Pattern l t pats o@(GRHSs _ [L _ (GRHS _ [test] bod)] bind))
  | unsafePrettyPrint test `elem` ["otherwise", "True"]
  = [gen "Redundant guard" (Pattern l t pats o{grhssGRHSs=[noLocA (GRHS EpAnnNotUsed [] bod)]}) [Delete Stmt (toSSA test)]]
hints _ (Pattern l t pats bod@(GRHSs _ _ binds)) | f binds
  = [suggestRemove "Redundant where" whereSpan "where" [ {- TODO refactoring for redundant where -} ]]
  where
    f :: HsLocalBinds GhcPs -> Bool
    f (HsValBinds _ (ValBinds _ bag _)) = isEmptyBag bag
    f (HsIPBinds _ (IPBinds _ l)) = null l
    f _ = False
    whereSpan = case l of
      UnhelpfulSpan s -> UnhelpfulSpan s
      RealSrcSpan s _ ->
        let end = realSrcSpanEnd s
            start = mkRealSrcLoc (srcSpanFile s) (srcLocLine end) (srcLocCol end - 5)
         in RealSrcSpan (mkRealSrcSpan start end) GHC.Data.Strict.Nothing
hints gen (Pattern l t pats o@(GRHSs _ (unsnoc -> Just (gs, L _ (GRHS _ [test] bod))) binds))
  | unsafePrettyPrint test == "True"
  = let otherwise_ = noLocA $ BodyStmt noExtField (strToVar "otherwise") noSyntaxExpr noSyntaxExpr in
      [gen "Use otherwise" (Pattern l t pats o{grhssGRHSs = gs ++ [noLocA (GRHS EpAnnNotUsed [otherwise_] bod)]}) [Replace Expr (toSSA test) [] "otherwise"]]
hints _ _ = []

asGuards :: LHsExpr GhcPs -> [(LHsExpr GhcPs, LHsExpr GhcPs)]
asGuards (L _ (HsPar _ _ x _)) = asGuards x
asGuards (L _ (HsIf _ a b c)) = (a, b) : asGuards c
asGuards x = [(strToVar "otherwise", x)]

data Pattern = Pattern SrcSpan R.RType [LPat GhcPs] (GRHSs GhcPs (LHsExpr GhcPs))

-- Invariant: Number of patterns may not change
asPattern :: LHsDecl GhcPs  -> [(Pattern, String -> Pattern -> [Refactoring R.SrcSpan] -> Idea)]
asPattern (L loc x) = concatMap decl (universeBi x)
  where
    decl :: HsBind GhcPs -> [(Pattern, String -> Pattern -> [Refactoring R.SrcSpan] -> Idea)]
    decl o@(PatBind _ pat rhs _) = [(Pattern (locA loc) Bind [pat] rhs, \msg (Pattern _ _ [pat] rhs) rs -> suggest msg (noLoc o :: Located (HsBind GhcPs)) (noLoc (PatBind EpAnnNotUsed pat rhs ([], [])) :: Located (HsBind GhcPs)) rs)]
    decl (FunBind _ _ (MG _ (L _ xs) _) _) = map match xs
    decl _ = []

    match :: LMatch GhcPs (LHsExpr GhcPs) -> (Pattern, String -> Pattern -> [Refactoring R.SrcSpan] -> Idea)
    match o@(L loc (Match _ ctx pats grhss)) = (Pattern (locA loc) R.Match pats grhss, \msg (Pattern _ _ pats grhss) rs -> suggest msg (reLoc o) (noLoc (Match EpAnnNotUsed ctx  pats grhss) :: Located (Match GhcPs (LHsExpr GhcPs))) rs)

-- First Bool is if 'Strict' is a language extension. Second Bool is
-- if this pattern in this context is going to be evaluated strictly.
patHint :: Bool -> Bool -> LPat GhcPs -> [Idea]
patHint _ _ o@(L _ (ConPat _ name (PrefixCon _ args)))
  | length args >= 3 && all isPWildcard args =
  let rec_fields = HsRecFields [] Nothing :: HsRecFields GhcPs (LPat GhcPs)
      new        = noLocA $ ConPat EpAnnNotUsed name (RecCon rec_fields) :: LPat GhcPs
  in
  [suggest "Use record patterns" (reLoc o) (reLoc new) [Replace R.Pattern (toSSA o) [] (unsafePrettyPrint new)]]
patHint _ _ o@(L _ (VarPat _ (L _ name)))
  | occNameString (rdrNameOcc name) == "otherwise" =
    [warn "Used otherwise as a pattern" (reLoc o) (noLoc (WildPat noExtField) :: Located (Pat GhcPs)) []]
patHint lang strict o@(L _ (BangPat _ pat@(L _ x)))
  | strict, f x = [warn "Redundant bang pattern" (reLoc o) (noLoc x :: Located (Pat GhcPs)) [r]]
  where
    f :: Pat GhcPs -> Bool
    f (ParPat _ _ (L _ x) _) = f x
    f (AsPat _ _ (L _ x)) = f x
    f LitPat {} = True
    f NPat {} = True
    f ConPat {} = True
    f TuplePat {} = True
    f ListPat {} = True
    f (SigPat _ (L _ p) _) = f p
    f _ = False
    r = Replace R.Pattern (toSSA o) [("x", toSSA pat)] "x"
patHint False _ o@(L _ (LazyPat _ pat@(L _ x)))
  | f x = [warn "Redundant irrefutable pattern" (reLoc o) (noLoc x :: Located (Pat GhcPs)) [r]]
  where
    f :: Pat GhcPs -> Bool
    f (ParPat _ _ (L _ x) _) = f x
    f (AsPat _ _ (L _ x)) = f x
    f WildPat{} = True
    f VarPat{} = True
    f _ = False
    r = Replace R.Pattern (toSSA o) [("x", toSSA pat)] "x"
patHint _ _ o@(L _ (AsPat _ v (L _ (WildPat _)))) =
  [warn "Redundant as-pattern" (reLoc o) (reLoc v) [Replace R.Pattern (toSSA o) [] (rdrNameStr v)]]
patHint _ _ _ = []

expHint :: LHsExpr GhcPs -> [Idea]
 -- Note the 'FromSource' in these equations (don't warn on generated match groups).
expHint o@(L _ (HsCase _ _ (MG _ (L _ [L _ (Match _ CaseAlt [L _ (WildPat _)] (GRHSs _ [L _ (GRHS _ [] e)] (EmptyLocalBinds _))) ]) FromSource ))) =
  [suggest "Redundant case" (reLoc o) (reLoc e) [r]]
  where
    r = Replace Expr (toSSA o) [("x", toSSA e)] "x"
expHint o@(L _ (HsCase _ (L _ (HsVar _ (L _ x))) (MG _ (L _ [L _ (Match _ CaseAlt [L _ (VarPat _ (L _ y))] (GRHSs _ [L _ (GRHS _ [] e)] (EmptyLocalBinds _))) ]) FromSource )))
  | occNameStr x == occNameStr y =
      [suggest "Redundant case" (reLoc o) (reLoc e) [r]]
  where
    r = Replace Expr (toSSA o) [("x", toSSA e)] "x"
expHint _ = []

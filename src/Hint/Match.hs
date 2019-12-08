{-# LANGUAGE PatternGuards, ViewPatterns, RecordWildCards, FlexibleContexts, ScopedTypeVariables #-}

-- Keep until 'checkSide', 'checkDefine', ... are used.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-
The matching does a fairly simple unification between the two terms, treating
any single letter variable on the left as a free variable. After the matching
we substitute, transform and check the side conditions. We also "see through"
both ($) and (.) functions on the right.

TRANSFORM PATTERNS
_eval_ - perform deep evaluation, must be used at the top of a RHS
_noParen_ - don't bracket this particular item

SIDE CONDITIONS
(&&), (||), not - boolean connectives
isAtom x - does x never need brackets
isFoo x - is the root constructor of x a "Foo"
notEq x y - are x and y not equal
notIn xs ys - are all x variables not in ys expressions
noTypeCheck, noQuickCheck - no semantics, a hint for testing only

($) AND (.)
We see through ($)/(.) by expanding it if nothing else matches.
We also see through (.) by translating rules that have (.) equivalents
to separate rules. For example:

concat (map f x) ==> concatMap f x
-- we spot both these rules can eta reduce with respect to x
concat . map f ==> concatMap f
-- we use the associativity of (.) to add
concat . map f . x ==> concatMap f . x
-- currently 36 of 169 rules have (.) equivalents

We see through (.) if the RHS is dull using id, e.g.

not (not x) ==> x
not . not ==> id
not . not . x ==> x
-}

module Hint.Match(readMatch,readMatch') where

import Control.Applicative
import Data.List.Extra
import Data.Maybe
import Config.Type
import Hint.Type
import Control.Monad
import Data.Tuple.Extra
import HSE.Unify
import Util
import Timing
import qualified Data.Set as Set
import Prelude
import qualified Refact.Types as R

import qualified HsSyn as GHC
import qualified SrcLoc as GHC
import qualified BasicTypes as GHC
import RdrName
import OccName
import Data.Data
import GHC.Util

fmapAn :: Exp b -> Exp SrcSpanInfo
fmapAn = fmap (const an)


---------------------------------------------------------------------
-- READ THE RULE

-- old

readMatch :: [HintRule] -> DeclHint
readMatch settings = findIdeas (concatMap readRule settings)

-- new

readMatch' :: [HintRule] -> Scope' -> ModuleEx -> GHC.LHsDecl GHC.GhcPs -> [Idea]
readMatch' settings = findIdeas' (concatMap readRule' settings)

-- old

readRule :: HintRule -> [HintRule]
readRule m@HintRule{hintRuleLHS=(fmapAn -> hintRuleLHS), hintRuleRHS=(fmapAn -> hintRuleRHS), hintRuleSide=(fmap fmapAn -> hintRuleSide)} =
    (:) m{hintRuleLHS=hintRuleLHS,hintRuleSide=hintRuleSide,hintRuleRHS=hintRuleRHS} $ do
        (l,v1) <- dotVersion hintRuleLHS
        (r,v2) <- dotVersion hintRuleRHS
        guard $ v1 == v2 && l /= [] && (length l > 1 || length r > 1) && Set.notMember v1 (freeVars $ maybeToList hintRuleSide ++ l ++ r)
        if r /= [] then
            [m{hintRuleLHS=dotApps l, hintRuleRHS=dotApps r, hintRuleSide=hintRuleSide}
            ,m{hintRuleLHS=dotApps (l++[toNamed v1]), hintRuleRHS=dotApps (r++[toNamed v1]), hintRuleSide=hintRuleSide}]
         else if length l > 1 then
            [m{hintRuleLHS=dotApps l, hintRuleRHS=toNamed "id", hintRuleSide=hintRuleSide}
            ,m{hintRuleLHS=dotApps (l++[toNamed v1]), hintRuleRHS=toNamed v1, hintRuleSide=hintRuleSide}]
         else []

-- new

readRule' :: HintRule -> [HintRule]
readRule' m@HintRule{ hintRuleGhcLHS=(stripLocs' . unwrap -> hintRuleGhcLHS)
                    , hintRuleGhcRHS=(stripLocs' . unwrap -> hintRuleGhcRHS)
                    , hintRuleGhcSide=((stripLocs' . unwrap <$>) -> hintRuleGhcSide)
                    } =
   (:) m{ hintRuleGhcLHS=wrap hintRuleGhcLHS
        , hintRuleGhcRHS=wrap hintRuleGhcRHS
        , hintRuleGhcSide=wrap <$> hintRuleGhcSide } $ do
    (l, v1) <- dotVersion' hintRuleGhcLHS
    (r, v2) <- dotVersion' hintRuleGhcRHS

    guard $ v1 == v2 && not (null l) && (length l > 1 || length r > 1) && Set.notMember v1 (Set.map occNameString (freeVars' $ maybeToList hintRuleGhcSide ++ l ++ r))
    if not (null r) then
      [ m{ hintRuleGhcLHS=wrap (dotApps' l), hintRuleGhcRHS=wrap (dotApps' r), hintRuleGhcSide=wrap <$> hintRuleGhcSide }
      , m{ hintRuleGhcLHS=wrap (dotApps' (l ++ [strToVar' v1])), hintRuleGhcRHS=wrap (dotApps' (r ++ [strToVar' v1])), hintRuleGhcSide=wrap <$> hintRuleGhcSide } ]
      else if length l > 1 then
            [ m{ hintRuleGhcLHS=wrap (dotApps' l), hintRuleGhcRHS=wrap (strToVar' "id"), hintRuleGhcSide=wrap <$> hintRuleGhcSide }
            , m{ hintRuleGhcLHS=wrap (dotApps' (l++[strToVar' v1])), hintRuleGhcRHS=wrap (strToVar' v1), hintRuleGhcSide=wrap <$> hintRuleGhcSide}]
      else []

-- old

-- find a dot version of this rule, return the sequence of app prefixes, and the var
dotVersion :: Exp_ -> [([Exp_], String)]
dotVersion (view -> Var_ v) | isUnifyVar v = [([], v)]
dotVersion (App l ls rs) = first (ls :) <$> dotVersion (fromParen rs)
dotVersion (InfixApp l x op y) = (first (LeftSection l x op :) <$> dotVersion y) ++
                                 (first (RightSection l op y:) <$> dotVersion x)
dotVersion _ = []

-- new

-- Find a dot version of this rule, return the sequence of app
-- prefixes, and the var.
dotVersion' :: GHC.LHsExpr GHC.GhcPs -> [([GHC.LHsExpr GHC.GhcPs], String)]
dotVersion' (view' -> Var_' v) | isUnifyVar v = [([], v)]
dotVersion' (GHC.LL _ (GHC.HsApp _ ls rs)) = first (ls :) <$> dotVersion' (fromParen' rs)
dotVersion' (GHC.LL l (GHC.OpApp _ x op y)) =
  -- In a GHC parse tree, raw sections aren't valid application terms.
  -- To be suitable as application terms, they must be enclosed in
  -- parentheses.

  --   If a == b then
  --   x is 'a', op is '==' and y is 'b' and,
  let lSec = addParen' (GHC.cL l (GHC.SectionL GHC.noExt x op)) -- (a == )
      rSec = addParen' (GHC.cL l (GHC.SectionR GHC.noExt op y)) -- ( == b)
  in (first (lSec :) <$> dotVersion' y) ++ (first (rSec :) <$> dotVersion' x) -- [([(a ==)], b), ([(b == )], a])].
dotVersion' _ = []

---------------------------------------------------------------------
-- PERFORM THE MATCHING

-- old

findIdeas :: [HintRule] -> Scope -> ModuleEx -> Decl_ -> [Idea]
findIdeas matches s _ decl = timed "Hint" "Match apply" $ forceList
    [ (idea (hintRuleSeverity m) (hintRuleName m) x y [r]){ideaNote=notes}
    | decl <- findDecls decl
    , (parent,x) <- universeParentExp decl
    , m <- matches, Just (y,notes, subst) <- [matchIdea s decl m parent x]
    , let r = R.Replace R.Expr (toSS x) subst (prettyPrint $ hintRuleRHS m) ]

-- new

findIdeas' :: [HintRule] -> Scope' -> ModuleEx -> GHC.LHsDecl GHC.GhcPs -> [Idea]
findIdeas' matches s _ decl = timed "Hint" "Match apply" $ forceList
    [ (idea' (hintRuleSeverity m) (hintRuleName m) x y [r]){ideaNote=notes}
    | decl <- findDecls' decl
    , (parent,x) <- universeParentExp' decl
    , m <- matches, Just (y, notes, subst) <- [matchIdea' s decl m parent x]
    , let r = R.Replace R.Expr (toSS' x) subst (unsafePrettyPrint $ unwrap (hintRuleGhcRHS m))
    ]

-- old

findDecls :: Decl_ -> [Decl_]
findDecls x@InstDecl{} = children x
findDecls RulePragmaDecl{} = []  -- often rules contain things that HLint would rewrite
findDecls x = [x]

-- new

findDecls' :: GHC.LHsDecl GHC.GhcPs -> [GHC.LHsDecl GHC.GhcPs]
findDecls' x@(GHC.LL _ GHC.InstD{}) = children x
findDecls' (GHC.LL _ GHC.RuleD{}) = [] -- Often rules contain things that HLint would rewrite.
findDecls' x = [x]

-- old

matchIdea :: Scope -> Decl_ -> HintRule -> Maybe (Int, Exp_) -> Exp_ -> Maybe (Exp_, [Note], [(String, R.SrcSpan)])
matchIdea s decl HintRule{..} parent x = do
    let nm a b = scopeMatch (hintRuleScope,a) (s,b)
    u <- unifyExp nm True hintRuleLHS x
    u <- validSubst (=~=) u
    -- need to check free vars before unqualification, but after subst (with e)
    -- need to unqualify before substitution (with res)
    let e = substitute u hintRuleRHS
        res = addBracket parent $ performSpecial $ substitute u $ unqualify hintRuleScope s hintRuleRHS
    guard $ (freeVars e Set.\\ Set.filter (not . isUnifyVar) (freeVars hintRuleRHS))
            `Set.isSubsetOf` freeVars x
        -- check no unexpected new free variables

    -- check it isn't going to get broken by QuasiQuotes as per #483
    -- if we have lambdas we might be moving, and QuasiQuotes, we might inadvertantly break free vars
    -- because quasi quotes don't show what free vars they make use of
    guard $ not (any isLambda $ universe hintRuleLHS) || not (any isQuasiQuote $ universe x)

    guard $ checkSide hintRuleSide $ ("original",x) : ("result",res) : fromSubst u
    guard $ checkDefine decl parent res
    return (res, hintRuleNotes, [(s, toSS pos) | (s, pos) <- fromSubst u, ann pos /= an])

-- new

matchIdea' :: Scope'
           -> GHC.LHsDecl GHC.GhcPs
           -> HintRule
           -> Maybe (Int, GHC.LHsExpr GHC.GhcPs)
           -> GHC.LHsExpr GHC.GhcPs
           -> Maybe (GHC.LHsExpr GHC.GhcPs, [Note], [(String, R.SrcSpan)])
matchIdea' sb decl HintRule{..} parent x = do
  let lhs = unwrap hintRuleGhcLHS
      rhs = unwrap hintRuleGhcRHS
      sa  = unwrap hintRuleGhcScope
      nm a b = scopeMatch' (sa, a) (sb, b)
  u <- unifyExp' nm True lhs x
  u <- validSubst' eqNoLoc' u

  -- Need to check free vars before unqualification, but after subst
  -- (with 'e') need to unqualify before substitution (with 'res').
  let e = substitute' u rhs
      res = addBracket' parent $ performSpecial' $ substitute' u $ unqualify' sa sb rhs
  guard $ (freeVars' e Set.\\ Set.filter (not . isUnifyVar . occNameString) (freeVars' rhs)) `Set.isSubsetOf` freeVars' x
      -- Check no unexpected new free variables.

  -- Check it isn't going to get broken by QuasiQuotes as per #483. If
  -- we have lambdas we might be moving, and QuasiQuotes, we might
  -- inadvertantly break free vars because quasi quotes don't show
  -- what free vars they make use of.
  guard $ not (any isLambda' $ universe lhs) || not (any isQuasiQuote' $ universe x)

  guard $ checkSide' (unwrap <$> hintRuleGhcSide) $ ("original", x) : ("result", res) : fromSubst' u
  guard $ checkDefine' decl parent res

  return (res, hintRuleNotes, [(s, toSS' pos) | (s, pos) <- fromSubst' u, GHC.getLoc pos /= GHC.noSrcSpan])

---------------------------------------------------------------------
-- SIDE CONDITIONS

-- old

checkSide :: Maybe Exp_ -> [(String, Exp_)] -> Bool
checkSide x bind = maybe True bool x
    where
        bool :: Exp_ -> Bool
        bool (InfixApp _ x op y)
            | opExp op ~= "&&" = bool x && bool y
            | opExp op ~= "||" = bool x || bool y
            | opExp op ~= "==" = expr (fromParen1 x) =~= expr (fromParen1 y)
        bool (App _ x y) | x ~= "not" = not $ bool y
        bool (Paren _ x) = bool x

        bool (App _ cond (sub -> y))
            | 'i':'s':typ <- fromNamed cond = isType typ y
        bool (App _ (App _ cond (sub -> x)) (sub -> y))
            | cond ~= "notIn" = and [x `notElem` universe y | x <- list x, y <- list y]
            | cond ~= "notEq" = x /=~= y
        bool x | x ~= "noTypeCheck" = True
        bool x | x ~= "noQuickCheck" = True
        bool x = error $ "Hint.Match.checkSide, unknown side condition: " ++ prettyPrint x

        expr :: Exp_ -> Exp_
        expr (App _ (fromNamed -> "subst") x) = sub $ fromParen1 x
        expr x = x

        isType "Compare" x = True -- just a hint for proof stuff
        isType "Atom" x = isAtom x
        isType "WHNF" x = isWHNF x
        isType "Wildcard" x = any isFieldWildcard $ universeS x
        isType "Nat" (asInt -> Just x) | x >= 0 = True
        isType "Pos" (asInt -> Just x) | x >  0 = True
        isType "Neg" (asInt -> Just x) | x <  0 = True
        isType "NegZero" (asInt -> Just x) | x <= 0 = True
        isType ('L':'i':'t':typ@(_:_)) (Lit _ x) =
           -- This case only comes up in the tests with 'LitInt'.
          let top = head (words $ show x) in
          typ == top
        isType typ x =
          let top = head (words $ show x) in
          typ == top

        asInt :: Exp_ -> Maybe Integer
        asInt (Paren _ x) = asInt x
        asInt (NegApp _ x) = negate <$> asInt x
        asInt (Lit _ (Int _ x _)) = Just x
        asInt _ = Nothing

        list :: Exp_ -> [Exp_]
        list (List _ xs) = xs
        list x = [x]

        sub :: Exp_ -> Exp_
        sub = transform f
            where f (view -> Var_ x) | Just y <- lookup x bind = y
                  f x = x

-- new

checkSide' :: Maybe (GHC.LHsExpr GHC.GhcPs) -> [(String, GHC.LHsExpr GHC.GhcPs)] -> Bool
checkSide' x bind = maybe True bool x
    where
      bool :: GHC.LHsExpr GHC.GhcPs -> Bool
      bool (GHC.LL _ (GHC.OpApp _ x op y))
        | varToStr' op == "&&" = bool x && bool y
        | varToStr' op == "||" = bool x || bool y
        | varToStr' op == "==" = expr (fromParen1' x) `eqNoLoc'` expr (fromParen1' y)
      bool (GHC.LL _ (GHC.HsApp _ x y)) | varToStr' x == "not" = not $ bool y
      bool (GHC.LL _ (GHC.HsPar _ x)) = bool x

      bool (GHC.LL _ (GHC.HsApp _ cond (sub -> y)))
        | 'i' : 's' : typ <- varToStr' cond = isType typ y
      bool (GHC.LL _ (GHC.HsApp _ (GHC.LL _ (GHC.HsApp _ cond (sub -> x))) (sub -> y)))
          | varToStr' cond == "notIn" = and [wrap (stripLocs' x) `notElem` map (wrap . stripLocs') (universe y) | x <- list x, y <- list y]
          | varToStr' cond == "notEq" = not (x `eqNoLoc'` y)
      bool x | varToStr' x == "noTypeCheck" = True
      bool x | varToStr' x == "noQuickCheck" = True
      bool x = error $ "Hint.Match.checkSide', unknown side condition: " ++ unsafePrettyPrint x

      expr :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
      expr (GHC.LL _ (GHC.HsApp _ (varToStr' -> "subst") x)) = sub $ fromParen1' x
      expr x = x

      isType "Compare" x = True -- Just a hint for proof stuff
      isType "Atom" x = isAtom' x
      isType "WHNF" x = isWHNF' x
      isType "Wildcard" x = any isFieldPun' (universeBi x) || any hasFieldsDotDot' (universeBi x)
      isType "Nat" (asInt -> Just x) | x >= 0 = True
      isType "Pos" (asInt -> Just x) | x >  0 = True
      isType "Neg" (asInt -> Just x) | x <  0 = True
      isType "NegZero" (asInt -> Just x) | x <= 0 = True
      isType "LitInt" (GHC.LL _ (GHC.HsLit _ GHC.HsInt{})) = True
      isType "LitInt" (GHC.LL _ (GHC.HsOverLit _ (GHC.OverLit _ GHC.HsIntegral{} _))) = True
      isType "Var" (GHC.LL _ GHC.HsVar{}) = True
      isType "App" (GHC.LL _ GHC.HsApp{}) = True
      isType "InfixApp" (GHC.LL _ x@GHC.OpApp{}) = True
      isType "Paren" (GHC.LL _ x@GHC.HsPar{}) = True
      isType "Tuple" (GHC.LL _ GHC.ExplicitTuple{}) = True

      isType typ (GHC.LL _ x) =
        let top = showConstr (toConstr x) in
        typ == top
      isType _ _ = False -- {-# COMPLETE LL#-}

      asInt :: GHC.LHsExpr GHC.GhcPs -> Maybe Integer
      asInt (GHC.LL _ (GHC.HsPar _ x)) = asInt x
      asInt (GHC.LL _ (GHC.NegApp _ x _)) = negate <$> asInt x
      asInt (GHC.LL _ (GHC.HsLit _ (GHC.HsInt _ (GHC.IL _ neg x)) )) = Just $ if neg then -x else x
      asInt (GHC.LL _ (GHC.HsOverLit _ (GHC.OverLit _ (GHC.HsIntegral (GHC.IL _ neg x)) _))) = Just $ if neg then -x else x
      asInt _ = Nothing

      list :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
      list (GHC.LL _ (GHC.ExplicitList _ _ xs)) = xs
      list x = [x]

      sub :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
      sub = transform f
        where f (view' -> Var_' x) | Just y <- lookup x bind = y
              f x = x

-- old

-- does the result look very much like the declaration
checkDefine :: Decl_ -> Maybe (Int, Exp_) -> Exp_ -> Bool
checkDefine x Nothing y = fromNamed x /= fromNamed (transformBi unqual $ head $ fromApps y)
checkDefine _ _ _ = True

-- new

-- Does the result look very much like the declaration?
checkDefine' :: GHC.LHsDecl GHC.GhcPs -> Maybe (Int, GHC.LHsExpr GHC.GhcPs) -> GHC.LHsExpr GHC.GhcPs -> Bool
checkDefine' x Nothing y = declName x /= Just (varToStr' (transformBi unqual' $ head $ fromApps' y))
checkDefine' _ _ _ = True

---------------------------------------------------------------------
-- TRANSFORMATION

-- old

-- if it has _eval_ do evaluation on it
performSpecial :: Exp_ -> Exp_
performSpecial = transform fNoParen . fEval
    where
        fEval (App _ e x) | e ~= "_eval_" = reduce x
        fEval x = x

        fNoParen (App _ e x) | e ~= "_noParen_" = fromParen x
        fNoParen x = x

-- new

-- If it has '_eval_' do evaluation on it.
performSpecial' :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
performSpecial' = transform fNoParen . fEval
  where
    fEval, fNoParen :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
    fEval (GHC.LL _ (GHC.HsApp _ e x)) | varToStr' e == "_eval_" = reduce' x
    fEval x = x
    fNoParen (GHC.LL _ (GHC.HsApp _ e x)) | varToStr' e == "_noParen_" = fromParen' x
    fNoParen x = x

-- old

-- contract Data.List.foo ==> foo, if Data.List is loaded
unqualify :: Scope -> Scope -> Exp_ -> Exp_
unqualify from to = transformBi f
    where
        f x@(UnQual _ (Ident _ s)) | isUnifyVar s = x
        f x = scopeMove (from,x) to

-- new

-- Contract : 'Data.List.foo' => 'foo' if 'Data.List' is loaded.
unqualify' :: Scope' -> Scope' -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
unqualify' from to = transformBi f
  where
    f :: GHC.Located RdrName -> GHC.Located RdrName
    f x@(GHC.L _ (Unqual s)) | isUnifyVar (occNameString s) = x
    f x = scopeMove' (from, x) to

-- old

addBracket :: Maybe (Int,Exp_) -> Exp_ -> Exp_
addBracket (Just (i,p)) c | needBracketOld i p c = Paren an c
addBracket _ x = x

-- new

addBracket' :: Maybe (Int, GHC.LHsExpr GHC.GhcPs) -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
addBracket' (Just (i, p)) c | needBracketOld' i p c = GHC.noLoc $ GHC.HsPar GHC.noExt c
addBracket' _ x = x

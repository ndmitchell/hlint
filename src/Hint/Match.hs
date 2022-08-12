{-# LANGUAGE RecordWildCards, NamedFieldPuns, TupleSections #-}
{-# LANGUAGE PatternGuards, ViewPatterns, FlexibleContexts #-}

{-
The matching does a fairly simple unification between the two terms, treating
any single letter variable on the left as a free variable. After the matching
we substitute, transform and check the side conditions. We also "see through"
both ($) and (.) functions on the right.

TRANSFORM PATTERNS
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

module Hint.Match(readMatch) where

import Hint.Type (ModuleEx,Idea,idea,ideaNote,toSSA)

import Util
import Timing
import qualified Data.Set as Set
import qualified Refact.Types as R

import Control.Monad
import Data.Tuple.Extra
import Data.Maybe
import Config.Type
import Data.Generics.Uniplate.DataOnly

import GHC.Data.Bag
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence
import Data.Data
import GHC.Util
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader

readMatch :: [HintRule] -> Scope -> ModuleEx -> LHsDecl GhcPs -> [Idea]
readMatch settings = findIdeas (concatMap readRule settings)

readRule :: HintRule -> [HintRule]
readRule m@HintRule{ hintRuleLHS=(stripLocs . unextendInstances -> hintRuleLHS)
                    , hintRuleRHS=(stripLocs . unextendInstances -> hintRuleRHS)
                    , hintRuleSide=((stripLocs . unextendInstances <$>) -> hintRuleSide)
                    } =
   (:) m{ hintRuleLHS=extendInstances hintRuleLHS
        , hintRuleRHS=extendInstances hintRuleRHS
        , hintRuleSide=extendInstances <$> hintRuleSide } $ do
    (l, v1) <- dotVersion hintRuleLHS
    (r, v2) <- dotVersion hintRuleRHS

    guard $ v1 == v2 && not (null l) && (length l > 1 || length r > 1) && Set.notMember v1 (Set.map occNameString (freeVars $ maybeToList hintRuleSide ++ l ++ r))
    if not (null r) then
      [ m{ hintRuleLHS=extendInstances (dotApps l), hintRuleRHS=extendInstances (dotApps r), hintRuleSide=extendInstances <$> hintRuleSide }
      , m{ hintRuleLHS=extendInstances (dotApps (l ++ [strToVar v1])), hintRuleRHS=extendInstances (dotApps (r ++ [strToVar v1])), hintRuleSide=extendInstances <$> hintRuleSide } ]
      else if length l > 1 then
            [ m{ hintRuleLHS=extendInstances (dotApps l), hintRuleRHS=extendInstances (strToVar "id"), hintRuleSide=extendInstances <$> hintRuleSide }
            , m{ hintRuleLHS=extendInstances (dotApps (l++[strToVar v1])), hintRuleRHS=extendInstances (strToVar v1), hintRuleSide=extendInstances <$> hintRuleSide}]
      else []

-- Find a dot version of this rule, return the sequence of app
-- prefixes, and the var.
dotVersion :: LHsExpr GhcPs -> [([LHsExpr GhcPs], String)]
dotVersion (view -> Var_ v) | isUnifyVar v = [([], v)]
dotVersion (L _ (HsApp _ ls rs)) = first (ls :) <$> dotVersion (fromParen rs)
dotVersion (L l (OpApp _ x op y)) =
  -- In a GHC parse tree, raw sections aren't valid application terms.
  -- To be suitable as application terms, they must be enclosed in
  -- parentheses.

  --   If a == b then
  --   x is 'a', op is '==' and y is 'b' and,
  let lSec = addParen (L l (SectionL EpAnnNotUsed x op)) -- (a == )
      rSec = addParen (L l (SectionR EpAnnNotUsed op y)) -- ( == b)
  in (first (lSec :) <$> dotVersion y) ++ (first (rSec :) <$> dotVersion x) -- [([(a ==)], b), ([(b == )], a])].
dotVersion _ = []

---------------------------------------------------------------------
-- PERFORM THE MATCHING

findIdeas :: [HintRule] -> Scope -> ModuleEx -> LHsDecl GhcPs -> [Idea]
findIdeas matches s _ decl = timed "Hint" "Match apply" $ forceList
    [ (idea (hintRuleSeverity m) (hintRuleName m) (reLoc x) (reLoc y) [r]){ideaNote=notes}
    | (name, expr) <- findDecls decl
    , (parent,x) <- universeParentExp expr
    , m <- matches, Just (y, tpl, notes, subst) <- [matchIdea s name m parent x]
    , let r = R.Replace R.Expr (toSSA x) subst (unsafePrettyPrint tpl)
    ]

-- | A list of root expressions, with their associated names
findDecls :: LHsDecl GhcPs -> [(String, LHsExpr GhcPs)]
findDecls x@(L _ (InstD _ (ClsInstD _ ClsInstDecl{cid_binds}))) =
    [(fromMaybe "" $ bindName xs, x) | xs <- bagToList cid_binds, x <- childrenBi xs]
findDecls (L _ RuleD{}) = [] -- Often rules contain things that HLint would rewrite.
findDecls x = map (fromMaybe "" $ declName x,) $ childrenBi x

matchIdea :: Scope
           -> String
           -> HintRule
           -> Maybe (Int, LHsExpr GhcPs)
           -> LHsExpr GhcPs
           -> Maybe (LHsExpr GhcPs, LHsExpr GhcPs, [Note], [(String, R.SrcSpan)])
matchIdea sb declName HintRule{..} parent x = do
  let lhs = unextendInstances hintRuleLHS
      rhs = unextendInstances hintRuleRHS
      sa  = hintRuleScope
      nm a b = scopeMatch (sa, a) (sb, b)

  (u, extra) <- unifyExp nm True lhs x
  u <- validSubst astEq u

  -- Need to check free vars before unqualification, but after subst
  -- (with 'e') need to unqualify before substitution (with 'res').
  let rhs' | Just fun <- extra = rebracket1 $ noLocA (HsApp EpAnnNotUsed fun rhs)
           | otherwise = rhs
      (e, (tpl, substNoParens)) = substitute u rhs'
      noParens = [varToStr $ fromParen x | L _ (HsApp _ (varToStr -> "_noParen_") x) <- universe tpl]

  u <- pure (removeParens noParens u)

  let res = addBracketTy (addBracket parent $ performSpecial $ fst $ substitute u $ unqualify sa sb rhs')
  guard $ (freeVars e Set.\\ Set.filter (not . isUnifyVar . occNameString) (freeVars rhs')) `Set.isSubsetOf` freeVars x
      -- Check no unexpected new free variables.

  -- Check it isn't going to get broken by QuasiQuotes as per #483. If
  -- we have lambdas we might be moving, and QuasiQuotes, we might
  -- inadvertantly break free vars because quasi quotes don't show
  -- what free vars they make use of.
  guard $ not (any isLambda $ universe lhs) || not (any isQuasiQuoteExpr $ universe x)

  guard $ checkSide (unextendInstances <$> hintRuleSide) $ ("original", x) : ("result", res) : fromSubst u
  guard $ checkDefine declName parent rhs

  (u, tpl) <- pure $ if any ((== noSrcSpan) . locA . getLoc . snd) (fromSubst u) then (mempty, res) else (u, tpl)
  tpl <- pure $ unqualify sa sb (addBracket parent $ performSpecial tpl)

  pure ( res, tpl, hintRuleNotes,
         [ (s, toSSA pos') | (s, pos) <- fromSubst u, locA (getLoc pos) /= noSrcSpan
                          , let pos' = if s `elem` substNoParens then fromParen pos else pos
         ]
       )

---------------------------------------------------------------------
-- SIDE CONDITIONS

checkSide :: Maybe (LHsExpr GhcPs) -> [(String, LHsExpr GhcPs)] -> Bool
checkSide x bind = maybe True bool x
    where
      bool :: LHsExpr GhcPs -> Bool
      bool (L _ (OpApp _ x op y))
        | varToStr op == "&&" = bool x && bool y
        | varToStr op == "||" = bool x || bool y
        | varToStr op == "==" = expr (fromParen1 x) `astEq` expr (fromParen1 y)
      bool (L _ (HsApp _ x y)) | varToStr x == "not" = not $ bool y
      bool (L _ (HsPar _ _ x _)) = bool x

      bool (L _ (HsApp _ cond (sub -> y)))
        | 'i' : 's' : typ <- varToStr cond = isType typ y
      bool (L _ (HsApp _ (L _ (HsApp _ cond (sub -> x))) (sub -> y)))
          | varToStr cond == "notIn" = and [extendInstances (stripLocs x) `notElem` map (extendInstances . stripLocs) (universe y) | x <- list x, y <- list y]
          | varToStr cond == "notEq" = not (x `astEq` y)
      bool x | varToStr x == "noTypeCheck" = True
      bool x | varToStr x == "noQuickCheck" = True
      bool x = error $ "Hint.Match.checkSide, unknown side condition: " ++ unsafePrettyPrint x

      expr :: LHsExpr GhcPs -> LHsExpr GhcPs
      expr (L _ (HsApp _ (varToStr -> "subst") x)) = sub $ fromParen1 x
      expr x = x

      isType "Compare" x = True -- Just a hint for proof stuff
      isType "Atom" x = isAtom x
      isType "WHNF" x = isWHNF x
      isType "Wildcard" x = any isFieldPun (universeBi x) || any hasFieldsDotDot (universeBi x)
      isType "Nat" (asInt -> Just x) | x >= 0 = True
      isType "Pos" (asInt -> Just x) | x >  0 = True
      isType "Neg" (asInt -> Just x) | x <  0 = True
      isType "NegZero" (asInt -> Just x) | x <= 0 = True
      isType "LitInt" (L _ (HsLit _ HsInt{})) = True
      isType "LitInt" (L _ (HsOverLit _ (OverLit _ HsIntegral{}))) = True
      isType "LitString" (L _ (HsLit _ HsString{})) = True
      isType "Var" (L _ HsVar{}) = True
      isType "App" (L _ HsApp{}) = True
      isType "InfixApp" (L _ x@OpApp{}) = True
      isType "Paren" (L _ x@HsPar{}) = True
      isType "Tuple" (L _ ExplicitTuple{}) = True

      isType typ (L _ x) =
        let top = showConstr (toConstr x) in
        typ == top

      asInt :: LHsExpr GhcPs -> Maybe Integer
      asInt (L _ (HsPar _ _ x _)) = asInt x
      asInt (L _ (NegApp _ x _)) = negate <$> asInt x
      asInt (L _ (HsLit _ (HsInt _ (IL _ _ x)) )) = Just x
      asInt (L _ (HsOverLit _ (OverLit _ (HsIntegral (IL _ _ x))))) = Just x
      asInt _ = Nothing

      list :: LHsExpr GhcPs -> [LHsExpr GhcPs]
      list (L _ (ExplicitList _ xs)) = xs
      list x = [x]

      sub :: LHsExpr GhcPs -> LHsExpr GhcPs
      sub = transform f
        where f (view -> Var_ x) | Just y <- lookup x bind = y
              f x = x

-- Does the result look very much like the declaration?
checkDefine :: String -> Maybe (Int, LHsExpr GhcPs) -> LHsExpr GhcPs -> Bool
checkDefine declName Nothing y =
  let funOrOp expr = (case expr of
        L _ (HsApp _ fun _) -> funOrOp fun
        L _ (OpApp _ _ op _) -> funOrOp op
        other -> other) :: LHsExpr GhcPs
   in declName /= varToStr (transformBi unqual $ funOrOp y)
checkDefine _ _ _ = True

---------------------------------------------------------------------
-- TRANSFORMATION

-- If it has '_noParen_', remove the brackets (if exist).
performSpecial :: LHsExpr GhcPs -> LHsExpr GhcPs
performSpecial = transform fNoParen
  where
    fNoParen :: LHsExpr GhcPs -> LHsExpr GhcPs
    fNoParen (L _ (HsApp _ e x)) | varToStr e == "_noParen_" = fromParen x
    fNoParen x = x

-- Contract : 'Data.List.foo' => 'foo' if 'Data.List' is loaded.
unqualify :: Scope -> Scope -> LHsExpr GhcPs -> LHsExpr GhcPs
unqualify from to = transformBi f
  where
    f :: LocatedN RdrName -> LocatedN RdrName
    f x@(L _ (Unqual s)) | isUnifyVar (occNameString s) = x
    f x = scopeMove (from, x) to

addBracket :: Maybe (Int, LHsExpr GhcPs) -> LHsExpr GhcPs -> LHsExpr GhcPs
addBracket (Just (i, p)) c | needBracketOld i p c = nlHsPar c
addBracket _ x = x

-- Type substitution e.g. 'Foo Int' for 'a' in 'Proxy a' can lead to a
-- need to bracket type applications in  This doesn't come up in HSE
-- because the pretty printer inserts them.
addBracketTy :: LHsExpr GhcPs -> LHsExpr GhcPs
addBracketTy= transformBi f
  where
    f :: LHsType GhcPs -> LHsType GhcPs
    f (L _ (HsAppTy _ t x@(L _ HsAppTy{}))) =
      noLocA (HsAppTy noExtField t (noLocA (HsParTy EpAnnNotUsed x)))
    f x = x

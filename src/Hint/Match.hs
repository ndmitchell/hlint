{-# LANGUAGE PatternGuards, ViewPatterns, RecordWildCards, FlexibleContexts, ScopedTypeVariables #-}

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

module Hint.Match(readMatch) where

import Control.Applicative
import Data.List.Extra
import Data.Maybe
import Config.Type
import Hint.Type
import Control.Monad
import Data.Tuple.Extra
import HSE.Unify
import qualified Data.Set as Set
import Prelude
import qualified Refact.Types as R


fmapAn = fmap (const an)


---------------------------------------------------------------------
-- READ THE RULE

readMatch :: [HintRule] -> DeclHint
readMatch settings = findIdeas (concatMap readRule settings)


readRule :: HintRule -> [HintRule]
readRule (m@HintRule{hintRuleLHS=(fmapAn -> hintRuleLHS), hintRuleRHS=(fmapAn -> hintRuleRHS), hintRuleSide=(fmap fmapAn -> hintRuleSide)}) =
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


-- find a dot version of this rule, return the sequence of app prefixes, and the var
dotVersion :: Exp_ -> [([Exp_], String)]
dotVersion (view -> Var_ v) | isUnifyVar v = [([], v)]
dotVersion (App l ls rs) = first (ls :) <$> dotVersion (fromParen rs)
dotVersion (InfixApp l x op y) = (first (LeftSection l x op :) <$> dotVersion y) ++
                                 (first (RightSection l op y:) <$> dotVersion x)
dotVersion _ = []


---------------------------------------------------------------------
-- PERFORM THE MATCHING

findIdeas :: [HintRule] -> Scope -> Module S -> Decl_ -> [Idea]
findIdeas matches s _ decl =
    [ (idea (hintRuleSeverity m) (hintRuleName m) x y [r]){ideaNote=notes}
    | decl <- case decl of InstDecl{} -> children decl; _ -> [decl]
    , (parent,x) <- universeParentExp decl, not $ isParen x
    , m <- matches, Just (y,notes, subst) <- [matchIdea s decl m parent x]
    , let r = R.Replace R.Expr (toSS x) subst (prettyPrint $ hintRuleRHS m) ]

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
    guard $ checkSide hintRuleSide $ ("original",x) : ("result",res) : fromSubst u
    guard $ checkDefine decl parent res
    return (res, hintRuleNotes, [(s, toSS pos) | (s, pos) <- fromSubst u, ann pos /= an])


---------------------------------------------------------------------
-- SIDE CONDITIONS

checkSide :: Maybe Exp_ -> [(String, Exp_)] -> Bool
checkSide x bind = maybe True f x
    where
        f (InfixApp _ x op y)
            | opExp op ~= "&&" = f x && f y
            | opExp op ~= "||" = f x || f y
        f (App _ x y) | x ~= "not" = not $ f y
        f (Paren _ x) = f x

        f (App _ cond (sub -> y))
            | 'i':'s':typ <- fromNamed cond
            = isType typ y
        f (App _ (App _ cond (sub -> x)) (sub -> y))
            | cond ~= "notIn" = and [x `notElem` universe y | x <- list x, y <- list y]
            | cond ~= "notEq" = x /=~= y
        f x | x ~= "noTypeCheck" = True
        f x | x ~= "noQuickCheck" = True
        f x = error $ "Hint.Match.checkSide, unknown side condition: " ++ prettyPrint x

        isType "Compare" x = True -- just a hint for proof stuff
        isType "Atom" x = isAtom x
        isType "WHNF" x = isWHNF x
        isType "Wildcard" x = any isFieldWildcard $ universeS x
        isType "Nat" (asInt -> Just x) | x >= 0 = True
        isType "Pos" (asInt -> Just x) | x >  0 = True
        isType "Neg" (asInt -> Just x) | x <  0 = True
        isType "NegZero" (asInt -> Just x) | x <= 0 = True
        isType ('L':'i':'t':typ@(_:_)) (Lit _ x) = head (words $ show x) == typ
        isType typ x = head (words $ show x) == typ

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


-- does the result look very much like the declaration
checkDefine :: Decl_ -> Maybe (Int, Exp_) -> Exp_ -> Bool
checkDefine x Nothing y = fromNamed x /= fromNamed (transformBi unqual $ head $ fromApps y)
checkDefine _ _ _ = True


---------------------------------------------------------------------
-- TRANSFORMATION

-- if it has _eval_ do evaluation on it
performSpecial :: Exp_ -> Exp_
performSpecial = transform fNoParen . fEval
    where
        fEval (App _ e x) | e ~= "_eval_" = reduce x
        fEval x = x

        fNoParen (App _ e x) | e ~= "_noParen_" = fromParen x
        fNoParen x = x

-- contract Data.List.foo ==> foo, if Data.List is loaded
unqualify :: Scope -> Scope -> Exp_ -> Exp_
unqualify from to = transformBi f
    where
        f x@(UnQual _ (Ident _ s)) | isUnifyVar s = x
        f x = scopeMove (from,x) to


addBracket :: Maybe (Int,Exp_) -> Exp_ -> Exp_
addBracket (Just (i,p)) c | needBracket i p c = Paren an c
addBracket _ x = x

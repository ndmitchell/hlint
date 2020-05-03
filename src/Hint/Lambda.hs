{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Concept:
    Remove all the lambdas you can be inserting only sections
    Never create a right section with +-# as the operator (they are misparsed)

    Rules:
    fun a = \x -> y  -- promote lambdas, provided no where's outside the lambda
    fun x = y x  -- eta reduce, x /= mr and foo /= symbol
    \x -> y x ==> y -- eta reduce
    ((#) x) ==> (x #)  -- rotate operators
    (flip op x) ==> (`op` x)  -- rotate operators
    \x y -> x + y ==> (+)  -- insert operator
    \x y -> op y x ==> flip op
    \x -> x + y ==> (+ y)  -- insert section,
    \x -> op x y ==> (`op` y)  -- insert section
    \x -> y + x ==> (y +)  -- insert section
    \x -> \y -> ... ==> \x y -- lambda compression
    \x -> (x +) ==> (+) -- operator reduction

<TEST>
f a = \x -> x + x -- f a x = x + x
f a = \a -> a + a -- f _ a = a + a
f (Just a) = \a -> a + a -- f (Just _) a = a + a @NoRefactor
f a = \x -> x + x where _ = test
f (test -> a) = \x -> x + x
f = \x -> x + x -- f x = x + x
fun x y z = f x y z -- fun = f @NoRefactor: refactoring for eta reduce is not implemented
fun x y z = f x x y z -- fun x = f x x @NoRefactor
fun x y z = f g z -- fun x y = f g @NoRefactor
fun x = f . g $ x -- fun = f . g @NoRefactor
f = foo (\y -> g x . h $ y) -- g x . h
f = foo (\y -> g x . h $ y) -- @Message Avoid lambda
f = foo ((*) x) -- (x *) @NoRefactor
f = (*) x
f = foo (flip op x) -- (`op` x) @NoRefactor
f = foo (flip op x) -- @Message Use section @NoRefactor
foo x = bar (\ d -> search d table) -- (`search` table)
foo x = bar (\ d -> search d table) -- @Message Avoid lambda using `infix`
f = flip op x
f = foo (flip (*) x) -- (* x) @NoRefactor
f = foo (flip (-) x)
f = foo (\x y -> fun x y) -- @Warning fun
f = foo (\x y z -> fun x y z) -- @Warning fun
f = foo (\z -> f x $ z) -- f x
f = foo (\x y -> x + y) -- (+)
f = foo (\x -> x * y) -- @Suggestion (* y)
f = foo (\x -> x # y)
f = foo (\x -> \y -> x x y y) -- \x y -> x x y y
f = foo (\x -> \x -> foo x x) -- \_ x -> foo x x
f = foo (\(foo -> x) -> \y -> x x y y)
f = foo (\(x:xs) -> \x -> foo x x) -- \(_:xs) x -> foo x x @NoRefactor
f = foo (\x -> \y -> \z -> x x y y z z) -- \x y z -> x x y y z z
x ! y = fromJust $ lookup x y
f = foo (\i -> writeIdea (getClass i) i)
f = bar (flip Foo.bar x) -- (`Foo.bar` x) @NoRefactor
f = a b (\x -> c x d)  -- (`c` d)
yes = \x -> a x where -- a
yes = \x y -> op y x where -- flip op
f = \y -> nub $ reverse y where -- nub . reverse
f = \z -> foo $ bar $ baz z where -- foo . bar . baz
f = \z -> foo $ bar x $ baz z where -- foo . bar x . baz
f = \z -> foo $ z $ baz z where
f = \x -> bar map (filter x) where -- bar map . filter
f = bar &+& \x -> f (g x)
foo = [\column -> set column [treeViewColumnTitle := printf "%s (match %d)" name (length candidnates)]]
foo = [\x -> x]
foo = [\m x -> insert x x m]
foo a b c = bar (flux ++ quux) c where flux = a -- foo a b = bar (flux ++ quux) @NoRefactor
foo a b c = bar (flux ++ quux) c where flux = c
yes = foo (\x -> Just x) -- @Warning Just
foo = bar (\x -> (x `f`)) -- f
foo = bar (\x -> shakeRoot </> "src" </> x)
baz = bar (\x -> (x +)) -- (+) @NoRefactor
xs `withArgsFrom` args = f args
foo = bar (\x -> case x of Y z -> z) -- \(Y z) -> z @NoRefactor
yes = blah (\ x -> case x of A -> a; B -> b) -- \ case A -> a; B -> b @NoRefactor
yes = blah (\ x -> case x of A -> a; B -> b) -- @Note may require `{-# LANGUAGE LambdaCase #-}` adding to the top of the file @NoRefactor
no = blah (\ x -> case x of A -> a x; B -> b x)
yes = blah (\ x -> (y, x)) -- (y,) @NoRefactor
yes = blah (\ x -> (y, x, z+q)) -- (y, , z+q) @NoRefactor
yes = blah (\ x -> (y, x, y, u, v)) -- (y, , y, u, v) @NoRefactor
yes = blah (\ x -> (y, x, z+q)) -- @Note may require `{-# LANGUAGE TupleSections #-}` adding to the top of the file @NoRefactor
yes = blah (\ x -> (y, x, z+x))
tmp = map (\ x -> runST $ action x)
yes = map (\f -> dataDir </> f) dataFiles -- (dataDir </>) @NoRefactor
{-# LANGUAGE TypeApplications #-}; noBug545 = coerce ((<>) @[a])
{-# LANGUAGE QuasiQuotes #-}; authOAuth2 name = authOAuth2Widget [whamlet|Login via #{name}|] name
{-# LANGUAGE QuasiQuotes #-}; authOAuth2 = foo (\name -> authOAuth2Widget [whamlet|Login via #{name}|] name)
f = {- generates a hint using hlint.yaml only -} map (flip (,) "a") "123"
f = {- generates a hint using hlint.yaml only -} map ((,) "a") "123"
f = map (\s -> MkFoo s 0 s) ["a","b","c"]
</TEST>
-}


module Hint.Lambda(lambdaHint) where

import Hint.Type (DeclHint', Idea, Note(RequiresExtension), suggest', warn', toSS', suggestN', ideaNote)
import Util
import Data.List.Extra
import qualified Data.Set as Set
import Refact.Types hiding (RType(Match))
import Data.Generics.Uniplate.Operations (universe, universeBi, transformBi)

import BasicTypes
import GHC.Util.Brackets (isAtom')
import GHC.Util.FreeVars (free', allVars', freeVars', pvars', vars', varss')
import GHC.Util.HsExpr (allowLeftSection, allowRightSection, niceLambdaR', lambda)
import GHC.Util.Outputable
import GHC.Util.RdrName (rdrNameStr')
import GHC.Util.View
import GHC.Hs
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr (isTypeApp, isOpApp, isLambda, isQuasiQuote, isVar, isDol, strToVar)
import OccName
import RdrName
import SrcLoc

lambdaHint :: DeclHint'
lambdaHint _ _ x
    =  concatMap (uncurry lambdaExp) (universeParentBi x)
    ++ concatMap lambdaDecl (universe x)

lambdaDecl :: LHsDecl GhcPs -> [Idea]
lambdaDecl
    o@(L _ (ValD _
        origBind@FunBind {fun_id = L loc1 _, fun_matches =
            MG {mg_alts =
                L _ [L _ (Match _ ctxt@(FunRhs _ Prefix _) pats (GRHSs _ [L _ (GRHS _ [] origBody@(L loc2 _))] bind))]}}))
    | L _ (EmptyLocalBinds noExtField) <- bind
    , isLambda $ fromParen' origBody
    , null (universeBi pats :: [HsExpr GhcPs])
    = [warn' "Redundant lambda" o (gen pats origBody) [Replace Decl (toSS' o) s1 t1]]
    | length pats2 < length pats, pvars' (drop (length pats2) pats) `disjoint` varss' bind
    = [warn' "Eta reduce" (reform pats origBody) (reform pats2 bod2)
          [ -- Disabled, see apply-refact #3
            -- Replace Decl (toSS' $ reform' pats origBody) s2 t2]]
          ]]
    where reform :: [LPat GhcPs] -> LHsExpr GhcPs -> LHsDecl GhcPs
          reform ps b = L loc $ ValD noExtField $
            origBind
              {fun_matches = MG noExtField (noLoc [noLoc $ Match noExtField ctxt ps $ GRHSs noExtField [noLoc $ GRHS noExtField [] b] $ noLoc $ EmptyLocalBinds noExtField]) Generated}

          loc = combineSrcSpans loc1 loc2

          gen :: [LPat GhcPs] -> LHsExpr GhcPs -> LHsDecl GhcPs
          gen ps = uncurry reform . fromLambda . lambda ps

          (finalpats, body) = fromLambda . lambda pats $ origBody
          (pats2, bod2) = etaReduce pats origBody
          template fps = unsafePrettyPrint $ reform (zipWith munge ['a'..'z'] fps) varBody
          subts fps b = ("body", toSS' b) : zipWith (\x y -> ([x],y)) ['a'..'z'] (map toSS' fps)
          s1 = subts finalpats body
          --s2 = subts pats2 bod2
          t1 = template finalpats
          --t2 = template pats2 bod2
lambdaDecl _ = []


etaReduce :: [LPat GhcPs] -> LHsExpr GhcPs -> ([LPat GhcPs], LHsExpr GhcPs)
etaReduce (unsnoc -> Just (ps, view' -> PVar_' p)) (L _ (HsApp _ x (view' -> Var_' y)))
    | p == y
    , y `notElem` vars' x
    , not $ any isQuasiQuote $ universe x
    = etaReduce ps x
etaReduce ps (L loc (OpApp _ x (isDol -> True) y)) = etaReduce ps (L loc (HsApp noExtField x y))
etaReduce ps x = (ps, x)

--Section refactoring is not currently implemented.
lambdaExp :: Maybe (LHsExpr GhcPs) -> LHsExpr GhcPs -> [Idea]
lambdaExp _ o@(L _ (HsPar _ (L _ (HsApp _ oper@(L _ (HsVar _ (L _ (rdrNameOcc -> f)))) y))))
    | isSymOcc f -- is this an operator?
    , isAtom' y
    , allowLeftSection $ occNameString f
    , not $ isTypeApp y =
      [suggestN' "Use section" o $ noLoc $ HsPar noExtField $ noLoc $ SectionL noExtField y oper]

lambdaExp _ o@(L _ (HsPar _ (view' -> App2' (view' -> Var_' "flip") origf@(view' -> Var_' f) y)))
    | allowRightSection f, not $ "(" `isPrefixOf` f
    = [suggestN' "Use section" o $ noLoc $ HsPar noExtField $ noLoc $ SectionR noExtField origf y]
lambdaExp p o@(L _ HsLam{})
    | not $ any isOpApp p
    , (res, refact) <- niceLambdaR' [] o
    , not $ isLambda res
    , not $ any isQuasiQuote $ universe res
    , not $ "runST" `Set.member` Set.map occNameString (freeVars' o)
    , let name = "Avoid lambda" ++ (if countRightSections res > countRightSections o then " using `infix`" else "")
    = [(if isVar res then warn' else suggest') name o res (refact $ toSS' o)]
    where
        countRightSections :: LHsExpr GhcPs -> Int
        countRightSections x = length [() | L _ (SectionR _ (view' -> Var_' _) _) <- universe x]
lambdaExp p o@(SimpleLambda origPats origBody)
    | isLambda (fromParen' origBody)
    , null (universeBi origPats :: [HsExpr GhcPs]) -- TODO: I think this checks for view patterns only, so maybe be more explicit about that?
    , maybe True (not . isLambda) p =
    [suggest' "Collapse lambdas" o (lambda pats body) [Replace Expr (toSS' o) subts template]]
    where
      (pats, body) = fromLambda o

      template = unsafePrettyPrint $ lambda (zipWith munge ['a'..'z'] pats) varBody

      subts = ("body", toSS' body) : zipWith (\x y -> ([x],y)) ['a'..'z'] (map toSS' pats)

-- match a lambda with a variable pattern, with no guards and no where clauses
lambdaExp _ o@(SimpleLambda [view' -> PVar_' x] (L _ expr)) =
    case expr of
        -- suggest TupleSections instead of lambdas
        ExplicitTuple _ args boxity
            -- is there exactly one argument that is exactly x?
            | ([_x], ys) <- partition ((==Just x) . tupArgVar) args
            -- the other arguments must not have a nested x somewhere in them
            , Set.notMember x $ Set.map occNameString $ freeVars' ys
            -> [(suggestN' "Use tuple-section" o $ noLoc $ ExplicitTuple noExtField (map removeX args) boxity)
                  {ideaNote = [RequiresExtension "TupleSections"]}]

        -- suggest @LambdaCase@/directly matching in a lambda instead of doing @\x -> case x of ...@
        HsCase _ (view' -> Var_' x') matchGroup
            -- is the case being done on the variable from our original lambda?
            | x == x'
            -- x must not be used in some other way inside the matches
            , Set.notMember x $ Set.map occNameString $ free' $ allVars' matchGroup
            -> case matchGroup of
                 -- is there a single match? - suggest match inside the lambda
                 --
                 -- we need to
                 --     * add brackets to the match, because matches in lambdas require them
                 --     * mark match as being in a lambda context so that it's printed properly
                 oldMG@(MG _ (L _ [L _ oldmatch]) _) ->
                     [suggestN' "Use lambda" o $ noLoc $ HsLam noExtField oldMG
                         { mg_alts = noLoc
                             [noLoc oldmatch
                                 { m_pats = map mkParPat $ m_pats oldmatch
                                 , m_ctxt = LambdaExpr
                                 }
                             ] }
                     ]

                 -- otherwise we should use @LambdaCase@
                 MG _ (L _ xs) _ ->
                     [(suggestN' "Use lambda-case" o $ noLoc $ HsLamCase noExtField matchGroup)
                         {ideaNote=[RequiresExtension "LambdaCase"]}]
                 _ -> []
        _ -> []
    where
        -- | Filter out tuple arguments, converting the @x@ (matched in the lambda) variable argument
        -- to a missing argument, so that we get the proper section.
        removeX :: LHsTupArg GhcPs -> LHsTupArg GhcPs
        removeX arg@(L _ (Present _ (view' -> Var_' x')))
            | x == x' = noLoc $ Missing noExtField
        removeX y = y
        -- | Extract the name of an argument of a tuple if it's present and a variable.
        tupArgVar :: LHsTupArg GhcPs -> Maybe String
        tupArgVar (L _ (Present _ (view' -> Var_' x))) = Just x
        tupArgVar _ = Nothing

lambdaExp _ _ = []

varBody :: LHsExpr GhcPs
varBody = strToVar "body"

-- | Squash lambdas and replace any repeated pattern variable with @_@
fromLambda :: LHsExpr GhcPs -> ([LPat GhcPs], LHsExpr GhcPs)
fromLambda (SimpleLambda ps1 (fromLambda . fromParen' -> (ps2,x))) = (transformBi (f $ pvars' ps2) ps1 ++ ps2, x)
    where f :: [String] -> Pat GhcPs -> Pat GhcPs
          f bad (VarPat _ (rdrNameStr' -> x))
              | x `elem` bad = WildPat noExtField
          f bad x = x
fromLambda x = ([], x)

-- | Replaces all non-wildcard patterns with a variable pattern with the given identifier.
munge :: Char -> LPat GhcPs -> LPat GhcPs
munge ident p@(L _ (WildPat _)) = p
munge ident (L ploc p) = L ploc (VarPat noExtField (L ploc $ mkRdrUnqual $ mkVarOcc [ident]))

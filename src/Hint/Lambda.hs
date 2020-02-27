{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Concept:
    Remove all the lambdas you can be inserting only sections
    Never create a right section with +-# as the operator (they are misparsed)

    Rules:
    fun a = \x -> y  -- promote lambdas, provided no where's outside the lambda
    fun x = y x  -- eta reduce, x /= mr and foo /= symbol
    \x -> y x  -- eta reduce
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
f (Just a) = \a -> a + a -- f (Just _) a = a + a
f a = \x -> x + x where _ = test
f (test -> a) = \x -> x + x
f = \x -> x + x -- f x = x + x
fun x y z = f x y z -- fun = f @NoRefactor: hlint bug, ideaRefactoring = []
fun x y z = f x x y z -- fun x = f x x @NoRefactor
fun x y z = f g z -- fun x y = f g @NoRefactor
fun mr = y mr
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
baz = bar (\x -> (x +)) -- (+)
foo = bar (\x -> case x of Y z -> z) -- \(Y z) -> z @NoRefactor
yes = blah (\ x -> case x of A -> a; B -> b) -- \ case A -> a; B -> b @NoRefactor
yes = blah (\ x -> case x of A -> a; B -> b) -- @Note may require `{-# LANGUAGE LambdaCase #-}` adding to the top of the file
no = blah (\ x -> case x of A -> a x; B -> b x)
yes = blah (\ x -> (y, x)) -- (y,)
yes = blah (\ x -> (y, x, z+q)) -- (y, , z+q) @NoRefactor
yes = blah (\ x -> (y, x, y, u, v)) -- (y, , y, u, v)
yes = blah (\ x -> (y, x, z+q)) -- @Note may require `{-# LANGUAGE TupleSections #-}` adding to the top of the file
yes = blah (\ x -> (y, x, z+x))
tmp = map (\ x -> runST $ action x)
yes = map (\f -> dataDir </> f) dataFiles -- (dataDir </>)
{-# LANGUAGE TypeApplications #-}; noBug545 = coerce ((<>) @[a])
{-# LANGUAGE QuasiQuotes #-}; authOAuth2 name = authOAuth2Widget [whamlet|Login via #{name}|] name
{-# LANGUAGE QuasiQuotes #-}; authOAuth2 = foo (\name -> authOAuth2Widget [whamlet|Login via #{name}|] name)
</TEST>
-}


module Hint.Lambda(lambdaHint) where

import Hint.Type
import Util
import Data.List.Extra
import qualified Data.Set as Set
import Refact.Types hiding (RType(Match))

import qualified GHC.Util.Brackets as GHC (isAtom')
import qualified GHC.Util.FreeVars as GHC (free', allVars', freeVars', pvars', vars', varss')
import qualified GHC.Util.HsExpr as GHC (allowLeftSection, allowRightSection, niceLambdaR', lambda)
import qualified GHC.Util.RdrName as GHC (rdrNameStr')
import qualified GHC.Util.View as GHC
import qualified HsSyn as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Hs.Expr as GHC (isTypeApp, isOpApp, isLambda, isQuasiQuote, isVar, isDol)
import qualified OccName as GHC
import qualified RdrName as GHC
import qualified SrcLoc as GHC
import qualified GHC.Util.Outputable as GHC
import qualified BasicTypes as GHC

lambdaHint :: DeclHint'
lambdaHint _ _ x
    =  concatMap (uncurry lambdaExp') (universeParentBi x)
    ++ concatMap lambdaDecl' (universe x)

-- TODO: handle PatBinds
lambdaDecl' :: GHC.LHsDecl GHC.GhcPs -> [Idea]
lambdaDecl'
    o@(GHC.LL loc1 (GHC.ValD _
        origBind@(GHC.FunBind {GHC.fun_matches =
            (GHC.MG {GHC.mg_alts =
                GHC.LL _ [GHC.LL _ (GHC.Match _ ctxt pats (GHC.GRHSs _ [GHC.LL loc2 (GHC.GRHS _ [] origBody)] bind))]})})))
    | GHC.LL _ (GHC.EmptyLocalBinds noExt) <- bind
    , GHC.isLambda $ GHC.fromParen' origBody
    , null (universeBi pats :: [GHC.HsExpr GHC.GhcPs])
    = [warn' "Redundant lambda" o (gen pats origBody) [Replace Decl (toSS' o) s1 t1]]
    | length pats2 < length pats, GHC.pvars' (drop (length pats2) pats) `disjoint` GHC.varss' bind
    = [warn' "Eta reduce" (reform' pats origBody) (reform' pats2 bod2)
          [ -- Disabled, see apply-refact #3
            -- Replace Decl (toSS' $ reform' pats origBody) s2 t2]]
          ]]
    where reform' :: [GHC.LPat GHC.GhcPs] -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsDecl GHC.GhcPs
          reform' ps b = GHC.LL loc $ GHC.ValD GHC.noExt $
            origBind
              {GHC.fun_matches = GHC.MG GHC.noExt (GHC.noLoc [GHC.noLoc $ GHC.Match GHC.noExt ctxt ps $ GHC.GRHSs GHC.noExt [GHC.noLoc $ GHC.GRHS GHC.noExt [] b] $ GHC.noLoc $ GHC.EmptyLocalBinds GHC.noExt]) GHC.Generated}

          loc = GHC.combineSrcSpans loc1 loc2

          gen :: [GHC.Pat GHC.GhcPs] -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsDecl GHC.GhcPs
          gen ps = uncurry reform' . fromLambda' . GHC.lambda ps

          (finalpats, body) = fromLambda' . GHC.lambda pats $ origBody
          (pats2, bod2) = etaReduce' pats origBody
          template fps = GHC.unsafePrettyPrint $ reform' (zipWith munge' ['a'..'z'] fps) varBody
          subts fps b = ("body", toSS' b) : zipWith (\x y -> ([x],y)) ['a'..'z'] (map toSS' fps)
          s1 = subts finalpats body
          --s2 = subts pats2 bod2
          t1 = template finalpats
          --t2 = template pats2 bod2
lambdaDecl' _ = []


etaReduce' :: [GHC.Pat GHC.GhcPs] -> GHC.LHsExpr GHC.GhcPs -> ([GHC.Pat GHC.GhcPs], GHC.LHsExpr GHC.GhcPs)
etaReduce' (unsnoc -> Just (ps, GHC.view' -> GHC.PVar_' p)) (GHC.LL _ (GHC.HsApp _ x (GHC.view' -> GHC.Var_' y)))
    | p == y
    , p /= "mr"
    -- TODO: what is this "mr" thing??
    , y `notElem` GHC.vars' x
    , not $ any GHC.isQuasiQuote $ universe x
    = etaReduce' ps x
etaReduce' ps (GHC.LL loc (GHC.OpApp _ x (GHC.isDol -> True) y)) = etaReduce' ps (GHC.LL loc (GHC.HsApp GHC.noExt x y))
etaReduce' ps x = (ps, x)

--Section refactoring is not currently implemented.
lambdaExp' :: Maybe (GHC.LHsExpr GHC.GhcPs) -> GHC.LHsExpr GHC.GhcPs -> [Idea]
lambdaExp' _ o@(GHC.LL _ (GHC.HsPar _ (GHC.LL _ (GHC.HsApp _ oper@(GHC.LL _ (GHC.HsVar _ (GHC.LL _ (GHC.rdrNameOcc -> f)))) y))))
    | GHC.isSymOcc f -- is this an operator?
    , GHC.isAtom' y
    , GHC.allowLeftSection $ GHC.occNameString f
    , not $ GHC.isTypeApp y =
      [suggestN' "Use section" o $ GHC.noLoc $ GHC.HsPar GHC.noExt $ GHC.noLoc $ GHC.SectionL GHC.NoExt y oper]

lambdaExp' _ o@(GHC.LL _ (GHC.HsPar _ (GHC.view' -> GHC.App2' (GHC.view' -> GHC.Var_' "flip") origf@(GHC.view' -> GHC.Var_' f) y)))
    | GHC.allowRightSection f
    = [suggestN' "Use section" o $ GHC.noLoc $ GHC.HsPar GHC.NoExt $ GHC.noLoc $ GHC.SectionR GHC.NoExt origf y]
lambdaExp' p o@(GHC.LL _ GHC.HsLam{})
    | all (not . GHC.isOpApp) p
    , (res, refact) <- GHC.niceLambdaR' [] o
    , not $ GHC.isLambda res
    , not $ any GHC.isQuasiQuote $ universe res
    , not $ "runST" `Set.member` (Set.map GHC.occNameString $ GHC.freeVars' o)
    , let name = "Avoid lambda" ++ (if countRightSections res > countRightSections o then " using `infix`" else "")
    = [(if GHC.isVar res then warn' else suggest') name o res (refact $ toSS' o)]
    where
        countRightSections :: GHC.LHsExpr GHC.GhcPs -> Int
        countRightSections x = length [() | GHC.LL _ (GHC.SectionR _ (GHC.view' -> GHC.Var_' _) _) <- universe x]
lambdaExp' p o@(GHC.SimpleLambda origPats origBody)
    | GHC.isLambda (GHC.fromParen' origBody)
    , null (universeBi origPats :: [GHC.HsExpr GHC.GhcPs]) -- TODO: I think this checks for view patterns only, so maybe be more explicit about that?
    , maybe True (not . GHC.isLambda) p =
    [suggest' "Collapse lambdas" o (GHC.lambda pats body) [Replace Expr (toSS' o) subts template]]
    where
      (pats, body) = fromLambda' o

      template = GHC.unsafePrettyPrint $ GHC.lambda (zipWith munge' ['a'..'z'] pats) varBody

      subts = ("body", toSS' body) : zipWith (\x y -> ([x],y)) ['a'..'z'] (map toSS' pats)

lambdaExp' _ o@(GHC.SimpleLambda [GHC.LL _ (GHC.view' -> GHC.PVar_' x)] (GHC.LL _ expr)) =
-- ^ match a lambda with a variable pattern, with no guards and no where clauses
    case expr of
        GHC.ExplicitTuple _ args boxity
        -- ^ suggest TupleSections instead of lambdas
            | ([_x], ys) <- partition ((==Just x) . tupArgVar) args
            -- ^ is there exactly one argument that is exactly x
            , Set.notMember x $ Set.map GHC.occNameString $ GHC.freeVars' ys
            -> [(suggestN' "Use tuple-section" o $ GHC.noLoc $ GHC.ExplicitTuple GHC.NoExt (map removeX args) boxity)
                  {ideaNote = [RequiresExtension "TupleSections"]}]
            -- ^ the other arguments must not have a nested x somewhere in them
        GHC.HsCase _ (GHC.view' -> GHC.Var_' x') matchGroup
        -- ^ suggest LambdaCase/directly matching in a lambda instead of doing @\x -> case x of ...@
            | x == x'
            -- ^ is the case being done on the variable from our original lambda
            , Set.notMember x $ Set.map GHC.occNameString $ GHC.free' $ GHC.allVars' matchGroup
            -- ^ x must not be used in some other way inside the matches
            -> case matchGroup of
                 oldMG@(GHC.MG _ (GHC.LL _ [GHC.LL _ oldmatch]) _) ->
                 -- ^ is there a single match? - suggest match inside the lambda
                     [suggestN' "Use lambda" o $ GHC.noLoc $ GHC.HsLam GHC.NoExt oldMG
                         { GHC.mg_alts = GHC.noLoc
                             [GHC.noLoc oldmatch
                                 { GHC.m_pats = map GHC.mkParPat $ GHC.m_pats oldmatch
                                 , GHC.m_ctxt = GHC.LambdaExpr
                                 }
                             ] }
                         -- ^ we need to
                         -- * add brackets to the match
                         -- * mark match as being in a lambda context so that it's printed properly
                     ]
                 GHC.MG _ (GHC.LL _ xs) _ ->
                 -- ^ otherwise we should use LambdaCase
                     [(suggestN' "Use lambda-case" o $ GHC.noLoc $ GHC.HsLamCase GHC.NoExt matchGroup)
                         {ideaNote=[RequiresExtension "LambdaCase"]}]
                 _ -> []
        _ -> []
    where
        -- | Filter out tuple arguments, converting the @x@ (matched in the lambda) variable argument
        -- to an missing argument, so that we get the proper section.
        removeX :: GHC.LHsTupArg GHC.GhcPs -> GHC.LHsTupArg GHC.GhcPs
        removeX arg@(GHC.LL _ (GHC.Present _ (GHC.view' -> GHC.Var_' x')))
            | x == x' = GHC.noLoc $ GHC.Missing GHC.NoExt
        removeX y = y
        -- | Extract the name of an argument of a tuple if it's present and a variable.
        tupArgVar :: GHC.LHsTupArg GHC.GhcPs -> Maybe String
        tupArgVar (GHC.LL _ (GHC.Present _ (GHC.view' -> GHC.Var_' x))) = Just x
        tupArgVar _ = Nothing

lambdaExp' _ _ = []

varBody :: GHC.LHsExpr GHC.GhcPs
varBody = GHC.noLoc $ GHC.HsVar GHC.noExt $ GHC.noLoc $ GHC.mkRdrUnqual $ GHC.mkVarOcc "body"

-- | Squash lambdas and replace any repeated pattern variable with @_@
fromLambda' :: GHC.LHsExpr GHC.GhcPs -> ([GHC.LPat GHC.GhcPs], GHC.LHsExpr GHC.GhcPs)
fromLambda' (GHC.SimpleLambda ps1 (fromLambda' . GHC.fromParen' -> (ps2,x))) = (transformBi (f $ GHC.pvars' ps2) ps1 ++ ps2, x)
    where f :: [String] -> GHC.Pat GHC.GhcPs -> GHC.Pat GHC.GhcPs
          f bad (GHC.VarPat _ (GHC.rdrNameStr' -> x))
              | x `elem` bad = GHC.WildPat GHC.noExt
          f bad x = x
fromLambda' x = ([], x)

-- | Replaces all non-wildcard patterns with a variable pattern with the given identifier.
munge' :: Char -> GHC.LPat GHC.GhcPs -> GHC.LPat GHC.GhcPs
munge' ident p@(GHC.LL _ (GHC.WildPat _)) = p
munge' ident (GHC.LL ploc p) = GHC.LL ploc (GHC.VarPat GHC.noExt (GHC.LL ploc $ GHC.mkRdrUnqual $ GHC.mkVarOcc [ident]))
munge' _ x = x -- "{-# COMPLETE LL #-}"

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

import Hint.Util
import Hint.Type
import Util
import Data.List.Extra
import Data.Maybe
import qualified Data.Set as Set
import Refact.Types hiding (RType(Match))

import qualified GHC.Util.Brackets as GHC (isAtom')
import qualified GHC.Util.FreeVars as GHC (free', allVars', freeVars', pvars')
import qualified GHC.Util.HsExpr as GHC (allowLeftSection, allowRightSection, niceLambdaR', lambda)
import qualified GHC.Util.RdrName as GHC (rdrNameStr')
import qualified GHC.Util.View as GHC
import qualified HsSyn as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Hs.Expr as GHC (isTypeApp, isOpApp, isLambda, isQuasiQuote, isVar)
import qualified OccName as GHC
import qualified RdrName as GHC
import qualified SrcLoc as GHC
import qualified GHC.Util.Outputable as GHC

--lambdaHint :: DeclHint
--lambdaHint _ _ x = concatMap (uncurry lambdaExp) (universeParentBi x) ++ concatMap lambdaDecl (universe x)
lambdaHint :: DeclHint'
lambdaHint _ _ x
    =  concatMap (uncurry lambdaExp') (universeParentBi x)
    ++ concatMap lambdaDecl' (universe x)

lambdaDecl' :: GHC.LHsDecl GHC.GhcPs -> [Idea]
lambdaDecl' _ = []

lambdaDecl :: Decl_ -> [Idea]
lambdaDecl (toFunBind -> o@(FunBind loc1 [Match _ name pats (UnGuardedRhs loc2 bod) bind]))
    | isNothing bind, isLambda $ fromParen bod, null (universeBi pats :: [Exp_]) =
      [warn "Redundant lambda" o (gen pats bod) [Replace Decl (toSS o) s1 t1]]
    | length pats2 < length pats, pvars (drop (length pats2) pats) `disjoint` varss bind
        = [warn "Eta reduce" (reform pats bod) (reform pats2 bod2)
            [ -- Disabled, see apply-refact #3
              -- Replace Decl (toSS $ reform pats bod) s2 t2]]
            ]]
        where reform p b = FunBind loc [Match an name p (UnGuardedRhs an b) Nothing]
              loc = setSpanInfoEnd loc1 $ srcSpanEnd $ srcInfoSpan loc2
              gen ps = uncurry reform . fromLambda . Lambda an ps
              (finalpats, body) = fromLambda . Lambda an pats $ bod
              (pats2, bod2) = etaReduce pats bod
              template fps b = prettyPrint $ reform (zipWith munge ['a'..'z'] fps) (toNamed "body")
              munge :: Char -> Pat_ -> Pat_
              munge ident p@(PWildCard _) = p
              munge ident p = PVar (ann p) (Ident (ann p) [ident])
              subts fps b = ("body", toSS b) : zipWith (\x y -> ([x],y)) ['a'..'z'] (map toSS fps)
              s1 = subts finalpats body
              --s2 = subts pats2 bod2
              t1 = template finalpats body
              --t2 = template pats2 bod2

lambdaDecl _ = []

setSpanInfoEnd ssi (line, col) = ssi{srcInfoSpan = (srcInfoSpan ssi){srcSpanEndLine=line, srcSpanEndColumn=col}}


etaReduce :: [Pat_] -> Exp_ -> ([Pat_], Exp_)
etaReduce ps (App _ x (Var _ (UnQual _ (Ident _ y))))
    | ps /= [], PVar _ (Ident _ p) <- last ps, p == y, p /= "mr", y `notElem` vars x
    , not $ any isQuasiQuote $ universe x
    = etaReduce (init ps) x
etaReduce ps (InfixApp a x (isDol -> True) y) = etaReduce ps (App a x y)
etaReduce ps x = (ps,x)

--Section refactoring is not currently implemented.
lambdaExp' :: Maybe (GHC.LHsExpr GHC.GhcPs) -> GHC.LHsExpr GHC.GhcPs -> [Idea]
lambdaExp' _ o@(GHC.LL _ (GHC.HsPar _ (GHC.LL _ (GHC.HsApp _ oper@(GHC.LL _ (GHC.HsVar _ (GHC.LL _ (GHC.rdrNameOcc -> f)))) y))))
    | GHC.isSymOcc f -- is this an operator?
    , GHC.isAtom' y
    , GHC.allowLeftSection $ GHC.occNameString f
    , not $ GHC.isTypeApp y =
      [suggestN' "Use section" o $ GHC.noLoc $ GHC.HsPar GHC.noExt $ GHC.noLoc $ GHC.SectionL GHC.NoExt y oper]
    -- TODO:
    -- why check if y requires no bracketing here?
    -- is allowLeftSection still relevant?
    -- what is the section refactoring stuff?

lambdaExp' _ o@(GHC.LL _ (GHC.HsPar _ (GHC.LL _ (GHC.HsApp _ (GHC.LL _ (GHC.HsApp _ (GHC.LL _ (GHC.HsVar _ (GHC.rdrNameStr' -> "flip"))) origf@(GHC.LL _ (GHC.HsVar _ (GHC.rdrNameStr' -> f))))) y))))
    | GHC.allowRightSection f
    = [suggestN' "Use section" o $ GHC.LL GHC.noSrcSpan $ GHC.HsPar GHC.NoExt $ GHC.LL GHC.noSrcSpan $ GHC.SectionR GHC.NoExt origf y]
-- TODO: perhaps PatternSynonyms?
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

      template = GHC.unsafePrettyPrint $ GHC.lambda (zipWith munge ['a'..'z'] pats) $ GHC.noLoc $ GHC.HsVar GHC.noExt $ GHC.noLoc $ GHC.mkRdrUnqual $ GHC.mkVarOcc "body"

      munge :: Char -> GHC.LPat GHC.GhcPs -> GHC.LPat GHC.GhcPs
      munge ident p@(GHC.LL _ (GHC.WildPat _)) = p
      munge ident (GHC.LL ploc p) = GHC.LL ploc (GHC.VarPat GHC.noExt (GHC.LL ploc $ GHC.mkRdrUnqual $ GHC.mkVarOcc [ident]))
      munge _ x = x -- "{-# COMPLETE LL #-}"

      subts = ("body", toSS' body) : zipWith (\x y -> ([x],y)) ['a'..'z'] (map toSS' pats)

lambdaExp' _ o@(GHC.SimpleLambda [GHC.LL _ (GHC.view' -> GHC.PVar_' x)] (GHC.LL _ expr)) =
-- ^ match a lambda with a variable pattern, with no guards and no where clauses
    case expr of
        GHC.ExplicitTuple _ args boxity
        -- ^ suggest TupleSections instead of lambdas
            | ([_x], ys) <- partition ((==Just x) . tupArgVar) args
            -- ^ is there exactly one argument that is exactly x
            , Set.notMember x $ Set.map GHC.occNameString $ GHC.freeVars' ys
            -> [(suggestN' "Use tuple-section" o $ GHC.LL GHC.noSrcSpan $ GHC.ExplicitTuple GHC.NoExt (map removeX args) boxity)
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
                     [suggestN' "Use lambda" o $ GHC.LL GHC.noSrcSpan $ GHC.HsLam GHC.NoExt oldMG
                         { GHC.mg_alts = GHC.LL GHC.noSrcSpan
                             [GHC.LL GHC.noSrcSpan oldmatch
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
                     [(suggestN' "Use lambda-case" o $ GHC.LL GHC.noSrcSpan $ GHC.HsLamCase GHC.NoExt matchGroup)
                         {ideaNote=[RequiresExtension "LambdaCase"]}]
                 _ -> []
        _ -> []
    where
        -- | Filter out tuple arguments, converting the @x@ (matched in the lambda) variable argument
        -- to an missing argument, so that we get the proper section.
        removeX :: GHC.LHsTupArg GHC.GhcPs -> GHC.LHsTupArg GHC.GhcPs
        removeX arg@(GHC.LL _ (GHC.Present _ (GHC.view' -> GHC.Var_' x')))
            | x == x' = GHC.LL GHC.noSrcSpan $ GHC.Missing GHC.NoExt
        removeX y = y
        -- | Extract the name of an argument of a tuple if it's present and a variable.
        tupArgVar :: GHC.LHsTupArg GHC.GhcPs -> Maybe String
        tupArgVar (GHC.LL _ (GHC.Present _ (GHC.view' -> GHC.Var_' x))) = Just x
        tupArgVar _ = Nothing

lambdaExp' _ _ = []

--Section refactoring is not currently implemented.
lambdaExp :: Maybe Exp_ -> Exp_ -> [Idea]
lambdaExp p o@(Paren _ (App _ v@(Var l (UnQual _ (Symbol _ x))) y)) | isAtom y, not $ isTypeApp y, allowLeftSection x =
    [suggestN "Use section" o (exp y x)] -- [Replace Expr (toSS o) subts template]]
    where
      exp op rhs = LeftSection an op (toNamed rhs)
--      template = prettyPrint (exp (toNamed "a") "*")
--      subts = [("a", toSS y), ("*", toSS v)]
lambdaExp p o@(Paren _ (App _ (App _ (view -> Var_ "flip") (Var _ x)) y)) | allowRightSection $ fromNamed x =
    [suggestN "Use section" o $ RightSection an (QVarOp an x) y]
lambdaExp p o@Lambda{}
    | maybe True (not . isInfixApp) p, (res, refact) <- niceLambdaR [] o
    , not $ isLambda res, not $ any isQuasiQuote $ universe res, not $ "runST" `Set.member` freeVars o
    , let name = "Avoid lambda" ++ (if countInfixNames res > countInfixNames o then " using `infix`" else "") =
    [(if isVar res || isCon res then warn else suggest) name o res (refact $ toSS o)]
    where countInfixNames x = length [() | RightSection _ (QVarOp _ (UnQual _ (Ident _ _))) _ <- universe x]
lambdaExp p o@(Lambda _ pats x) | isLambda (fromParen x), null (universeBi pats :: [Exp_]), maybe True (not . isLambda) p =
    [suggest "Collapse lambdas" o (Lambda an pats body) [Replace Expr (toSS o) subts template]]
    where
      (pats, body) = fromLambda o
      template = prettyPrint $  Lambda an (zipWith munge ['a'..'z'] pats) (toNamed "body")
      munge :: Char -> Pat_ -> Pat_
      munge ident p@(PWildCard _) = p
      munge ident p = PVar (ann p) (Ident (ann p) [ident])
      subts = ("body", toSS body) : zipWith (\x y -> ([x],y)) ['a'..'z'] (map toSS pats)
lambdaExp p o@(Lambda _ [view -> PVar_ u] (Case _ (view -> Var_ v) alts))
    | u == v, u `notElem` vars alts = case alts of
        [Alt _ pat (UnGuardedRhs _ bod) Nothing] -> [suggestN "Use lambda" o $ Lambda an [pat] bod]
        _ -> [(suggestN "Use lambda-case" o $ LCase an alts){ideaNote=[RequiresExtension "LambdaCase"]}]
lambdaExp p o@(Lambda _ [view -> PVar_ u] (Tuple _ boxed xs))
    | ([yes],no) <- partition (~= u) xs, u `notElem` concatMap vars no
    = [(suggestN "Use tuple-section" o $ TupleSection an boxed [if x ~= u then Nothing else Just x | x <- xs])
        {ideaNote=[RequiresExtension "TupleSections"]}]
lambdaExp _ _ = []


-- replace any repeated pattern variable with _
fromLambda :: Exp_ -> ([Pat_], Exp_)
fromLambda (Lambda _ ps1 (fromLambda . fromParen -> (ps2,x))) = (transformBi (f $ pvars ps2) ps1 ++ ps2, x)
    where f bad x@PVar{} | prettyPrint x `elem` bad = PWildCard an
          f bad x = x
fromLambda x = ([], x)

-- Squash lambdas and replace any repeated pattern variable with _
fromLambda' :: GHC.LHsExpr GHC.GhcPs -> ([GHC.LPat GHC.GhcPs], GHC.LHsExpr GHC.GhcPs)
fromLambda' (GHC.SimpleLambda ps1 (fromLambda' . GHC.fromParen' -> (ps2,x))) = (transformBi (f $ GHC.pvars' ps2) ps1 ++ ps2, x)
    where f :: [String] -> GHC.Pat GHC.GhcPs -> GHC.Pat GHC.GhcPs
          f bad (GHC.VarPat _ (GHC.LL _ (GHC.rdrNameOcc -> x)))
              | GHC.occNameString x `elem` bad = GHC.WildPat GHC.noExt
          f bad x = x
fromLambda' x = ([], x)

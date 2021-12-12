{-# LANGUAGE LambdaCase, PatternGuards, TupleSections, ViewPatterns #-}

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
a = \x -> x + x -- a x = x + x
f (Just a) = \a -> a + a -- f (Just _) a = a + a
f (Foo a b c) = \c -> c + c -- f (Foo a b _) c = c + c
f a = \x -> x + x where _ = test
f (test -> a) = \x -> x + x
f = \x -> x + x -- f x = x + x
fun x y z = f x y z -- fun = f
fun x y z = f x x y z -- fun x = f x x
fun x y z = f g z -- fun x y = f g
fun x = f . g $ x -- fun = f . g
fun a b = f a b c where g x y = h x y -- g = h
fun a b = let g x y = h x y in f a b c -- g = h
f = foo (\y -> g x . h $ y) -- g x . h
f = foo (\y -> g x . h $ y) -- @Message Avoid lambda
f = foo ((*) x) -- (x *)
f = (*) x
f = foo (flip op x) -- (`op` x)
f = foo (flip op x) -- @Message Use section
f = foo (flip x y) -- (`x` y)
foo x = bar (\ d -> search d table) -- (`search` table)
foo x = bar (\ d -> search d table) -- @Message Avoid lambda using `infix`
f = flip op x
f = foo (flip (*) x) -- (* x)
f = foo (flip (Prelude.*) x) -- (Prelude.* x)
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
f = foo (\(x:xs) -> \x -> foo x x) -- \(_:xs) x -> foo x x
f = foo (\x -> \y -> \z -> x x y y z z) -- \x y z -> x x y y z z
x ! y = fromJust $ lookup x y
f = foo (\i -> writeIdea (getClass i) i)
f = bar (flip Foo.bar x) -- (`Foo.bar` x)
f = a b (\x -> c x d)  -- (`c` d)
yes = \x -> a x where -- a
yes = \x y -> op y x where -- flip op
yes = \x y -> op z y x where -- flip (op z)
f = \y -> nub $ reverse y where -- nub . reverse
f = \z -> foo $ bar $ baz z where -- foo . bar . baz
f = \z -> foo $ bar x $ baz z where -- foo . bar x . baz
f = \z -> foo $ z $ baz z where
f = \x -> bar map (filter x) where -- bar map . filter
f = bar &+& \x -> f (g x)
foo = [\column -> set column [treeViewColumnTitle := printf "%s (match %d)" name (length candidnates)]]
foo = [\x -> x]
foo = [\m x -> insert x x m]
foo a b c = bar (flux ++ quux) c where flux = a -- foo a b = bar (flux ++ quux)
foo a b c = bar (flux ++ quux) c where flux = c
yes = foo (\x -> Just x) -- @Warning Just
foo = bar (\x -> (x `f`)) -- f
foo = bar (\x -> shakeRoot </> "src" </> x)
baz = bar (\x -> (x +)) -- (+)
xs `withArgsFrom` args = f args
foo = bar (\x -> case x of Y z -> z) -- \(Y z) -> z
foo = bar (\x -> case x of [y, z] -> z) -- \[y, z] -> z
yes = blah (\ x -> case x of A -> a; B -> b) -- \ case A -> a; B -> b
yes = blah (\ x -> case x of A -> a; B -> b) -- @Note may require `{-# LANGUAGE LambdaCase #-}` adding to the top of the file
no = blah (\ x -> case x of A -> a x; B -> b x)
foo = bar (\x -> case x of Y z | z > 0 -> z) -- \case Y z | z > 0 -> z
yes = blah (\ x -> (y, x)) -- (y,)
yes = blah (\ x -> (y, x, z+q)) -- (y, , z+q)
yes = blah (\ x -> (y, x, y, u, v)) -- (y, , y, u, v)
yes = blah (\ x -> (y, x, z+q)) -- @Note may require `{-# LANGUAGE TupleSections #-}` adding to the top of the file
yes = blah (\ x -> (y, x, z+x))
tmp = map (\ x -> runST $ action x)
yes = map (\f -> dataDir </> f) dataFiles -- (dataDir </>)
{-# LANGUAGE TypeApplications #-}; noBug545 = coerce ((<>) @[a])
{-# LANGUAGE QuasiQuotes #-}; authOAuth2 name = authOAuth2Widget [whamlet|Login via #{name}|] name
{-# LANGUAGE QuasiQuotes #-}; authOAuth2 = foo (\name -> authOAuth2Widget [whamlet|Login via #{name}|] name)
f = {- generates a hint using hlint.yaml only -} map (flip (,) "a") "123"
f = {- generates a hint using hlint.yaml only -} map ((,) "a") "123"
f = map (\s -> MkFoo s 0 s) ["a","b","c"]
</TEST>
-}


module Hint.Lambda(lambdaHint) where

import Hint.Type (DeclHint, Idea, Note(RequiresExtension), suggest, warn, toSS, toSSA, suggestN, ideaNote, substVars, toRefactSrcSpan)
import Util
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Refact.Types hiding (Match)
import Data.Generics.Uniplate.DataOnly (universe, universeBi, transformBi)

import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Hs
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr (isTypeApp, isOpApp, isLambda, isQuasiQuote, isVar, isDol, strToVar)
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
import GHC.Util.Brackets (isAtom)
import GHC.Util.FreeVars (free, allVars, freeVars, pvars, vars, varss)
import GHC.Util.HsExpr (allowLeftSection, allowRightSection, niceLambdaR, lambda)
import GHC.Util.View

lambdaHint :: DeclHint
lambdaHint _ _ x
    =  concatMap (uncurry lambdaExp) (universeParentBi x)
    ++ concatMap (uncurry lambdaBind) binds
  where
    binds =
        ( case x of
            -- Turn a top-level HsBind under a ValD into an LHsBind.
            -- Also, its refact type needs to be Decl.
            L loc (ValD _ bind) -> ((L loc bind, Decl) :)
            _ -> id
        )
            ((,Bind) <$> universeBi x)

lambdaBind :: LHsBind GhcPs -> RType -> [Idea]
lambdaBind
    o@(L _ origBind@FunBind {fun_id = funName@(L loc1 _), fun_matches =
        MG {mg_alts =
            L _ [L _ (Match _ ctxt@(FunRhs _ Prefix _) pats (GRHSs _ [L _ (GRHS _ [] origBody@(L loc2 _))] bind))]}}) rtype
    | EmptyLocalBinds _ <- bind
    , isLambda $ fromParen origBody
    , null (universeBi pats :: [HsExpr GhcPs])
    = let (newPats, newBody) = fromLambda . lambda pats $ origBody
          (sub, tpl) = mkSubtsAndTpl newPats newBody
          gen :: [LPat GhcPs] -> LHsExpr GhcPs -> Located (HsDecl GhcPs)
          gen ps = uncurry reform . fromLambda . lambda ps
          refacts = case newBody of
              -- https://github.com/alanz/ghc-exactprint/issues/97
              L _ HsCase{} -> []
              _ -> [Replace rtype (toSSA o) sub tpl]
       in [warn "Redundant lambda" (reLoc o) (gen pats origBody) refacts]

    | let (newPats, newBody) = etaReduce pats origBody
    , length newPats < length pats, pvars (drop (length newPats) pats) `disjoint` varss bind
    = let (sub, tpl) = mkSubtsAndTpl newPats newBody
       in [warn "Eta reduce" (reform pats origBody) (reform newPats newBody)
            [Replace rtype (toSS $ reform pats origBody) sub tpl]
          ]
    where
          reform :: [LPat GhcPs] -> LHsExpr GhcPs -> Located (HsDecl GhcPs)
          reform ps b = L (combineSrcSpans (locA loc1) (locA loc2)) $ ValD noExtField $
             origBind {fun_matches = MG noExtField (noLocA [noLocA $ Match EpAnnNotUsed ctxt ps $ GRHSs emptyComments [noLoc $ GRHS EpAnnNotUsed [] b] $ EmptyLocalBinds noExtField]) Generated}

          mkSubtsAndTpl newPats newBody = (sub, tpl)
            where
              (origPats, vars) = mkOrigPats (Just (rdrNameStr funName)) newPats
              sub = ("body", toSSA newBody) : zip vars (map toSSA newPats)
              tpl = unsafePrettyPrint (reform origPats varBody)

lambdaBind _ _ = []

etaReduce :: [LPat GhcPs] -> LHsExpr GhcPs -> ([LPat GhcPs], LHsExpr GhcPs)
etaReduce (unsnoc -> Just (ps, view -> PVar_ p)) (L _ (HsApp _ x (view -> Var_ y)))
    | p == y
    , y `notElem` vars x
    , not $ any isQuasiQuote $ universe x
    = etaReduce ps x
etaReduce ps (L loc (OpApp _ x (isDol -> True) y)) = etaReduce ps (L loc (HsApp EpAnnNotUsed x y))
etaReduce ps x = (ps, x)

lambdaExp :: Maybe (LHsExpr GhcPs) -> LHsExpr GhcPs -> [Idea]
lambdaExp _ o@(L _ (HsPar _ (L _ (HsApp _ oper@(L _ (HsVar _ origf@(L _ (rdrNameOcc -> f)))) y))))
    | isSymOcc f -- is this an operator?
    , isAtom y
    , allowLeftSection $ occNameString f
    , not $ isTypeApp y
    = [suggest "Use section" (reLoc o) (reLoc to) [r]]
    where
        to :: LHsExpr GhcPs
        to = noLocA $ HsPar EpAnnNotUsed $ noLocA $ SectionL EpAnnNotUsed y oper
        r = Replace Expr (toSSA o) [("x", toSSA y)] ("(x " ++ unsafePrettyPrint origf ++ ")")

lambdaExp _ o@(L _ (HsPar _ (view -> App2 (view -> Var_ "flip") origf@(view -> RdrName_ f) y)))
    | allowRightSection (rdrNameStr f), not $ "(" `isPrefixOf` rdrNameStr f
    = [suggest "Use section" (reLoc o) (reLoc to) [r]]
    where
        to :: LHsExpr GhcPs
        to = noLocA $ HsPar EpAnnNotUsed $ noLocA $ SectionR EpAnnNotUsed origf y
        op = if isSymbolRdrName (unLoc f)
               then unsafePrettyPrint f
               else "`" ++ unsafePrettyPrint f ++ "`"
        var = if rdrNameStr f == "x" then "y" else "x"
        r = Replace Expr (toSSA o) [(var, toSSA y)] ("(" ++ op ++ " " ++ var ++ ")")

lambdaExp p o@(L _ HsLam{})
    | not $ any isOpApp p
    , (res, refact) <- niceLambdaR [] o
    , not $ isLambda res
    , not $ any isQuasiQuote $ universe res
    , not $ "runST" `Set.member` Set.map occNameString (freeVars o)
    , let name = "Avoid lambda" ++ (if countRightSections res > countRightSections o then " using `infix`" else "")
    -- If the lambda's parent is an HsPar, and the result is also an HsPar, the span should include the parentheses.
    , let from = case p of
              -- Avoid creating redundant bracket.
              Just p@(L _ (HsPar _ (L _ HsLam{})))
                | L _ HsPar{} <- res -> p
                | L _ (HsVar _ (L _ name)) <- res, not (isSymbolRdrName name) -> p
              _ -> o
    = [(if isVar res then warn else suggest) name (reLoc from) (reLoc res) (refact $ toSSA from)]
    where
        countRightSections :: LHsExpr GhcPs -> Int
        countRightSections x = length [() | L _ (SectionR _ (view -> Var_ _) _) <- universe x]

lambdaExp p o@(SimpleLambda origPats origBody)
    | isLambda (fromParen origBody)
    , null (universeBi origPats :: [HsExpr GhcPs]) -- TODO: I think this checks for view patterns only, so maybe be more explicit about that?
    , maybe True (not . isLambda) p =
    [suggest "Collapse lambdas" (reLoc o) (reLoc (lambda pats body)) [Replace Expr (toSSA o) subts template]]
    where
      (pats, body) = fromLambda o
      (oPats, vars) = mkOrigPats Nothing pats
      subts = ("body", toSSA body) : zip vars (map toSSA pats)
      template = unsafePrettyPrint (lambda oPats varBody)

-- match a lambda with a variable pattern, with no guards and no where clauses
lambdaExp _ o@(SimpleLambda [view -> PVar_ x] (L _ expr)) =
    case expr of
        -- suggest TupleSections instead of lambdas
        ExplicitTuple _ args boxity
            -- is there exactly one argument that is exactly x?
            | ([_x], ys) <- partition ((==Just x) . tupArgVar) args
            -- the other arguments must not have a nested x somewhere in them
            , Set.notMember x $ Set.map occNameString $ freeVars ys
            -> [(suggestN "Use tuple-section" (reLoc o) $ noLoc $ ExplicitTuple EpAnnNotUsed (map removeX args) boxity)
                  {ideaNote = [RequiresExtension "TupleSections"]}]
        -- suggest @LambdaCase@/directly matching in a lambda instead of doing @\x -> case x of ...@
        HsCase _ (view -> Var_ x') matchGroup
            -- is the case being done on the variable from our original lambda?
            | x == x'
            -- x must not be used in some other way inside the matches
            , Set.notMember x $ Set.map occNameString $ free $ allVars matchGroup
            -> case matchGroup of
                 -- is there a single match? - suggest match inside the lambda
                 --
                 -- we need to
                 --     * add brackets to the match, because matches in lambdas require them
                 --     * mark match as being in a lambda context so that it's printed properly
                 oldMG@(MG _ (L _ [L _ oldmatch]) _)
                   | all (\(L _ (GRHS _ stmts _)) -> null stmts) (grhssGRHSs (m_grhss oldmatch)) ->
                     let patLocs = fmap (locA . getLoc) (m_pats oldmatch)
                         bodyLocs = concatMap (\case L _ (GRHS _ _ body) -> [locA (getLoc body)])
                                        $ grhssGRHSs (m_grhss oldmatch)
                         r | notNull patLocs && notNull bodyLocs =
                             let xloc = foldl1' combineSrcSpans patLocs
                                 yloc = foldl1' combineSrcSpans bodyLocs
                              in [ Replace Expr (toSSA o) [("x", toRefactSrcSpan xloc), ("y", toRefactSrcSpan yloc)]
                                     ((if needParens then "\\(x)" else "\\x") ++ " -> y")
                                 ]
                           | otherwise = []
                         needParens = any (patNeedsParens appPrec . unLoc) (m_pats oldmatch)
                      in [ suggest "Use lambda" (reLoc o)
                             ( noLoc $ HsLam noExtField oldMG
                                 { mg_alts = noLocA
                                     [ noLocA oldmatch
                                         { m_pats = map mkParPat $ m_pats oldmatch
                                         , m_ctxt = LambdaExpr
                                         }
                                     ]
                                 }
                               :: Located (HsExpr GhcPs)
                             )
                             r
                         ]

                 -- otherwise we should use @LambdaCase@
                 MG _ (L _ _) _ ->
                     [(suggestN "Use lambda-case" (reLoc o) $ noLoc $ HsLamCase EpAnnNotUsed matchGroup)
                         {ideaNote=[RequiresExtension "LambdaCase"]}]
        _ -> []
    where
        -- | Filter out tuple arguments, converting the @x@ (matched in the lambda) variable argument
        -- to a missing argument, so that we get the proper section.
        removeX :: HsTupArg GhcPs -> HsTupArg GhcPs
        removeX (Present _ (view -> Var_ x'))
            | x == x' = Missing EpAnnNotUsed
        removeX y = y
        -- | Extract the name of an argument of a tuple if it's present and a variable.
        tupArgVar :: HsTupArg GhcPs -> Maybe String
        tupArgVar (Present _ (view -> Var_ x)) = Just x
        tupArgVar _ = Nothing

lambdaExp _ _ = []

varBody :: LHsExpr GhcPs
varBody = strToVar "body"

-- | Squash lambdas and replace any repeated pattern variable with @_@
fromLambda :: LHsExpr GhcPs -> ([LPat GhcPs], LHsExpr GhcPs)
fromLambda (SimpleLambda ps1 (fromLambda . fromParen -> (ps2,x))) = (transformBi (f $ pvars ps2) ps1 ++ ps2, x)
    where f :: [String] -> Pat GhcPs -> Pat GhcPs
          f bad (VarPat _ (rdrNameStr -> x))
              | x `elem` bad = WildPat noExtField
          f bad x = x
fromLambda x = ([], x)

-- | For each pattern, if it does not contain wildcards, replace it with a variable pattern.
--
-- The second component of the result is a list of substitution variables, which are guaranteed
-- to not occur in the function name or patterns with wildcards. For example, given
-- 'f (Foo a b _) = ...', 'f', 'a' and 'b' are not usable as substitution variables.
mkOrigPats :: Maybe String -> [LPat GhcPs] -> ([LPat GhcPs], [String])
mkOrigPats funName pats = (zipWith munge vars pats', vars)
  where
    (Set.unions -> used, pats') = unzip (map f pats)

    -- Remove variables that occur in the function name or patterns with wildcards
    vars = filter (\s -> s `Set.notMember` used && Just s /= funName) substVars

    -- Returns (chars in the pattern if the pattern contains wildcards, (whether the pattern contains wildcards, the pattern))
    f :: LPat GhcPs -> (Set String, (Bool, LPat GhcPs))
    f p
      | any isWildPat (universe p) =
          let used = Set.fromList [rdrNameStr name | (L _ (VarPat _ name)) <- universe p]
           in (used, (True, p))
      | otherwise = (mempty, (False, p))

    isWildPat :: LPat GhcPs -> Bool
    isWildPat = \case (L _ (WildPat _)) -> True; _ -> False

    -- Replace the pattern with a variable pattern if the pattern doesn't contain wildcards.
    munge :: String -> (Bool, LPat GhcPs) -> LPat GhcPs
    munge _ (True, p) = p
    munge ident (False, L ploc _) = L ploc (VarPat noExtField (noLocA $ mkRdrUnqual $ mkVarOcc ident))

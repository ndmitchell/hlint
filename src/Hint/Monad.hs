{-# LANGUAGE ViewPatterns, PatternGuards, FlexibleContexts #-}
{-
    Find and match:

    mapM, foldM, forM, replicateM, sequence, zipWithM
    not at the last line of a do statement, or to the left of >>

    Use let x = y instead of x <- return y, unless x is contained
    within y, or bound more than once in that do block.

<TEST>
yes = do mapM print a; return b -- mapM_ print a
yes = do _ <- mapM print a; return b -- mapM_ print a
no = mapM print a
no = do foo ; mapM print a
yes = do (bar+foo) -- (bar+foo)
no = do bar ; foo
yes = do bar; a <- foo; return a -- do bar; foo
no = do bar; a <- foo; return b
yes = do x <- bar; x -- do join bar
no = do x <- bar; x; x
yes = do x <- bar; return (f x) -- do f <$> bar
yes = do x <- bar; return $ f x -- do f <$> bar
yes = do x <- bar; pure $ f x -- do f <$> bar
yes = do x <- bar; return $ f (g x) -- do f . g <$> bar
yes = do x <- bar; return (f $ g x) -- do f . g <$> bar
yes = do x <- bar $ baz; return (f $ g x)
no = do x <- bar; return (f x x)
{-# LANGUAGE RecursiveDo #-}; no = mdo hook <- mkTrigger pat (act >> rmHook hook) ; return hook
yes = do x <- return y; foo x -- @Suggestion do let x = y; foo x
yes = do x <- return $ y + z; foo x -- do let x = y + z; foo x
no = do x <- return x; foo x
no = do x <- return y; x <- return y; foo x
yes = do forM files $ \x -> return (); return () -- forM_ files $ \x -> return ()
yes = do if a then forM x y else return (); return 12 -- forM_ x y
yes = do case a of {_ -> forM x y; x:xs -> foo xs}; return () -- forM_ x y
foldM_ f a xs = foldM f a xs >> return ()
folder f a xs = foldM f a xs >> return () -- foldM_ f a xs
folder f a xs = foldM f a xs >>= \_ -> return () -- foldM_ f a xs
yes = mapM async ds >>= mapM wait >> return () -- mapM async ds >>= mapM_ wait
main = "wait" ~> do f a $ sleep 10
main = f $ do g a $ sleep 10 -- g a $ sleep 10
main = do f a $ sleep 10 -- f a $ sleep 10
main = do foo x; return 3; bar z -- do foo x; bar z
main = void $ forM_ f xs -- forM_ f xs
main = void $ forM f xs -- void $ forM_ f xs
main = do _ <- forM_ f xs; bar -- forM_ f xs
main = do bar; forM_ f xs; return () -- do bar; forM_ f xs
main = do a; when b c; return () -- do a; when b c
</TEST>
-}


module Hint.Monad(monadHint) where

import Hint.Type(DeclHint',Idea,ideaNote,warn',toSS',suggest',Note(Note))

import HsSyn
import SrcLoc
import BasicTypes
import TcEvidence
import RdrName
import OccName
import Bag
import GHC.Util

import Data.Tuple.Extra
import Data.Maybe
import Data.List.Extra
import Refact.Types hiding (Match)
import qualified Refact.Types as R

badFuncs :: [String]
badFuncs = ["mapM","foldM","forM","replicateM","sequence","zipWithM","traverse","for","sequenceA"]
unitFuncs :: [String]
unitFuncs = ["when","unless","void"]

monadHint :: DeclHint'
monadHint _ _ d = concatMap (monadExp d) $ universeParentExp' d

monadExp :: LHsDecl GhcPs -> (Maybe (Int, LHsExpr GhcPs), LHsExpr GhcPs) -> [Idea]
monadExp (declName -> decl) (parent, x) =
  case x of
    (view' -> App2' op x1 x2) | isTag' op ">>" -> f x1
    (view' -> App2' op x1 (view' -> LamConst1' _)) | isTag' op ">>=" -> f x1
    (LL l (HsApp _ op x)) | isTag' op "void" -> seenVoid (cL l . HsApp noExt op) x
    (LL l (OpApp _ op dol x)) | isTag' op "void", isDol' dol -> seenVoid (cL l . OpApp noExt op dol) x
    (LL loc (HsDo _ _ (LL _ [LL _ (BodyStmt _ y _ _ )]))) -> [warn' "Redundant do" x y [Replace Expr (toSS' x) [("y", toSS' y)] "y"] | not $ doOperator parent y]
    (LL loc (HsDo _ DoExpr (L _ xs))) ->
      monadSteps (cL loc . HsDo noExt DoExpr . noLoc) xs ++
      [suggest' "Use let" x (cL loc (HsDo noExt DoExpr (noLoc y)) :: LHsExpr GhcPs) rs | Just (y, rs) <- [monadLet xs]] ++
      concat [f x | (LL _ (BodyStmt _ x _ _)) <- init xs] ++
      concat [f x | (LL _ (BindStmt _ (LL _ WildPat{}) x _ _)) <- init xs]
    _ -> []
  where
    f = monadNoResult (fromMaybe "" decl) id
    seenVoid wrap x = monadNoResult (fromMaybe "" decl) wrap x ++ [warn' "Redundant void" (wrap x) x [] | returnsUnit x]

-- Sometimes people write 'a * do a + b', to avoid brackets.
doOperator :: (Eq a, Num a) => Maybe (a, LHsExpr GhcPs) -> LHsExpr GhcPs -> Bool
doOperator (Just (2, LL _ (OpApp _ _ op _ )))  (LL _ OpApp {}) | not $ isDol' op = True
doOperator _ _ = False

returnsUnit :: LHsExpr GhcPs -> Bool
returnsUnit (LL _ (HsPar _ x)) = returnsUnit x
returnsUnit (LL _ (HsApp _ x _)) = returnsUnit x
returnsUnit (LL _ (OpApp _ x op _)) | isDol' op = returnsUnit x
returnsUnit (LL _ (HsVar _ (L _ x))) = occNameString (rdrNameOcc x) `elem` map (++ "_") badFuncs ++ unitFuncs
returnsUnit _ = False

-- See through HsPar, and down HsIf/HsCase, return the name to use in
-- the hint, and the revised expression.
monadNoResult :: String -> (LHsExpr GhcPs -> LHsExpr GhcPs) -> LHsExpr GhcPs -> [Idea]
monadNoResult inside wrap (LL l (HsPar _ x)) = monadNoResult inside (wrap . cL l . HsPar noExt) x
monadNoResult inside wrap (LL l (HsApp _ x y)) = monadNoResult inside (\x -> wrap $ cL l (HsApp noExt x y)) x
monadNoResult inside wrap (LL l (OpApp _ x tag@(LL _ (HsVar _ (L _ op))) y))
    | isDol' tag = monadNoResult inside (\x -> wrap $ cL l (OpApp noExt x tag y)) x
    | occNameString (rdrNameOcc op) == ">>=" = monadNoResult inside (wrap . cL l . OpApp noExt x tag) y
monadNoResult inside wrap x
    | x2 : _ <- filter (isTag' x) badFuncs
    , let x3 = x2 ++ "_"
    = [warn' ("Use " ++ x3) (wrap x) (wrap $ strToVar' x3) [Replace Expr (toSS' x) [] x3] | inside /= x3]
monadNoResult inside wrap (replaceBranches' -> (bs, rewrap)) =
    map (\x -> x{ideaNote=nubOrd $ Note "May require adding void to other branches" : ideaNote x}) $ concat
        [monadNoResult inside id b | b <- bs]

monadStep :: ([ExprLStmt GhcPs] -> LHsExpr GhcPs)
           -> [ExprLStmt GhcPs] -> [Idea]

-- Rewrite 'do return x; $2' as 'do $2'.
monadStep wrap o@(LL _ (BodyStmt _ (fromRet -> Just (ret, _)) _ _ ) : x : xs)
  = [warn' ("Redundant " ++ ret) (wrap o) (wrap $ x:xs) [Delete Stmt (toSS' (head o))]]

-- Rewrite 'do a <- $1; return a' as 'do $1'.
monadStep wrap o@[ g@(LL _ (BindStmt _ (LL _ (VarPat _ (L _ p))) x _ _ ))
                  , q@(LL _ (BodyStmt _ (fromRet -> Just (ret, LL _ (HsVar _ (L _ v)))) _ _))]
  | occNameString (rdrNameOcc p) == occNameString (rdrNameOcc v)
  = [warn' ("Redundant " ++ ret) (wrap o) (wrap [noLoc $ BodyStmt noExt x noSyntaxExpr' noSyntaxExpr'])
      [Replace Stmt (toSS' g) [("x", toSS' x)] "x", Delete Stmt (toSS' q)]]

-- Suggest to use join. Rewrite 'do x <- $1; x; $2' as 'do join $1; $2'.
monadStep wrap o@(g@(LL _ (BindStmt _ (view' -> PVar_' p) x _ _)):q@(LL _ (BodyStmt _ (view' -> Var_' v) _ _)):xs)
  | p == v && v `notElem` varss' xs
  = let app = noLoc $ HsApp noExt (strToVar' "join") x
        body = noLoc $ BodyStmt noExt (rebracket1' app) noSyntaxExpr' noSyntaxExpr'
        stmts = body : xs
    in [warn' "Use join" (wrap o) (wrap stmts) r]
  where r = [Replace Stmt (toSS' g) [("x", toSS' x)] "join x", Delete Stmt (toSS' q)]

-- Redundant variable capture. Rewrite 'do _ <- <return ()>; $1' as
-- 'do <return ()>; $1'.
monadStep wrap (o@(LL loc (BindStmt _ p x _ _)) : rest)
    | isPWildCard' p, returnsUnit x
    = let body = cL loc $ BodyStmt noExt x noSyntaxExpr' noSyntaxExpr' :: ExprLStmt GhcPs
      in [warn' "Redundant variable capture" o body []]

-- Redundant unit return : 'do <return ()>; return ()'.
monadStep
  wrap o@[ LL _ (BodyStmt _ x _ _)
         , LL _ (BodyStmt _ (fromRet -> Just (ret, LL _ (HsVar _ (L _ unit)))) _ _)]
     | returnsUnit x, occNameString (rdrNameOcc unit) == "()"
  = [warn' ("Redundant " ++ ret) (wrap o) (wrap $ take 1 o) []]

-- Rewrite 'do x <- $1; return $ f $ g x' as 'f . g <$> x'
monadStep wrap
  o@[g@(LL _ (BindStmt _ (view' -> PVar_' u) x _ _))
    , q@(LL _ (BodyStmt _ (fromApplies -> (ret:f:fs, view' -> Var_' v)) _ _))]
  | isReturn' ret, notDol x, u == v, length fs < 3, all isSimple (f : fs), v `notElem` vars' (f : fs)
  =
      [warn' "Use <$>" (wrap o) (wrap [noLoc $ BodyStmt noExt (noLoc $ OpApp noExt (foldl' (\acc e -> noLoc $ OpApp noExt acc (strToVar' ".") e) f fs) (strToVar' "<$>") x) noSyntaxExpr' noSyntaxExpr'])
      [Replace Stmt (toSS' g) (("x", toSS' x):zip vs (toSS' <$> f:fs)) (intercalate " . " (take (length fs + 1) vs) ++ " <$> x"), Delete Stmt (toSS' q)]]
  where
    isSimple (fromApps' -> xs) = all isAtom' (x : xs)
    vs = ('f':) . show <$> [0..]

    notDol :: LHsExpr GhcPs -> Bool
    notDol (LL _ (OpApp _ _ op _)) = not $ isDol' op
    notDol _ = True

monadStep _ _ = []

-- Suggest removing a return
monadSteps :: ([ExprLStmt GhcPs] -> LHsExpr GhcPs) -> [ExprLStmt GhcPs] -> [Idea]
monadSteps wrap (x : xs) = monadStep wrap (x : xs) ++ monadSteps (wrap . (x :)) xs
monadSteps _ _ = []

-- | Rewrite 'do ...; x <- return y; ...' as 'do ...; let x = y; ...'.
monadLet :: [ExprLStmt GhcPs] -> Maybe ([ExprLStmt GhcPs], [Refactoring R.SrcSpan])
monadLet xs = if null rs then Nothing else Just (ys, rs)
  where
    (ys, catMaybes -> rs) = unzip $ map mkLet xs
    vs = concatMap pvars' [p | (LL _ (BindStmt _ p _ _ _)) <- xs]

    mkLet :: ExprLStmt GhcPs -> (ExprLStmt GhcPs, Maybe (Refactoring R.SrcSpan))
    mkLet g@(LL _ (BindStmt _ v@(view' -> PVar_' p) (fromRet -> Just (_, y)) _ _ ))
      | p `notElem` vars' y, p `notElem` delete p vs
      = (template p y, Just refact)
      where
        refact = Replace Stmt (toSS' g) [("lhs", toSS' v), ("rhs", toSS' y)]
                      (unsafePrettyPrint $ template "lhs" (strToVar' "rhs"))
    mkLet x = (x, Nothing)

    template :: String -> LHsExpr GhcPs -> ExprLStmt GhcPs
    template lhs rhs =
        let p = noLoc $ mkRdrUnqual (mkVarOcc lhs)
            grhs = noLoc (GRHS noExt [] rhs)
            grhss = GRHSs noExt [grhs] (noLoc (EmptyLocalBinds noExt))
            match = noLoc $ Match noExt (FunRhs p Prefix NoSrcStrict) [] grhss
            fb = noLoc $ FunBind noExt p (MG noExt (noLoc [match]) Generated) WpHole []
            binds = unitBag fb
            valBinds = ValBinds noExt binds []
            localBinds = noLoc $ HsValBinds noExt valBinds
         in noLoc $ LetStmt noExt localBinds

fromApplies :: LHsExpr GhcPs -> ([LHsExpr GhcPs], LHsExpr GhcPs)
fromApplies (LL _ (HsApp _ f x)) = first (f:) $ fromApplies (fromParen' x)
fromApplies (LL _ (OpApp _ f (isDol' -> True) x)) = first (f:) $ fromApplies x
fromApplies x = ([], x)

fromRet :: LHsExpr GhcPs -> Maybe (String, LHsExpr GhcPs)
fromRet (LL _ (HsPar _ x)) = fromRet x
fromRet (LL _ (OpApp _ x (LL _ (HsVar _ (L _ y))) z)) | occNameString (rdrNameOcc y) == "$" = fromRet $ noLoc (HsApp noExt x z)
fromRet (LL _ (HsApp _ x y)) | isReturn' x = Just (unsafePrettyPrint x, y)
fromRet _ = Nothing

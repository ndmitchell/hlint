{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
yes = do (bar+foo) --
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
yes = do x <- return y; foo x -- @Suggestion let x = y
yes = do x <- return $ y + z; foo x -- let x = y + z
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
{-# LANGUAGE BlockArguments #-}; main = print do 17 + 25
{-# LANGUAGE BlockArguments #-}; main = print do 17 --
main = f $ do g a $ sleep 10 --
main = do f a $ sleep 10 -- @Ignore
main = do foo x; return 3; bar z --
main = void $ forM_ f xs -- forM_ f xs
main = void (forM_ f xs) -- forM_ f xs
main = void $ forM f xs -- forM_ f xs
main = void (forM f xs) -- forM_ f xs
main = do _ <- forM_ f xs; bar -- forM_ f xs
main = do bar; forM_ f xs; return () -- do bar; forM_ f xs
main = do a; when b c; return () -- do a; when b c
bar = 1 * do {\x -> x+x} + y
issue978 = do \
   print "x" \
   if False then main else do \
   return ()
</TEST>
-}


module Hint.Monad(monadHint) where

import Hint.Type

import GHC.Hs hiding (Warning)
import GHC.Types.Fixity
import GHC.Types.SrcLoc
import GHC.Types.Basic
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence
import GHC.Data.Bag
import qualified GHC.Data.Strict

import Language.Haskell.GhclibParserEx.GHC.Hs.Pat
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
import GHC.Util

import Data.Generics.Uniplate.DataOnly
import Data.Tuple.Extra
import Data.Maybe
import Data.List.Extra
import Refact.Types hiding (Match)
import qualified Refact.Types as R


badFuncs :: [String]
badFuncs = ["mapM","foldM","forM","replicateM","sequence","zipWithM","traverse","for","sequenceA"]
unitFuncs :: [String]
unitFuncs = ["when","unless","void"]

monadHint :: DeclHint
monadHint _ _ d = concatMap (f Nothing Nothing) $ childrenBi d
    where
        decl = declName d
        f parentDo parentExpr x =
            monadExp decl parentDo parentExpr x ++
            concat [f (if isHsDo x then Just x else parentDo) (Just (i, x)) c | (i, c) <- zipFrom 0 $ children x]

        isHsDo (L _ HsDo{}) = True
        isHsDo _ = False


-- | Call with the name of the declaration,
--   the nearest enclosing `do` expression
--   the nearest enclosing expression
--   the expression of interest
monadExp :: Maybe String -> Maybe (LHsExpr GhcPs) -> Maybe (Int, LHsExpr GhcPs) -> LHsExpr GhcPs -> [Idea]
monadExp decl parentDo parentExpr x =
  case x of
    (view -> App2 op x1 x2) | isTag ">>" op -> f x1
    (view -> App2 op x1 (view -> LamConst1 _)) | isTag ">>=" op -> f x1
    (L l (HsApp _ op x)) | isTag "void" op -> seenVoid (L l . HsApp EpAnnNotUsed op) x
    (L l (OpApp _ op dol x)) | isTag "void" op, isDol dol -> seenVoid (L l . OpApp EpAnnNotUsed op dol) x
    (L loc (HsDo _ ctx (L loc2 [L loc3 (BodyStmt _ y _ _ )]))) ->
      let doOrMDo = case ctx of MDoExpr _ -> "mdo"; _ -> "do"
       in [ ideaRemove Ignore ("Redundant " ++ doOrMDo) (doSpan doOrMDo (locA loc)) doOrMDo [Replace Expr (toSSA x) [("y", toSSA y)] "y"]
          | not $ doAsBrackets parentExpr y
          , not $ doAsAvoidingIndentation parentDo x
          ]
    (L loc (HsDo _ (DoExpr mm) (L _ xs))) ->
      monadSteps (L loc . HsDo EpAnnNotUsed (DoExpr mm) . noLocA) xs ++
      [suggest "Use let" (reLoc from) (reLoc to) [r] | (from, to, r) <- monadLet xs] ++
      concat [f x | (L _ (BodyStmt _ x _ _)) <- dropEnd1 xs] ++
      concat [f x | (L _ (BindStmt _ (L _ WildPat{}) x)) <- dropEnd1 xs]
    _ -> []
  where
    f = monadNoResult (fromMaybe "" decl) id
    seenVoid wrap (L l (HsPar x p y q)) = seenVoid (wrap . L l . \y -> HsPar x p y q) y
    seenVoid wrap x =
      -- Suggest `traverse_ f x` given `void $ traverse_ f x`
      [warn "Redundant void" (reLoc (wrap x)) (reLoc x) [Replace Expr (toSSA (wrap x)) [("a", toSSA x)] "a"] | returnsUnit x]
        -- Suggest `traverse_ f x` given `void $ traverse f x`
        ++ ( case modifyAppHead
               ( \fun@(L l name) ->
                   ( if occNameStr name `elem` badFuncs
                       then L l (mkRdrUnqual (mkVarOcc (occNameStr name ++ "_")))
                       else fun,
                     fun
                   )
               )
               x of
               (x', Just fun@(L l name)) | occNameStr name `elem` badFuncs ->
                  let fun_ = occNameStr name ++ "_"
                   in [warn ("Use " ++ fun_) (reLoc (wrap x)) (reLoc x')
                        [Replace Expr (toSSA (wrap x)) [("a", toSSA x)] "a",
                         Replace Expr (toSSA fun) [] fun_]]
               _ -> []
           )
    doSpan doOrMDo = \case
      UnhelpfulSpan s -> UnhelpfulSpan s
      RealSrcSpan s _ ->
        let start = realSrcSpanStart s
            end = mkRealSrcLoc (srcSpanFile s) (srcLocLine start) (srcLocCol start + length doOrMDo)
         in RealSrcSpan (mkRealSrcSpan start end) GHC.Data.Strict.Nothing

-- Sometimes people write 'a * do a + b', to avoid brackets,
-- or using BlockArguments they can write 'a do a b',
-- or using indentation a * do {\b -> c} * d
-- Return True if they are using do as brackets
doAsBrackets :: Maybe (Int, LHsExpr GhcPs) -> LHsExpr GhcPs -> Bool
doAsBrackets (Just (2, L _ (OpApp _ _ op _ ))) _ | isDol op = False -- not quite atomic, but close enough
doAsBrackets (Just (i, o)) x = needBracket i o x
doAsBrackets Nothing x = False


-- Sometimes people write do, to avoid identation, see
-- https://github.com/ndmitchell/hlint/issues/978
-- Return True if they are using do as avoiding identation
doAsAvoidingIndentation :: Maybe (LHsExpr GhcPs) -> LHsExpr GhcPs -> Bool
doAsAvoidingIndentation (Just (L _ (HsDo _ _ (L anna _)))) (L _ (HsDo _ _ (L annb _)))
  | SrcSpanAnn _ (RealSrcSpan a _) <- anna
  , SrcSpanAnn _ (RealSrcSpan b _) <- annb
    = srcSpanStartCol a == srcSpanStartCol b
doAsAvoidingIndentation parent self = False

-- Apply a function to the application head, including `head arg` and `head $ arg`, which modifies
-- the head and returns a value. Sees through parentheses.
modifyAppHead :: forall a. (LIdP GhcPs -> (LIdP GhcPs, a)) -> LHsExpr GhcPs -> (LHsExpr GhcPs, Maybe a)
modifyAppHead f = go id
  where
    go :: (LHsExpr GhcPs -> LHsExpr GhcPs) -> LHsExpr GhcPs -> (LHsExpr GhcPs, Maybe a)
    go wrap (L l (HsPar _ p x q )) = go (wrap . L l . \x -> HsPar EpAnnNotUsed p x q) x
    go wrap (L l (HsApp _ x y)) = go (\x -> wrap $ L l (HsApp EpAnnNotUsed x y)) x
    go wrap (L l (OpApp _ x op y)) | isDol op = go (\x -> wrap $ L l (OpApp EpAnnNotUsed x op y)) x
    go wrap (L l (HsVar _ x)) = (wrap (L l (HsVar NoExtField x')), Just a)
      where (x', a) = f x
    go _ expr = (expr, Nothing)

returnsUnit :: LHsExpr GhcPs -> Bool
returnsUnit = fromMaybe False
            . snd
            . modifyAppHead (\x -> (x, occNameStr (unLoc x) `elem` map (++ "_") badFuncs ++ unitFuncs))

-- See through HsPar, and down HsIf/HsCase, return the name to use in
-- the hint, and the revised expression.
monadNoResult :: String -> (LHsExpr GhcPs -> LHsExpr GhcPs) -> LHsExpr GhcPs -> [Idea]
monadNoResult inside wrap (L l (HsPar _ _ x _)) = monadNoResult inside (wrap . nlHsPar) x
monadNoResult inside wrap (L l (HsApp _ x y)) = monadNoResult inside (\x -> wrap $ L l (HsApp EpAnnNotUsed x y)) x
monadNoResult inside wrap (L l (OpApp _ x tag@(L _ (HsVar _ (L _ op))) y))
    | isDol tag = monadNoResult inside (\x -> wrap $ L l (OpApp EpAnnNotUsed x tag y)) x
    | occNameStr op == ">>=" = monadNoResult inside (wrap . L l . OpApp EpAnnNotUsed x tag) y
monadNoResult inside wrap x
    | x2 : _ <- filter (`isTag` x) badFuncs
    , let x3 = x2 ++ "_"
    = [warn ("Use " ++ x3) (reLoc (wrap x)) (reLoc (wrap $ strToVar x3)) [Replace Expr (toSSA x) [] x3] | inside /= x3]
monadNoResult inside wrap (replaceBranches -> (bs, rewrap)) =
    map (\x -> x{ideaNote=nubOrd $ Note "May require adding void to other branches" : ideaNote x}) $ concat
        [monadNoResult inside id b | b <- bs]

monadStep :: ([ExprLStmt GhcPs] -> LHsExpr GhcPs)
           -> [ExprLStmt GhcPs] -> [Idea]

-- Rewrite 'do return x; $2' as 'do $2'.
monadStep wrap (o@(L _ (BodyStmt _ (fromRet -> Just (ret, _)) _ _ )) : xs@(_:_))
  = [ideaRemove Warning ("Redundant " ++ ret) (locA (getLoc o)) (unsafePrettyPrint o) [Delete Stmt (toSSA o)]]

-- Rewrite 'do a <- $1; return a' as 'do $1'.
monadStep wrap o@[ g@(L _ (BindStmt _ (L _ (VarPat _ (L _ p))) x))
                  , q@(L _ (BodyStmt _ (fromRet -> Just (ret, L _ (HsVar _ (L _ v)))) _ _))]
  | occNameStr p == occNameStr v
  = [warn ("Redundant " ++ ret) (reLoc (wrap o)) (reLoc (wrap [noLocA $ BodyStmt noExtField x noSyntaxExpr noSyntaxExpr]))
      [Replace Stmt (toSSA g) [("x", toSSA x)] "x", Delete Stmt (toSSA q)]]

-- Suggest to use join. Rewrite 'do x <- $1; x; $2' as 'do join $1; $2'.
monadStep wrap o@(g@(L _ (BindStmt _ (view -> PVar_ p) x)):q@(L _ (BodyStmt _ (view -> Var_ v) _ _)):xs)
  | p == v && v `notElem` varss xs
  = let app = noLocA $ HsApp EpAnnNotUsed (strToVar "join") x
        body = noLocA $ BodyStmt noExtField (rebracket1 app) noSyntaxExpr noSyntaxExpr
        stmts = body : xs
    in [warn "Use join" (reLoc (wrap o)) (reLoc (wrap stmts)) r]
  where r = [Replace Stmt (toSSA g) [("x", toSSA x)] "join x", Delete Stmt (toSSA q)]

-- Redundant variable capture. Rewrite 'do _ <- <return ()>; $1' as
-- 'do <return ()>; $1'.
monadStep wrap (o@(L loc (BindStmt _ p x)) : rest)
    | isPWildcard p, returnsUnit x
    = let body = L loc $ BodyStmt noExtField x noSyntaxExpr noSyntaxExpr :: ExprLStmt GhcPs
      in [warn "Redundant variable capture" (reLoc o) (reLoc body) [Replace Stmt (toSSA o) [("x", toSSA x)] "x"]]

-- Redundant unit return : 'do <return ()>; return ()'.
monadStep
  wrap o@[ L _ (BodyStmt _ x _ _)
         , q@(L _ (BodyStmt _ (fromRet -> Just (ret, L _ (HsVar _ (L _ unit)))) _ _))]
     | returnsUnit x, occNameStr unit == "()"
  = [warn ("Redundant " ++ ret) (reLoc (wrap o)) (reLoc (wrap $ take 1 o)) [Delete Stmt (toSSA q)]]

-- Rewrite 'do x <- $1; return $ f $ g x' as 'f . g <$> x'
monadStep wrap
  o@[g@(L _ (BindStmt _ (view -> PVar_ u) x))
    , q@(L _ (BodyStmt _ (fromApplies -> (ret:f:fs, view -> Var_ v)) _ _))]
  | isReturn ret, notDol x, u == v, length fs < 3, all isSimple (f : fs), v `notElem` vars (f : fs)
  =
      [warn "Use <$>" (reLoc (wrap o)) (reLoc (wrap [noLocA $ BodyStmt noExtField (noLocA $ OpApp EpAnnNotUsed (foldl' (\acc e -> noLocA $ OpApp EpAnnNotUsed acc (strToVar ".") e) f fs) (strToVar "<$>") x) noSyntaxExpr noSyntaxExpr]))
      [Replace Stmt (toSSA g) (("x", toSSA x):zip vs (toSSA <$> f:fs)) (intercalate " . " (take (length fs + 1) vs) ++ " <$> x"), Delete Stmt (toSSA q)]]
  where
    isSimple (fromApps -> xs) = all isAtom (x : xs)
    vs = ('f':) . show <$> [0..]

    notDol :: LHsExpr GhcPs -> Bool
    notDol (L _ (OpApp _ _ op _)) = not $ isDol op
    notDol _ = True
monadStep _ _ = []

-- Suggest removing a return
monadSteps :: ([ExprLStmt GhcPs] -> LHsExpr GhcPs) -> [ExprLStmt GhcPs] -> [Idea]
monadSteps wrap (x : xs) = monadStep wrap (x : xs) ++ monadSteps (wrap . (x :)) xs
monadSteps _ _ = []

-- | Rewrite 'do ...; x <- return y; ...' as 'do ...; let x = y; ...'.
monadLet :: [ExprLStmt GhcPs] -> [(ExprLStmt GhcPs, ExprLStmt GhcPs, Refactoring R.SrcSpan)]
monadLet xs = mapMaybe mkLet xs
  where
    vs = concatMap pvars [p | (L _ (BindStmt _ p _ )) <- xs]

    mkLet :: ExprLStmt GhcPs -> Maybe (ExprLStmt GhcPs, ExprLStmt GhcPs, Refactoring R.SrcSpan)
    mkLet x@(L _ (BindStmt _ v@(view -> PVar_ p) (fromRet -> Just (_, y))))
      | p `notElem` vars y, p `notElem` delete p vs
      = Just (x, template p y, refact)
      where
        refact = Replace Stmt (toSSA x) [("lhs", toSSA v), ("rhs", toSSA y)]
                      (unsafePrettyPrint $ template "lhs" (strToVar "rhs"))
    mkLet _ = Nothing

    template :: String -> LHsExpr GhcPs -> ExprLStmt GhcPs
    template lhs rhs =
        let p = noLocA $ mkRdrUnqual (mkVarOcc lhs)
            grhs = noLocA (GRHS EpAnnNotUsed [] rhs)
            grhss = GRHSs emptyComments [grhs] (EmptyLocalBinds noExtField)
            match = noLocA $ Match EpAnnNotUsed (FunRhs p Prefix NoSrcStrict) [] grhss
            fb = noLocA $ FunBind noExtField p (MG noExtField (noLocA [match]) Generated) []
            binds = unitBag fb
            valBinds = ValBinds NoAnnSortKey binds []
            localBinds = HsValBinds EpAnnNotUsed valBinds
         in noLocA $ LetStmt EpAnnNotUsed localBinds

fromApplies :: LHsExpr GhcPs -> ([LHsExpr GhcPs], LHsExpr GhcPs)
fromApplies (L _ (HsApp _ f x)) = first (f:) $ fromApplies (fromParen x)
fromApplies (L _ (OpApp _ f (isDol -> True) x)) = first (f:) $ fromApplies x
fromApplies x = ([], x)

fromRet :: LHsExpr GhcPs -> Maybe (String, LHsExpr GhcPs)
fromRet (L _ (HsPar _ _ x _)) = fromRet x
fromRet (L _ (OpApp _ x (L _ (HsVar _ (L _ y))) z)) | occNameStr y == "$" = fromRet $ noLocA (HsApp EpAnnNotUsed x z)
fromRet (L _ (HsApp _ x y)) | isReturn x = Just (unsafePrettyPrint x, y)
fromRet _ = Nothing

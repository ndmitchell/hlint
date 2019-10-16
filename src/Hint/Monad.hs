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
main = do _ <- forM_ f xs; bar -- do forM_ f xs; bar
main = do bar; forM_ f xs; return () -- do bar; forM_ f xs
main = do a; when b c; return () -- do a; when b c
</TEST>
-}


module Hint.Monad(monadHint) where

import Control.Applicative
import Data.Tuple.Extra
import Data.Maybe
import Data.List.Extra
import Hint.Type
import Refact.Types
import qualified Refact.Types as R
import Prelude


badFuncs :: [String]
badFuncs = ["mapM","foldM","forM","replicateM","sequence","zipWithM","traverse","for","sequenceA"]
unitFuncs :: [String]
unitFuncs = ["when","unless","void"]


monadHint :: DeclHint
monadHint _ _ d = concatMap (monadExp d) $ universeParentExp d

monadExp :: Decl_ -> (Maybe (Int, Exp_), Exp_) -> [Idea]
monadExp (fromNamed -> decl) (parent, x) = case x of
        (view -> App2 op x1 x2) | op ~= ">>" -> f x1
        (view -> App2 op x1 (view -> LamConst1 _)) | op ~= ">>=" -> f x1
        App an op x | op ~= "void" -> seenVoid (App an op) x
        InfixApp an op dol x | op ~= "void", isDol dol -> seenVoid (InfixApp an op dol) x
        Do an [Qualifier _ y] -> [warn "Redundant do" x y [Replace Expr (toSS x) [("y", toSS y)] "y"] | not $ doOperator parent y]
        Do an xs ->
            monadSteps (Do an) xs ++
            [suggest "Use let" x (Do an y) rs | Just (y, rs) <- [monadLet xs]] ++
            concat [f x | Qualifier _ x <- init xs] ++
            concat [f x | Generator _ (PWildCard _) x <- init xs]
        _ -> []
    where
        f = monadNoResult decl id
        seenVoid wrap x = monadNoResult decl wrap x ++ [warn "Redundant void" (wrap x) x [] | returnsUnit x]



-- Sometimes people write a * do a + b, to avoid brackets
doOperator :: (Eq a, Num a) => Maybe (a, Exp S) -> Exp l -> Bool
doOperator (Just (1, InfixApp _ _ op _)) InfixApp{} | not $ isDol op = True
doOperator _ _ = False


returnsUnit :: Exp_ -> Bool
returnsUnit (Paren _ x) = returnsUnit x
returnsUnit (App _ x _) = returnsUnit x
returnsUnit (InfixApp _ x op _) | isDol op = returnsUnit x
returnsUnit (Var _ x) = any (x ~=) $ map (++ "_") badFuncs ++ unitFuncs
returnsUnit _ = False


-- see through Paren and down if/case etc
-- return the name to use in the hint, and the revised expression
monadNoResult :: String -> (Exp_ -> Exp_) -> Exp_ -> [Idea]
monadNoResult inside wrap (Paren l x) = monadNoResult inside (wrap . Paren l) x
monadNoResult inside wrap (App l x y) = monadNoResult inside (\x -> wrap $ App l x y) x
monadNoResult inside wrap (InfixApp l x op y)
    | isDol op = monadNoResult inside (\x -> wrap $ InfixApp l x op y) x
    | op ~= ">>=" = monadNoResult inside (wrap . InfixApp l x op) y
monadNoResult inside wrap x
    | x2:_ <- filter (x ~=) badFuncs
    , let x3 = x2 ++ "_"
    = [warn ("Use " ++ x3) (wrap x) (wrap $ toNamed x3) [Replace Expr (toSS x) [] x3] | inside /= x3]
monadNoResult inside wrap (replaceBranches -> (bs, rewrap)) =
    map (\x -> x{ideaNote=nubOrd $ Note "May require adding void to other branches" : ideaNote x}) $ concat
        [monadNoResult inside id b | b <- bs]


monadStep :: ([Stmt S] -> Exp_) -> [Stmt S] -> [Idea]

-- do return x; $2 ==> do $2
monadStep wrap o@(Qualifier _ (fromRet -> Just (ret, _)):x:xs) =
    [warn ("Redundant " ++ ret) (wrap o) (wrap $ x:xs) [Delete Stmt (toSS (head o))]]

-- do a <- $1; return a ==> do $1
monadStep wrap o@[g@(Generator _ (PVar _ p) x), q@(Qualifier _ (fromRet -> Just (ret, Var _ v)))]
    | fromNamed v == fromNamed p
    = [warn ("Redundant " ++ ret) (wrap o) (wrap [Qualifier an x])
            [Replace Stmt (toSS g) [("x", toSS x)] "x", Delete Stmt (toSS q)]]

-- do x <- $1; x; $2  ==> do join $1; $2
monadStep wrap o@(g@(Generator _ (view -> PVar_ p) x):q@(Qualifier _ (view -> Var_ v)):xs)
    | p == v && v `notElem` varss xs
    = [warn "Use join" (wrap o) (wrap $ Qualifier an (rebracket1 $ App an (toNamed "join") x):xs) r]
    where r = [Replace Stmt (toSS g) [("x", toSS x)] "join x", Delete Stmt (toSS q)]

-- do _ <- <return ()>; $1 ==> do <return ()>; $1
monadStep wrap o@(Generator an PWildCard{} x:rest)
    | returnsUnit x
    = [warn "Redundant variable capture" (wrap o) (wrap $ Qualifier an x : rest) []]

-- do <return ()>; return ()
monadStep wrap o@[Qualifier an x, Qualifier _ (fromRet -> Just (ret, unit))]
    | returnsUnit x, unit ~= "()"
    = [warn ("Redundant " ++ ret) (wrap o) (wrap $ take 1 o) []]

-- do x <- $1; return $ f $ g x ==> f . g <$> x
monadStep wrap
    o@[g@(Generator _ (view -> PVar_ u) x)
      ,q@(Qualifier _ (fromApplies -> (ret:f:fs, view -> Var_ v)))]
        | isReturn ret, notDol x, u == v, length fs < 3, all isSimple (f:fs), v `notElem` vars (f:fs)
        = [warn "Use <$>" (wrap o) (wrap [Qualifier an (InfixApp an (foldl' (flip (InfixApp an) (toNamed ".")) f fs) (toNamed "<$>") x)])
            [Replace Stmt (toSS g) (("x", toSS x):zip vs (toSS <$> f:fs)) (intercalate " . " (take (length fs + 1) vs) ++ " <$> x"), Delete Stmt (toSS q)]]
    where
        isSimple (fromApps -> xs) = all isAtom (x:xs)
        vs = ('f':) . show <$> [0..]
        notDol (InfixApp _ _ op _) = not $ isDol op
        notDol _ = True

monadStep _ _ = []

-- Suggest removing a return
monadSteps :: ([Stmt S] -> Exp_) -> [Stmt S] -> [Idea]
monadSteps wrap (x:xs) = monadStep wrap (x:xs) ++ monadSteps (wrap . (x :)) xs
monadSteps _ _ = []


-- | do ...; x <- return y; ... ==> do ...; let x = y; ...
monadLet :: [Stmt S] -> Maybe ([Stmt S], [Refactoring R.SrcSpan])
monadLet xs = if null rs then Nothing else Just (ys, rs)
    where
        (ys, catMaybes -> rs) = unzip $ map mkLet xs
        vs = concatMap pvars [p | Generator _ p _ <- xs]
        mkLet g@(Generator _ v@(view -> PVar_ p) (fromRet -> Just (_, y)))
            | p `notElem` vars y, p `notElem` delete p vs
            = (template (toNamed p) y, Just refact)
         where
            refact = Replace Stmt (toSS g) [("lhs", toSS v), ("rhs", toSS y)]
                      (prettyPrint $ template (toNamed "lhs") (toNamed "rhs"))
        mkLet x = (x, Nothing)
        template lhs rhs = LetStmt an $ BDecls an [PatBind an lhs (UnGuardedRhs an rhs) Nothing]


fromApplies :: Exp_ -> ([Exp_], Exp_)
fromApplies (App _ f x) = first (f:) $ fromApplies (fromParen x)
fromApplies (InfixApp _ f (isDol -> True) x) = first (f:) $ fromApplies x
fromApplies x = ([], x)


-- | Match @return x@ to @Just x@.
fromRet :: Exp_ -> Maybe (String, Exp_)
fromRet (Paren _ x) = fromRet x
fromRet (InfixApp _ x y z) | opExp y ~= "$" = fromRet $ App an x z
fromRet (App _ x y) | isReturn x = Just (prettyPrint x, y)
fromRet _ = Nothing

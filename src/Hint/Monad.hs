{-# LANGUAGE LambdaCase, ViewPatterns, PatternGuards, FlexibleContexts #-}

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
yes = do x <- bar; return $ f (g x) -- do f . g <$> bar
yes = do x <- bar; return (f $ g x) -- do f . g <$> bar
no = do x <- bar; return (f x x)
yes = do x <- bar; f x -- do bar >>= f
no = do x <- bar; f x x
{-# LANGUAGE RecursiveDo #-}; no = mdo hook <- mkTrigger pat (act >> rmHook hook) ; return hook
yes = do x <- return y; foo x -- @Suggestion do let x = y; foo x
yes = do x <- return $ y + z; foo x -- do let x = y + z; foo x
no = do x <- return x; foo x
no = do x <- return y; x <- return y; foo x
yes = do forM files $ \x -> return (); return () -- forM_ files $ \x -> return ()
yes = do if a then forM x y else sequence z q; return () -- if a then forM_ x y else sequence_ z q
yes = do case a of {_ -> forM x y; x:xs -> forM x xs}; return () -- case a of _ -> forM_ x y ; x:xs -> forM_ x xs
foldM_ f a xs = foldM f a xs >> return ()
folder f a xs = foldM f a xs >> return () -- foldM_ f a xs
folder f a xs = foldM f a xs >>= \_ -> return () -- foldM_ f a xs
yes = mapM async ds >>= mapM wait >> return () -- mapM async ds >>= mapM_ wait
main = "wait" ~> do f a $ sleep 10
main = f $ do g a $ sleep 10 -- g a $ sleep 10
main = do f a $ sleep 10 -- f a $ sleep 10
</TEST>
-}


module Hint.Monad(monadHint) where

import Control.Applicative
import Data.Tuple.Extra
import Data.Maybe
import Data.List
import Hint.Type
import Refact.Types
import qualified Refact.Types as R
import Prelude


badFuncs = ["mapM","foldM","forM","replicateM","sequence","zipWithM"]


monadHint :: DeclHint
monadHint _ _ d = concatMap (monadExp d) $ universeParentExp d

monadExp :: Decl_ -> (Maybe (Int, Exp_), Exp_) -> [Idea]
monadExp decl (parent, x) = case x of
        (view -> App2 op x1 x2) | op ~= ">>" -> f x1
        (view -> App2 op x1 (view -> LamConst1 _)) | op ~= ">>=" -> f x1
        Do _ xs -> [warn "Redundant return" x (Do an y) rs | Just (y, rs) <- [monadReturn xs]] ++
                   [warn "Use join" x (Do an y) rs | Just (y, rs) <- [monadJoin xs ['a'..'z']]] ++
                   [warn ("Use " ++ name) x (Do an y) rs | Just (y, rs, name) <- [monadFmapOrBind xs]] ++
                   [warn "Redundant do" x y [Replace Expr (toSS x) [("y", toSS y)] "y"]
                        | [Qualifier _ y] <- [xs], not $ doOperator parent y] ++
                   [suggest "Use let" x (Do an y) rs | Just (y, rs) <- [monadLet xs]] ++
                   concat [f x | Qualifier _ x <- init xs] ++
                   concat [f x | Generator _ (PWildCard _) x <- init xs]
        _ -> []
    where
        f x = [warn ("Use " ++ name) x y r  | Just (name,y, r) <- [monadCall x], fromNamed decl /= name]

-- Sometimes people write a * do a + b, to avoid brackets
doOperator (Just (1, InfixApp _ _ op _)) InfixApp{} | not $ isDol op = True
doOperator _ _ = False

middle :: (b -> d) -> (a, b, c) -> (a, d, c)
middle f (a,b,c) = (a, f b, c)


-- see through Paren and down if/case etc
-- return the name to use in the hint, and the revised expression
monadCall :: Exp_ -> Maybe (String,Exp_, [Refactoring R.SrcSpan])
monadCall (Paren l x) = middle (Paren l) <$> monadCall x
monadCall (App l x y) = middle (\x -> App l x y) <$> monadCall x
monadCall (InfixApp l x op y)
    | isDol op = middle (\x -> InfixApp l x op y) <$> monadCall x
    | op ~= ">>=" = middle (InfixApp l x op) <$> monadCall y
monadCall (replaceBranches -> (bs@(_:_), gen)) | all isJust res
    = Just ("Use simple functions", gen $ map (\(Just (a,b,c)) -> b) res, rs)
    where res = map monadCall bs
          rs  = concatMap (\(Just (a,b,c)) -> c) res
monadCall x | x2:_ <- filter (x ~=) badFuncs = let x3 = x2 ++ "_" in  Just (x3, toNamed x3, [Replace Expr (toSS x) [] x3])
monadCall _ = Nothing

monadFmapOrBind :: [Stmt S] -> Maybe ([Stmt S], [Refactoring R.SrcSpan], String)
monadFmapOrBind (reverse ->
                 q@(Qualifier _ (let go = \ case App _ f x                      -> first (f:) $ go (fromParen x)
                                                 InfixApp _ f (isDol -> True) x -> first (f:) $ go x
                                                 x -> ([], x)
                                 in go -> (fs, view -> Var_ v))):g@(Generator _ (view -> PVar_ u) x):rest)
    | Just ((~= "return") . fromParen -> True) <- case x of App _ ret _                      -> Just ret
                                                            InfixApp _ ret (isDol -> True) _ -> Just ret
                                                            _ -> Nothing
    = Nothing
    | u == v, v `notElem` vars fs, (rets, f:fs) <- span (~= "return") fs
    = (\ (a, b, c) ->
       (reverse (Qualifier an a:rest),
        [Replace Stmt (toSS g) (("x", toSS x):zip vs (toSS <$> f:fs)) (b $ intercalate " . " vs),
         Delete Stmt (toSS q)], c)) <$>
      let f' = foldl' (flip (InfixApp an) (toNamed ".")) f fs
      in case rets of
          []  -> Just (InfixApp an x (toNamed ">>=") f', ("x >>= " ++), "bind")
          [_] -> Just (InfixApp an f' (toNamed "<$>") x, (++ " <$> x"), "fmap")
          _   -> Nothing
  where vs = ('f':) . show <$> [0..]
monadFmapOrBind _ = Nothing

monadReturn :: [Stmt S] -> Maybe ([Stmt S], [Refactoring R.SrcSpan])
monadReturn (reverse -> q@(Qualifier _ (App _ ret (Var _ v))):g@(Generator _ (PVar _ p) x):rest)
    | ret ~= "return", fromNamed v == fromNamed p
    = Just (reverse (Qualifier an x : rest),
            [Replace Stmt (toSS g) [("x", toSS x)] "x", Delete Stmt (toSS q)])
monadReturn _ = Nothing

monadJoin :: [Stmt S] -> String -> Maybe ([Stmt S], [Refactoring R.SrcSpan])
monadJoin (g@(Generator _ (view -> PVar_ p) x):q@(Qualifier _ (view -> Var_ v)):xs) (c:cs)
    | p == v && v `notElem` varss xs
    = Just . f $ fromMaybe def (monadJoin xs cs)
    where
      gen expr = Qualifier (ann x) (rebracket1 $ App an (toNamed "join") expr)
      def = (xs, [])
      f (ss, rs) = (s:ss, r ++ rs)
      s = gen x
      r = [Replace Stmt (toSS g) [("x", toSS x)] "join x", Delete Stmt (toSS q)]

monadJoin (x:xs) cs = first (x:)   <$> monadJoin xs cs
monadJoin [] _ = Nothing

monadLet :: [Stmt S] -> Maybe ([Stmt S], [Refactoring R.SrcSpan])
monadLet xs = if null rs then Nothing else Just (ys, rs)
    where
        (ys, catMaybes -> rs) = unzip $ map mkLet xs
        vs = concatMap pvars [p | Generator _ p _ <- xs]
        mkLet g@(Generator _ v@(view -> PVar_ p) (fromRet -> Just y))
            | p `notElem` vars y, p `notElem` delete p vs
            = (template (toNamed p) y, Just refact)
         where
            refact = Replace Stmt (toSS g) [("lhs", toSS v), ("rhs", toSS y)]
                      (prettyPrint $ template (toNamed "lhs") (toNamed "rhs"))
        mkLet x = (x, Nothing)
        template lhs rhs = LetStmt an $ BDecls an [PatBind an lhs (UnGuardedRhs an rhs) Nothing]

fromRet (Paren _ x) = fromRet x
fromRet (InfixApp _ x y z) | opExp y ~= "$" = fromRet $ App an x z
fromRet (App _ x y) | x ~= "return" = Just y
fromRet _ = Nothing

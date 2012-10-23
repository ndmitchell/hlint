{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Find and match:

    mapM, foldM, forM, replicateM, sequence, zipWithM
    not at the last line of a do statement, or to the left of >>

    Use let x = y instead of x <- return y, unless x is contained
    within y, or bound more than once in that do block.

<TEST>
yes = do mapM print a; return b -- mapM_ print a
no = mapM print a
no = do foo ; mapM print a
yes = do (bar+foo) -- (bar+foo)
no = do bar ; foo
yes = do bar; a <- foo; return a -- do bar; foo
no = do bar; a <- foo; return b
yes = do x <- bar; x -- do join bar
no = do x <- bar; x; x
no = mdo hook <- mkTrigger pat (act >> rmHook hook) ; return hook
yes = do x <- return y; foo x -- @Warning do let x = y; foo x
yes = do x <- return $ y + z; foo x -- do let x = y + z; foo x
no = do x <- return x; foo x
no = do x <- return y; x <- return y; foo x
yes = do forM files $ \x -> return (); return () -- forM_ files $ \x -> return ()
yes = do if a then forM x y else sequence z q; return () -- if a then forM_ x y else sequence_ z q
yes = do case a of {_ -> forM x y; x:xs -> forM x xs}; return () -- case a of _ -> forM_ x y ; x:xs -> forM_ x xs
foldM_ f a xs = foldM f a xs >> return ()
folder f a xs = foldM f a xs >> return () -- foldM_ f a xs
yes = mapM async ds >>= mapM wait >> return () -- mapM async ds >>= mapM_ wait
</TEST>
-}


module Hint.Monad where

import Control.Arrow
import Data.Maybe
import Data.List
import Hint.Type


badFuncs = ["mapM","foldM","forM","replicateM","sequence","zipWithM"]


monadHint :: DeclHint
monadHint _ _ d = concatMap (monadExp d) $ universeBi d

monadExp :: Decl_ -> Exp_ -> [Idea]
monadExp decl x = case x of
        (view -> App2 op x1 x2) | op ~= ">>" -> f x1
        Do _ xs -> [err "Redundant return" x y | Just y <- [monadReturn xs]] ++
                   [err "Use join" x (Do an y) | Just y <- [monadJoin xs]] ++
                   [err "Redundant do" x y | [Qualifier _ y] <- [xs]] ++
                   [warn "Use let" x (Do an y) | Just y <- [monadLet xs]] ++
                   concat [f x | Qualifier _ x <- init xs]
        _ -> []
    where
        f x = [err ("Use " ++ name) x y | Just (name,y) <- [monadCall x], fromNamed decl /= name]


-- see through Paren and down if/case etc
-- return the name to use in the hint, and the revised expression
monadCall :: Exp_ -> Maybe (String,Exp_)
monadCall (Paren _ x) = fmap (second $ Paren an) $ monadCall x
monadCall (App _ x y) = fmap (second $ \x -> App an x y) $ monadCall x
monadCall (InfixApp _ x op y)
    | isDol op = fmap (second $ \x -> InfixApp an x op y) $ monadCall x
    | op ~= ">>=" = fmap (second $ \y -> InfixApp an x op y) $ monadCall y
monadCall (replaceBranches -> (bs@(_:_), gen)) | all isJust res
    = Just (fst $ fromJust $ head res, gen $ map (snd . fromJust) res)
    where res = map monadCall bs
monadCall x | x:_ <- filter (x ~=) badFuncs = let x2 = x ++ "_" in  Just (x2, toNamed x2)
monadCall _ = Nothing


monadReturn (reverse -> Qualifier _ (App _ ret (Var _ v)):Generator _ (PVar _ p) x:rest)
    | ret ~= "return", fromNamed v == fromNamed p
    = Just $ Do an $ reverse $ Qualifier an x : rest
monadReturn _ = Nothing


monadJoin (Generator _ (view -> PVar_ p) x:Qualifier _ (view -> Var_ v):xs)
    | p == v && v `notElem` vars xs
    = Just $ Qualifier an (rebracket1 $ App an (toNamed "join") x) : fromMaybe xs (monadJoin xs)
monadJoin (x:xs) = fmap (x:) $ monadJoin xs
monadJoin [] = Nothing


monadLet xs = if xs == ys then Nothing else Just ys
    where
        ys = map mkLet xs
        vs = concatMap pvars [p | Generator _ p _ <- xs]
        mkLet (Generator _ (view -> PVar_ p) (fromRet -> Just y))
            | p `notElem` vars y, p `notElem` delete p vs
            = LetStmt an $ BDecls an [PatBind an (toNamed p) Nothing (UnGuardedRhs an y) Nothing]
        mkLet x = x

fromRet (Paren _ x) = fromRet x
fromRet (InfixApp _ x y z) | opExp y ~= "$" = fromRet $ App an x z
fromRet (App _ x y) | x ~= "return" = Just y
fromRet _ = Nothing

{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Find and match:

    mapM, foldM, forM, replicateM, sequence, zipWithM
    not at the last line of a do statement, or to the left of >>

<TEST>
yes = do mapM print a; return b where res = mapM_ print a
no = mapM print a
no = do foo ; mapM print a
yes = do (bar+foo) where res = (bar+foo)
no = do bar ; foo
yes = do bar; a <- foo; return a where res = do bar; foo
no = do bar; a <- foo; return b
yes = do x <- bar; x where res = do join bar
no = do x <- bar; x; x
no = mdo hook <- mkTrigger pat (act >> rmHook hook) ; return hook
</TEST>
-}


module Hint.Monad where

import Control.Arrow
import Control.Monad
import Data.Maybe
import HSE.All
import Type


badFuncs = ["mapM","foldM","forM","replicateM","sequence","zipWithM"]


monadHint :: DeclHint
monadHint _ = concatMap monadExp . universeExp nullSrcLoc

monadExp :: (SrcLoc,Exp) -> [Idea]
monadExp (loc,x) = case x of
        (view -> App2 op x1 x2) | op ~= ">>" -> f x1
        Do xs -> [idea Error "Redundant return" loc x y | Just y <- [monadReturn xs]] ++
                 [idea Error "Use join" loc x (Do y) | Just y <- [monadJoin xs]] ++
                 [idea Error "Redundant do" loc x y | [Qualifier y] <- [xs]] ++
                 concat [f x | Qualifier x <- init xs]
        _ -> []
    where
        f x = [idea Error ("Use " ++ name) loc x y
              |Just (name,y) <- [monadCall x]]


-- see through Paren and down if/case etc
monadCall :: Exp -> Maybe (String,Exp)
monadCall (Paren x) = liftM (second Paren) $ monadCall x
monadCall (App x y) = liftM (second (`App` y)) $ monadCall x
monadCall x | x:_ <- filter (x ~=) badFuncs = let x2 = x ++ "_" in  Just (x2, toNamed x2)
monadCall _ = Nothing


monadReturn (reverse -> Qualifier (App ret (Var v)):Generator _ (PVar p) x:rest)
    | ret ~= "return", fromNamed v == fromNamed p
    = Just $ Do $ reverse $ Qualifier x : rest
monadReturn _ = Nothing


monadJoin (Generator _ (PVar p) x:Qualifier (Var v):xs)
    | fromNamed p == fromNamed v && Var v `notElem` universeBi xs
    = Just $ Qualifier (ensureBracket1 $ App (toNamed "join") x) : fromMaybe xs (monadJoin xs)
monadJoin (x:xs) = liftM (x:) $ monadJoin xs
monadJoin [] = Nothing

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
yes = do x <- return y; foo x -- do let x = y; foo x
yes = do x <- return $ y + z; foo x -- do let x = y + z; foo x
no = do x <- return x; foo x
no = do x <- return y; x <- return y; foo x
</TEST>
-}


module Hint.Monad where

import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.List
import HSE.All
import Type
import Hint


badFuncs = ["mapM","foldM","forM","replicateM","sequence","zipWithM"]


monadHint :: DeclHint
monadHint _ _ = concatMap monadExp . universeExp nullSrcLoc

monadExp :: (SrcLoc,Exp) -> [Idea]
monadExp (loc,x) = case x of
        (view -> App2 op x1 x2) | op ~= ">>" -> f x1
        Do xs -> [idea Error "Redundant return" loc x y | Just y <- [monadReturn xs]] ++
                 [idea Error "Use join" loc x (Do y) | Just y <- [monadJoin xs]] ++
                 [idea Error "Redundant do" loc x y | [Qualifier y] <- [xs]] ++
                 [idea Error "Use let" loc x (Do y) | Just y <- [monadLet xs]] ++
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


monadLet xs = if xs == ys then Nothing else Just ys
    where
        ys = map mkLet xs
        vars = [v | Generator _ p _ <- xs, PVar v <- universe p]
        mkLet (Generator sl (PVar p) (fromRet -> Just y))
            | p `notElem` [v | Var (UnQual v) <- universeBi y], p `notElem` delete p vars
            = LetStmt $ BDecls [PatBind sl (PVar p) Nothing (UnGuardedRhs y) (BDecls [])]
        mkLet x = x

fromRet (Paren x) = fromRet x
fromRet (InfixApp x y z) | opExp y ~= "$" = fromRet $ App x z
fromRet (App x y) | x ~= "return" = Just y
fromRet _ = Nothing

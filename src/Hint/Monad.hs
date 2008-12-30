{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Find and match:

    mapM, foldM, forM, replicateM, sequence, zipWithM
    not at the last line of a do statement, or to the left of >>

<TEST>
yes = do mapM print a; return b
no = mapM print a
no = do foo ; mapM print a
yes = do (bar+foo)
no = do bar ; foo
</TEST>
-}


module Hint.Monad where

import Control.Monad
import HSE.All
import Type


badFuncs = ["mapM","foldM","forM","replicateM","sequence","zipWithM"]


monadHint :: Hint
monadHint = concatMap monadExp . universeExp nullSrcLoc

monadExp :: (SrcLoc,Exp) -> [Idea]
monadExp (loc,x) = case x of
        (view -> App2 op x1 x2) | op ~= ">>" -> f x1
        Do xs -> [idea "Redundant do" loc x y | [Qualifier y] <- [xs]] ++
                 concat [f x | Qualifier x <- init xs]
        MDo xs -> monadExp (loc, Do xs)
        _ -> []
    where
        f x = [idea "Use a more efficient monadic variant" loc x y
              |Just y <- [monadCall x]]


-- see through Paren and down if/case etc
monadCall :: Exp -> Maybe Exp
monadCall (Paren x) = liftM Paren $ monadCall x
monadCall (App x y) = liftM (`App` y) $ monadCall x
monadCall x | x:_ <- filter (x ~=) badFuncs = Just $ toVar (x ++ "_")
monadCall _ = Nothing

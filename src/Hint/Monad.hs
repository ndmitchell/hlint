{-# LANGUAGE ViewPatterns, PatternGuards, FlexibleContexts #-}

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
monadHint _ _ d = concatMap (monadExp d) $ universeBi d

monadExp :: Decl_ -> Exp_ -> [Idea]
monadExp decl x = case x of
        (view -> App2 op x1 x2) | op ~= ">>" -> f x1
        Do _ xs -> [(err "Redundant return" x (Do an y)) { ideaRefactoring = rs } | Just (y, rs) <- [monadReturn xs]] ++
                   [(err "Use join" x (Do an y)) { ideaRefactoring = rs } | Just (y, rs) <- [monadJoin xs ['a'..'z']]] ++
                   [err' "Redundant do" x y [("y", ann y)] "y" | [Qualifier _ y] <- [xs]] ++
                   [(warn "Use let" x (Do an y)) { ideaRefactoring = rs } | Just (y, rs) <- [monadLet xs]] ++
                   concat [f x | Qualifier _ x <- init xs]
        _ -> []
    where
        f x = [err ("Use " ++ name) x y | Just (name,y) <- [monadCall x], fromNamed decl /= name]
        split [] = ([], [], [])
        split ((s, v, t):xs) =
          let (ss, subts, ts) = split xs in
            (s:ss, (v, ann t): subts, t:ts)


-- see through Paren and down if/case etc
-- return the name to use in the hint, and the revised expression
monadCall :: Exp_ -> Maybe (String,Exp_)
monadCall (Paren _ x) = second (Paren an) <$> monadCall x
monadCall (App _ x y) = second (\x -> App an x y) <$> monadCall x
monadCall (InfixApp _ x op y)
    | isDol op = second (\x -> InfixApp an x op y) <$> monadCall x
    | op ~= ">>=" = second (\y -> InfixApp an x op y) <$> monadCall y
monadCall (replaceBranches -> (bs@(_:_), gen)) | all isJust res
    = Just (fst $ fromJust $ head res, gen $ map (snd . fromJust) res)
    where res = map monadCall bs
monadCall x | x:_ <- filter (x ~=) badFuncs = let x2 = x ++ "_" in  Just (x2, toNamed x2)
monadCall _ = Nothing

monadReturn :: [Stmt S] -> Maybe ([Stmt S], [Refactoring R.SrcSpan])
monadReturn (reverse -> q@(Qualifier _ (App _ ret (Var _ v))):g@(Generator _ (PVar _ p) x):rest)
    | ret ~= "return", fromNamed v == fromNamed p
    = Just (reverse (Qualifier an x : rest),
            [Replace Stmt (toSS g) [("x", toSS x)] "x", Delete (toSS q)])
monadReturn _ = Nothing

monadJoin :: [Stmt S] -> [Char] -> Maybe ([Stmt S], [Refactoring R.SrcSpan])
monadJoin (g@(Generator _ (view -> PVar_ p) x):q@(Qualifier _ (view -> Var_ v)):xs) (c:cs)
    | p == v && v `notElem` varss xs
    = Just . f $ fromMaybe def (monadJoin xs cs)
    where
      gen expr = Qualifier (ann x) (rebracket1 $ App an (toNamed "join") expr)
      def = (xs, [])
      f (ss, rs) = (s:ss, r ++ rs)
      s = gen x
      r = [Replace Stmt (toSS g) [("x", toSS x)] "join x", Delete (toSS q)]

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

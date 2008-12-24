{-# LANGUAGE PatternGuards #-}

{-
<TEST>
yes = (f x) x
no = f (x x)
yes = (f x) ||| y
yes = if (f x) then y else z
yes = if x then (f y) else z
</TEST>
-}


module Hint.Bracket where

import Control.Monad
import Control.Monad.State
import Data.Generics
import Data.Maybe
import Type
import HSE.Util
import Language.Haskell.Exts


bracketHint :: Hint
bracketHint = concatMap bracketExp . universeExp nullSrcLoc

bracketExp :: (SrcLoc,Exp) -> [Idea]
bracketExp (loc,x) = [idea "Use fewer brackets" loc x y | Just y <- [f x]]
    where
        f :: Exp -> Maybe Exp
        f (Paren x) | atom x = Just x
        f x = if cs /= [] && b then Just r else Nothing
            where
                (r,(b,[])) = runState (gmapM g x) (False, cs)
                cs = snd $ precedence x
        
        g :: Data a => a -> State (Bool,[Int]) a
        g x | Just y <- cast x = do
              (b,c:cs) <- get
              liftM (fromJust . cast) $ case y of
                  Paren z | fst (precedence z) < c -> put (True, cs) >> return z
                  _ -> put (b, cs) >> return y
        g x = return x


-- return my precedence, and the precedence of my children
-- higher precedence means no brackets
-- if the object in a position has a lower priority, the brackets are unnecessary
precedence :: Exp -> (Int,[Int])
precedence x = case x of
        If{} -> block * [block,block,block]
        Let{} -> block * [block]
        Case{} -> block * [block]
        InfixApp{} -> op * [op,op]
        App{} -> appL * [appR,appL]
        _ -> top * []
    where
        (*) = (,)
        appL:appR:op:block:top:_ = [1..]

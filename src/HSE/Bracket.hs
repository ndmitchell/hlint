{-# LANGUAGE PatternGuards, ViewPatterns, MultiParamTypeClasses #-}

module HSE.Bracket where

import Control.Monad.State
import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Language.Haskell.Exts


addParen :: Exp -> Exp
addParen x = if isAtom x then x else XExpTag x


remParen :: Exp -> Exp
remParen = transform g . transform f
    where
        g (XExpTag x) = Paren x
        g x = x
    
        f (XExpTag x) | isAtom x = x
        f (InfixApp a b c) = InfixApp (f2 a) b (f2 c)
        f x = x
        
        f2 (XExpTag (App a b)) = App a b
        f2 x = x


isParen :: Exp -> Bool
isParen (Paren _) = True
isParen (XExpTag _) = True
isParen _ = False


fromParen :: Exp -> Exp
fromParen (Paren x) = fromParen x
fromParen (XExpTag x) = fromParen x
fromParen x = x


paren :: Exp -> Exp
paren x = if isAtom x then x else Paren x


-- | Is this item lexically requiring no bracketing ever
--   i.e. is totally atomic
isAtom :: Exp -> Bool
isAtom x = case x of
    XExpTag{} -> True -- because pretending to be Paren
    Paren{} -> True
    Var{} -> True
    Con{} -> True
    Lit{} -> True
    Tuple{} -> True
    List{} -> True
    LeftSection{} -> True
    RightSection{} -> True
    RecConstr{} -> True
    ListComp{} -> True
    _ -> False




-- Nothing = I don't know, i.e. because of fixities
needBracket :: Int -> Exp -> Exp -> Maybe Bool
needBracket i parent child 
    | isAtom child = Just False
    | InfixApp{} <- parent, App{} <- child = Just False
    | otherwise = Nothing

{-
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
        _ -> unknown * []
    where
        (*) = (,)
        unknown:appL:appR:op:block:top:_ = [1..]
-}


-- True implies I changed this level
descendBracket :: (Exp -> (Bool, Exp)) -> Exp -> Exp
descendBracket f x = flip evalState 0 $ flip descendM x $ \y -> do
    i <- get
    modify (+1)
    (b,y) <- return $ f y
    let p = if b && needBracket i x y /= Just False then Paren else id
    return $ p y


transformBracket :: (Exp -> Maybe Exp) -> Exp -> Exp
transformBracket f = snd . g
    where
        g = f2 . descendBracket g
        f2 x = maybe (False,x) ((,) True) (f x)


-- ensure that all the 1-level children are appropriately bracketed
ensureBracket1 :: Exp -> Exp
ensureBracket1 = descendBracket ((,) True)

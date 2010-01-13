{-# LANGUAGE PatternGuards #-}

module HSE.Bracket where

import Control.Monad.State
import Data.Maybe
import HSE.Type
import HSE.Util


paren :: Exp_ -> Exp_
paren x = if isAtom x then x else Paren (ann x) x


-- | Is this item lexically requiring no bracketing ever
--   i.e. is totally atomic
isAtom :: Exp_ -> Bool
isAtom x = case x of
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
    EnumFrom{} -> True
    EnumFromTo{} -> True
    EnumFromThen{} -> True
    EnumFromThenTo{} -> True
    _ -> False


-- Err on the side of caution, True = don't know
needBracket :: Int -> Exp_ -> Exp_ -> Bool
needBracket i parent child 
    | isAtom child = False
    | InfixApp{} <- parent, isApp child = False
    | ListComp{} <- parent = False
    | List{} <- parent = False
    | If{} <- parent, isAnyApp child = False
    | App{} <- parent, i == 0, isApp child = False
    | ExpTypeSig{} <- parent, i == 0 = False
    | otherwise = True


-- True implies I changed this level
descendBracket :: (Exp_ -> (Bool, Exp_)) -> Exp_ -> Exp_
descendBracket f x = flip evalState 0 $ flip descendM x $ \y -> do
    i <- get
    modify (+1)
    (b,y) <- return $ f y
    let p = if b && needBracket i x y then (\x -> Paren (ann x) x) else id
    return $ p y


transformBracket :: (Exp_ -> Maybe Exp_) -> Exp_ -> Exp_
transformBracket f = snd . g
    where
        g = f2 . descendBracket g
        f2 x = maybe (False,x) ((,) True) (f x)


-- ensure that all the 1-level children are appropriately bracketed
ensureBracket1 :: Exp_ -> Exp_
ensureBracket1 = descendBracket ((,) True)

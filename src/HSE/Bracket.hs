{-# LANGUAGE PatternGuards, TypeSynonymInstances #-}

module HSE.Bracket where

import Control.Monad.State
import Data.Maybe
import HSE.Type
import HSE.Util



class Brackets a where
    remParen :: a -> Maybe a -- remove one paren, or Nothing if there is no paren
    addParen :: a -> a -- write out a paren

    -- | Is this item lexically requiring no bracketing ever
    --   i.e. is totally atomic
    isAtom :: a -> Bool

    -- | Is the child safe free from brackets in the parent position.
    --   Err on the side of caution, True = don't know
    needBracket :: Int -> a -> a -> Bool


instance Brackets Exp_ where
    remParen (Paren _ x) = Just x
    remParen _ = Nothing
    addParen = Paren an

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

    needBracket i parent child 
        | isAtom child = False
        | InfixApp{} <- parent, isApp child = False
        | ListComp{} <- parent = False
        | List{} <- parent = False
        | If{} <- parent, isAnyApp child = False
        | App{} <- parent, i == 0, isApp child = False
        | ExpTypeSig{} <- parent, i == 0 = False
        | otherwise = True



paren :: Exp_ -> Exp_
paren x = if isAtom x then x else addParen x


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

{-# LANGUAGE PatternGuards, TypeSynonymInstances #-}

module HSE.Bracket where

import Control.Monad.State
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

    -- note: i is the index in children, not in the AST
    needBracket i parent child 
        | isAtom child = False
        | InfixApp{} <- parent, App{} <- child = False
        | ListComp{} <- parent = False
        | List{} <- parent = False
        | Tuple{} <- parent = False
        | If{} <- parent, isAnyApp child = False
        | App{} <- parent, i == 0, App{} <- child = False
        | ExpTypeSig{} <- parent, i == 0 = False
        | Paren{} <- parent = False
        | isDotApp parent, isDotApp child, i == 1 = False
        | otherwise = True


instance Brackets Type_ where
    remParen (TyParen _ x) = Just x
    remParen _ = Nothing
    addParen = TyParen an

    isAtom x = case x of
        TyParen{} -> True
        TyTuple{} -> True
        TyList{} -> True
        TyVar{} -> True
        TyCon{} -> True
        _ -> False

    needBracket i parent child
        | isAtom child = False
        | TyFun{} <- parent, i == 1, TyFun{} <- child = False
        | TyFun{} <- parent, TyApp{} <- child = False
        | TyTuple{} <- parent = False
        | TyList{} <- parent = False
        | TyInfix{} <- parent, TyApp{} <- child = False
        | TyParen{} <- parent = False
        | otherwise = True


instance Brackets Pat_ where
    remParen (PParen _ x) = Just x
    remParen _ = Nothing
    addParen = PParen an

    isAtom x = case x of
        PParen{} -> True
        PTuple{} -> True
        PList{} -> True
        PVar{} -> True
        PApp _ _ [] -> True
        PWildCard{} -> True
        _ -> False

    needBracket i parent child
        | isAtom child = False
        | PTuple{} <- parent = False
        | PList{} <- parent = False
        | PInfixApp{} <- parent, PApp{} <- child = False
        | PParen{} <- parent = False
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


-- a list of application, with any necessary brackets
appsBracket :: [Exp_] -> Exp_
appsBracket = foldl1 (\x -> ensureBracket1 . App an x)

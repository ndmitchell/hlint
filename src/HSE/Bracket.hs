{-# LANGUAGE PatternGuards, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fno-warn-incomplete-patterns #-}

module HSE.Bracket(
    Brackets(..),
    paren, transformBracket, rebracket1, appsBracket
    ) where

import HSE.Type
import HSE.Util
import Util


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
        Tuple{} -> True
        List{} -> True
        LeftSection{} -> True
        RightSection{} -> True
        TupleSection{} -> True
        RecConstr{} -> True
        ListComp{} -> True
        EnumFrom{} -> True
        EnumFromTo{} -> True
        EnumFromThen{} -> True
        EnumFromThenTo{} -> True
        _ -> isLexeme x

    -- note: i is the index in children, not in the AST
    needBracket i parent child
        | isAtom child = False
        | InfixApp{} <- parent, App{} <- child = False
        | isSection parent, App{} <- child = False
        | Let{} <- parent, App{} <- child = False
        | ListComp{} <- parent = False
        | List{} <- parent = False
        | Tuple{} <- parent = False
        | If{} <- parent, isAnyApp child = False
        | App{} <- parent, i == 0, App{} <- child = False
        | ExpTypeSig{} <- parent, i == 0, isApp child = False
        | Paren{} <- parent = False
        | isDotApp parent, isDotApp child, i == 1 = False
        | RecConstr{} <- parent = False
        | RecUpdate{} <- parent, i /= 0 = False
        | Case{} <- parent, i /= 0 || isAnyApp child = False
        | Lambda{} <- parent, i == length (universeBi parent :: [Pat_]) - 1 = False -- watch out for PViewPat
        | Do{} <- parent = False
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
-- a -> (b -> c) is not a required bracket, but useful for documentation about arity etc.
--        | TyFun{} <- parent, i == 1, TyFun{} <- child = False
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
        PRec{} -> True
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


-- | Add a Paren around something if it is not atomic
paren :: Exp_ -> Exp_
paren x = if isAtom x then x else addParen x


-- | Descend, and if something changes then add/remove brackets appropriately
descendBracket :: (Exp_ -> (Bool, Exp_)) -> Exp_ -> Exp_
descendBracket op x = descendIndex g x
    where
        g i y = if a then f i b else b
            where (a,b) = op y

        f i (Paren _ y) | not $ needBracket i x y = y
        f i y | needBracket i x y = addParen y
        f i y = y


transformBracket :: (Exp_ -> Maybe Exp_) -> Exp_ -> Exp_
transformBracket op = snd . g
    where
        g = f . descendBracket g
        f x = maybe (False,x) ((,) True) (op x)


-- | Add/remove brackets as suggested needBracket at 1-level of depth
rebracket1 :: Exp_ -> Exp_
rebracket1 = descendBracket (\x -> (True,x))


-- a list of application, with any necessary brackets
appsBracket :: [Exp_] -> Exp_
appsBracket = foldl1 (\x -> rebracket1 . App an x)

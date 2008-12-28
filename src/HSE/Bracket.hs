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
    XExpTag _ -> True -- because pretending to be Paren
    Paren _ -> True
    Var _ -> True
    Con _ -> True
    Lit _ -> True
    Tuple _ -> True
    List _ -> True
    LeftSection _ _ -> True
    RightSection _ _ -> True
    RecConstr _ _ -> True
    ListComp _ _ -> True
    _ -> False




-- Nothing = I don't know, i.e. because of fixities
needBracket :: Int -> Exp -> Exp -> Maybe Bool
needBracket _ _ _ = Nothing



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

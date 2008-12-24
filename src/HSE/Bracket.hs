{-# LANGUAGE PatternGuards, ViewPatterns, MultiParamTypeClasses #-}

module HSE.Bracket where

import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Language.Haskell.Exts


addParen, hsParen :: Exp -> Exp
addParen x = if atom x then x else XExpTag x
hsParen x = if atom x then x else Paren x

remParen :: Exp -> Exp
remParen = transform g . transform f
    where
        g (XExpTag x) = Paren x
        g x = x
    
        f (XExpTag x) | atom x = x
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

atom x = case x of
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

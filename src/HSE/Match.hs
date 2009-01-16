{-# LANGUAGE PatternGuards, ViewPatterns, MultiParamTypeClasses #-}

module HSE.Match where

import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Language.Haskell.Exts
import HSE.Bracket
import HSE.Util


class View a b where
    view :: a -> b


data App2 = NoApp2 | App2 Exp Exp Exp deriving Show

instance View Exp App2 where
    view (fromParen -> InfixApp lhs op rhs) = view $ opExp op `App` lhs `App` rhs
    view (fromParen -> (fromParen -> f `App` x) `App` y) = App2 f x y
    view _ = NoApp2


data App1 = NoApp1 | App1 Exp Exp deriving Show

instance View Exp App1 where
    view (fromParen -> f `App` x) = App1 f x
    view _ = NoApp1


data Infix = NoInfix | Infix Exp Exp Exp deriving Show

instance View Exp Infix where
    view (fromParen -> InfixApp a b c) = Infix a (opExp b) c
    view _ = NoInfix


(~=) :: Exp -> String -> Bool
(Con (Special Cons)) ~= ":" = True
(Con x) ~= y = Var x ~= y
(List []) ~= "[]" = True
x ~= y = fromVar x == Just y

{-# LANGUAGE PatternGuards, ViewPatterns, MultiParamTypeClasses #-}

module HSE.Match where

import HSE.Generics
import Data.Char
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


-- | fromNamed will return "" when it cannot be represented
--   toNamed may crash on ""
class Named a where
    toNamed :: String -> a
    fromNamed :: a -> String

instance Named Exp where
    fromNamed (Var x) = fromNamed x
    fromNamed (Con x) = fromNamed x
    fromNamed (List []) = "[]"
    fromNamed _ = ""
    
    toNamed "[]" = List []
    toNamed xs@(x:_) | isUpper x || x == ':' = Con $ toNamed xs
                     | otherwise = toNamed xs

instance Named QName where
    fromNamed (Special Cons) = ":"
    fromNamed (UnQual x) = fromNamed x
    fromNamed _ = ""

    toNamed ":" = Special Cons
    toNamed x = UnQual $ toNamed x

instance Named Name where
    fromNamed (Ident x) = x
    fromNamed (Symbol x) = x

    toNamed xs@(x:_) | isAlpha x || x `elem` "_'" = Ident xs
                     | otherwise = Symbol xs

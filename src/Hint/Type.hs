
module Hint.Type(module Hint.Type, module Idea, module HSE.All) where

import HSE.All
import Idea
import Data.Monoid


type DeclHint = Scope -> Module_ -> Decl_ -> [Idea]
type ModuHint = Scope -> Module_          -> [Idea]
type CrossHint = [(Scope, Module_)] -> [Idea]

data Hint = Hint
    {hintModules :: [(Scope, Module SrcSpanInfo)] -> [Idea]
    ,hintModule :: Scope -> Module SrcSpanInfo -> [Idea]
    ,hintDecl :: Scope -> Module SrcSpanInfo -> Decl SrcSpanInfo -> [Idea]
    }

instance Monoid Hint where
    mempty = Hint (\_ -> []) (\_ _ -> []) (\_ _ _ -> [])
    mappend (Hint x1 x2 x3) (Hint y1 y2 y3) = Hint (\a -> x1 a ++ y1 a) (\a b -> x2 a b ++ y2 a b) (\a b c -> x3 a b c ++ y3 a b c)

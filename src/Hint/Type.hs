
module Hint.Type(module Hint.Type, module Idea, module HSE.All, module Refact) where

import Data.Monoid
import HSE.All
import Idea
import Prelude
import Refact


type DeclHint = Scope -> Module_ -> Decl_ -> [Idea]
type ModuHint = Scope -> Module_          -> [Idea]
type CrossHint = [(Scope, Module_)] -> [Idea]

-- | Functions to generate hints, combined using the 'Monoid' instance.
data Hint = Hint
    {hintModules :: [(Scope, Module SrcSpanInfo)] -> [Idea] -- ^ Given a list of modules (and their scope information) generate some 'Idea's.
    ,hintModule :: Scope -> Module SrcSpanInfo -> [Idea] -- ^ Given a single module and its scope information generate some 'Idea's.
    ,hintDecl :: Scope -> Module SrcSpanInfo -> Decl SrcSpanInfo -> [Idea]
        -- ^ Given a declaration (with a module and scope) generate some 'Idea's.
        --   This function will be partially applied with one module/scope, then used on multiple 'Decl' values.
    ,hintComment :: Comment -> [Idea] -- ^ Given a comment generate some 'Idea's.
    }

instance Monoid Hint where
    mempty = Hint (const []) (\_ _ -> []) (\_ _ _ -> []) (const [])
    mappend (Hint x1 x2 x3 x4) (Hint y1 y2 y3 y4) =
        Hint (\a -> x1 a ++ y1 a) (\a b -> x2 a b ++ y2 a b) (\a b c -> x3 a b c ++ y3 a b c) (\a -> x4 a ++ y4 a)

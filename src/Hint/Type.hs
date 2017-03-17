
module Hint.Type(
    DeclHint, ModuHint, CrossHint, Hint(..),
    module Export
    ) where

import Data.Monoid
import Config.Type
import HSE.All  as Export
import Idea     as Export
import Prelude
import Refact   as Export


type DeclHint = Scope -> Module_ -> Decl_ -> [Idea]
type ModuHint = Scope -> Module_          -> [Idea]
type CrossHint = [(Scope, Module_)] -> [Idea]

-- | Functions to generate hints, combined using the 'Monoid' instance.
data Hint = Hint
    {hintModules :: [Setting] -> [(Scope, Module_)] -> [Idea] -- ^ Given a list of modules (and their scope information) generate some 'Idea's.
    ,hintModule :: [Setting] -> Scope -> Module_ -> [Idea] -- ^ Given a single module and its scope information generate some 'Idea's.
    ,hintDecl :: [Setting] -> Scope -> Module_ -> Decl_ -> [Idea]
        -- ^ Given a declaration (with a module and scope) generate some 'Idea's.
        --   This function will be partially applied with one module/scope, then used on multiple 'Decl' values.
    ,hintComment :: [Setting] -> Comment -> [Idea] -- ^ Given a comment generate some 'Idea's.
    }

instance Monoid Hint where
    mempty = Hint (\_ _ -> []) (\_ _ _ -> []) (\_ _ _ _ -> []) (\_ _ -> [])
    mappend (Hint x1 x2 x3 x4) (Hint y1 y2 y3 y4) = Hint
        (\a b -> x1 a b ++ y1 a b)
        (\a b c -> x2 a b c ++ y2 a b c)
        (\a b c d -> x3 a b c d ++ y3 a b c d)
        (\a b -> x4 a b ++ y4 a b)

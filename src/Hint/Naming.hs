{-# LANGUAGE FlexibleContexts #-}
{-
    Suggest the use of camelCase

    Only permit:
    _*[A-Za-z]*_*#*'*

    Apply this to things that would get exported by default only
    Also allow prop_ as it's a standard QuickCheck idiom
    Also allow case_ as it's a standard test-framework-th idiom
    Also don't suggest anything mentioned elsewhere in the module

<TEST>
data Yes = Foo | Bar'Test -- data Yes = Foo | BarTest
data Yes = Bar | Test_Bar -- data Yes = Bar | TestBar
data No = a :::: b
data Yes = Foo {bar_cap :: Int} -- data Yes = Foo{barCap :: Int}
data No = FOO | BarBAR | BarBBar
yes_foo = yes_foo + yes_foo -- yesFoo = ...
no = 1 where yes_foo = 2
a -== b = 1
myTest = 1; my_test = 1
semiring'laws = 1 -- semiringLaws = ...
data Yes = FOO_A | Foo_B -- data Yes = FOO_A | FooB
case_foo = 1
cast_foo = 1 -- castFoo = ...
runMutator# = 1
</TEST>
-}


module Hint.Naming(namingHint) where

import Hint.Type
import Data.List
import Data.Char
import Data.Maybe
import Util
import qualified Data.Set as Set


namingHint :: DeclHint
namingHint _ modu = naming $ Set.fromList [x | Ident _ x <- universeS modu]

naming :: Set.Set String -> Decl_ -> [Idea]
naming seen x = [warn "Use camelCase" x2 (replaceNames res x2) | notNull res]
    where res = [(n,y) | n <- nub $ getNames x, Just y <- [suggestName n], not $ y `Set.member` seen]
          x2 = shorten x


shorten :: Decl_ -> Decl_
shorten x = case x of
    FunBind sl (Match a b c d _:_) -> FunBind sl [f (Match a b c) d]
    PatBind a b c d _ -> f (PatBind a b c) d
    x -> x
    where
        dots = Var an $ UnQual an $ Ident an "..." -- Must be an Ident, not a Symbol
        f cont (UnGuardedRhs _ _) = cont (UnGuardedRhs an dots) Nothing
        f cont (GuardedRhss _ _) = cont (GuardedRhss an [GuardedRhs an [Qualifier an dots] dots]) Nothing


getNames :: Decl_ -> [String]
getNames x = case x of
    FunBind{} -> name
    PatBind{} -> name
    TypeDecl{} -> name
    DataDecl _ _ _ _ cons _ -> name ++ [fromNamed x | QualConDecl _ _ _ x <- cons, x <- f x]
    GDataDecl _ _ _ _ _ cons _ -> name ++ [fromNamed x | GadtDecl _ x _ <- cons]
    TypeFamDecl{} -> name
    DataFamDecl{} -> name
    ClassDecl{} -> name
    _ -> []
    where
        name = [fromNamed x]

        f (ConDecl _ x _) = [x]
        f (InfixConDecl _ _ x _) = [x]
        f (RecDecl _ x ys) = x : concat [y | FieldDecl _ y _ <- ys]


suggestName :: String -> Maybe String
suggestName x = listToMaybe [f x | not $ isSym x || good || not (any isLower x) || "prop_" `isPrefixOf` x || "case_" `isPrefixOf` x]
    where
        good = all isAlphaNum $ drp '_' $ drp '#' $ drp '\'' $ reverse $ drp '_' x
        drp x = dropWhile (== x)

        f xs = us ++ g ys
            where (us,ys) = span (== '_') xs

        g x | x `elem` ["_","'","_'"] = x
        g (a:x:xs) | a `elem` "_'" && isAlphaNum x = toUpper x : g xs
        g (x:xs) | isAlphaNum x = x : g xs
                 | otherwise = g xs
        g [] = []


replaceNames :: Biplate a (Name S) => [(String,String)] -> a -> a
replaceNames rep = descendBi f
    where f (Ident _ x) = Ident an $ fromMaybe x $ lookup x rep
          f x = x


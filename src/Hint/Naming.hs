{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Suggest the use of camelCase

    Only permit:
    _?[A-Za-z]*_?'?

    Apply this to things that would get exported by default only

<TEST>
data Yes = Foo | Bar'Test
data Yes = Bar | Test_Bar
data Yes = Foo {bar_cap :: Int}
data No = FOO | BarBAR | BarBBar
yes_foo = yes_foo + yes_foo where res = "yesFoo = yesFoo + yesFoo"
no = 1 where yes_foo = 2
</TEST>
-}


module Hint.Naming where

import HSE.All
import Type
import Data.List
import Data.Char
import Data.Maybe


namingHint :: Hint
namingHint x = [warn "Use camelCase" (declSrcLoc x) x (replaceNames res x) | not $ null res]
    where res = [(n,y) | n <- nub $ getNames x, Just y <- [suggestName n]]


getNames :: Decl -> [String]
getNames x = case x of
    FunBind{} -> name
    PatBind{} -> name
    TypeDecl{} -> names
    DataDecl{} -> names
    GDataDecl{} -> names
    TypeFamDecl{} -> names
    DataFamDecl{} -> names
    ClassDecl{} -> names
    _ -> []
    where
        name = [fromNamed x]
        names = map fromNamed (universeBi x :: [Name])


suggestName :: String -> Maybe String
suggestName x = listToMaybe [f x | not good]
    where
        good = all isAlphaNum $ drp '_' $ drp '\'' $ reverse $ drp '_' x
        drp x ys = if [x] `isPrefixOf` ys then tail ys else ys

        f ('_':xs) = '_' : g xs
        f xs = g xs

        g x | x `elem` ["_","'","_'"] = x
        g ('_':x:xs) | isAlphaNum x = toUpper x : g xs
        g (x:xs) | isAlphaNum x = x : g xs
                 | otherwise = g xs
        g [] = []


replaceNames :: Data a => [(String,String)] -> a -> a
replaceNames rep = descendBi f
    where f x = fromMaybe x $ lookup x rep

{-# LANGUAGE ViewPatterns, PatternGuards #-}

{-
    Suggest the use of camelCase

    Only permit:
    _*[A-Za-z]*_?'?

    Apply this to things that would get exported by default only
    Also disallow prop_ as it's a standard QuickCheck idiom

<TEST>
data Yes = Foo | Bar'Test
data Yes = Bar | Test_Bar
data No = a :::: b
data Yes = Foo {bar_cap :: Int}
data No = FOO | BarBAR | BarBBar
yes_foo = yes_foo + yes_foo where res = "yesFoo = ..."
no = 1 where yes_foo = 2
a -== b = 1
</TEST>
-}


module Hint.Naming where

import HSE.All
import Type
import Data.List
import Data.Char
import Data.Maybe


namingHint :: Hint
namingHint _ x = [warn "Use camelCase" (declSrcLoc x) x2 (replaceNames res x2) | not $ null res]
    where res = [(n,y) | n <- nub $ getNames x, Just y <- [suggestName n]]
          x2 = shorten x


shorten :: Decl -> Decl
shorten x = case x of
    FunBind (Match a b c d e _:_) -> FunBind [f (Match a b c d) e]
    PatBind a b c d _ -> f (PatBind a b c) d
    x -> x
    where
        dots = Var $ UnQual $ Ident "..."
        f cont (UnGuardedRhs _) = cont (UnGuardedRhs dots) (BDecls [])
        f cont (GuardedRhss _) = cont (GuardedRhss [GuardedRhs nullSrcLoc [Qualifier dots] dots]) (BDecls [])


getNames :: Decl -> [String]
getNames x = case x of
    FunBind{} -> name
    PatBind{} -> name
    TypeDecl{} -> name
    DataDecl _ _ _ _ _ cons _ -> name ++ [fromNamed x | QualConDecl _ _ _ x <- cons, x <- f x]
    GDataDecl _ _ _ _ _ _ cons _ -> name ++ [fromNamed x | GadtDecl _ x _ <- cons]
    TypeFamDecl{} -> name
    DataFamDecl{} -> name
    ClassDecl{} -> name
    _ -> []
    where
        name = [fromNamed x]

        f (ConDecl x _) = [x]
        f (InfixConDecl _ x _) = [x]
        f (RecDecl x ys) = x : concatMap fst ys


suggestName :: String -> Maybe String
suggestName x = listToMaybe [f x | not $ isSym x || good || "prop_" `isPrefixOf` x]
    where
        good = all isAlphaNum $ drp '_' $ drp '\'' $ reverse $ dropWhile (== '_') x
        drp x ys = if [x] `isPrefixOf` ys then tail ys else ys

        f xs = us ++ g ys
            where (us,ys) = span (== '_') xs

        g x | x `elem` ["_","'","_'"] = x
        g ('_':x:xs) | isAlphaNum x = toUpper x : g xs
        g (x:xs) | isAlphaNum x = x : g xs
                 | otherwise = g xs
        g [] = []


replaceNames :: Data a => [(String,String)] -> a -> a
replaceNames rep = descendBi f
    where f x = fromMaybe x $ lookup x rep

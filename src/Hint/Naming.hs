{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-
    Suggest the use of camelCase

    Only permit:
    _*[A-Za-z]*_*#*'*

    Apply this to things that would get exported by default only
    Also allow prop_ as it's a standard QuickCheck idiom
    Also allow case_ as it's a standard test-framework-th idiom
    Also allow test_ as it's a standard tasty-th idiom
    Also allow numbers separated by _
    Also don't suggest anything mentioned elsewhere in the module

<TEST>
data Yes = Foo | Bar'Test -- data Yes = Foo | BarTest
data Yes = Bar | Test_Bar -- data Yes = Bar | TestBar
data No = a :::: b
data Yes = Foo {bar_cap :: Int}
data No = FOO | BarBAR | BarBBar
yes_foo = yes_foo + yes_foo -- yesFoo = ...
no = 1 where yes_foo = 2
a -== b = 1
myTest = 1; my_test = 1
semiring'laws = 1 -- semiringLaws = ...
data Yes = FOO_A | Foo_B -- data Yes = FOO_A | FooB
case_foo = 1
test_foo = 1
cast_foo = 1 -- castFoo = ...
replicateM_ = 1
_foo__ = 1
section_1_1 = 1
runMutator# = 1
</TEST>
-}


module Hint.Naming(namingHint) where

import Hint.Type
import Data.List.Extra
import Data.Data
import Data.Char
import Data.Maybe
import Refact.Types hiding (RType(Match))
import qualified Data.Set as Set

import qualified GHC.Util as Hs
import qualified "ghc-lib-parser" BasicTypes as Hs
import qualified "ghc-lib-parser" FastString as Hs
import qualified "ghc-lib-parser" HsDecls as Hs
import qualified "ghc-lib-parser" HsExtension as Hs
import qualified "ghc-lib-parser" HsSyn as Hs
import qualified "ghc-lib-parser" OccName as Hs
import qualified "ghc-lib-parser" RdrName as Hs
import qualified "ghc-lib-parser" SrcLoc as Hs

namingHint :: DeclHint'
namingHint _ modu = namingNew $ Set.fromList $ concatMap getNamesNew $ Hs.hsmodDecls $ Hs.unLoc (ghcModule modu)

namingNew :: Set.Set String -> Hs.LHsDecl Hs.GhcPs -> [Idea]
namingNew seen originalDecl =
    [ suggest' "Use camelCase"
               (shortenNew originalDecl)
               (shortenNew replacedDecl)
               [Replace Bind (toSrcSpan' originalDecl) [] (Hs.unsafePrettyPrint replacedDecl)]
    | not $ null suggestedNames
    ]
    where
        suggestedNames =
            [ (originalName, suggestedName)
            | originalName <- nubOrd $ getNamesNew originalDecl
            , Just suggestedName <- [suggestName originalName]
            , not $ suggestedName `Set.member` seen
            ]
        replacedDecl = replaceNamesNew suggestedNames originalDecl

naming :: Set.Set String -> Decl_ -> [Idea]
naming seen x = [suggest "Use camelCase" x' x2' [Replace Bind (toSS x) [] (prettyPrint x2)] | not $ null res]
    where res = [(n,y) | n <- nubOrd $ getNames x, Just y <- [suggestName n], not $ y `Set.member` seen]
          x2 = replaceNames res x
          x' = shorten x
          x2' = shorten x2

-- TODO: shorten pattern names
shortenNew :: Hs.LHsDecl Hs.GhcPs -> Hs.LHsDecl Hs.GhcPs
shortenNew (Hs.L locDecl (Hs.ValD ttg0 bind@(Hs.FunBind _ _ matchGroup@(Hs.MG _ (Hs.L locMatches matches) _) _ _))) =
    Hs.L locDecl (Hs.ValD ttg0 bind {Hs.fun_matches = matchGroup {Hs.mg_alts = Hs.L locMatches $ map shortenMatch matches}})
shortenNew x = x

shortenMatch :: Hs.LMatch Hs.GhcPs (Hs.LHsExpr Hs.GhcPs) -> Hs.LMatch Hs.GhcPs (Hs.LHsExpr Hs.GhcPs)
shortenMatch (Hs.L locMatch match@(Hs.Match _ _ _ grhss@(Hs.GRHSs _ rhss _))) =
    Hs.L locMatch match {Hs.m_grhss = grhss {Hs.grhssGRHSs = map shortenLGRHS rhss}}

shortenLGRHS :: Hs.LGRHS Hs.GhcPs (Hs.LHsExpr Hs.GhcPs) -> Hs.LGRHS Hs.GhcPs (Hs.LHsExpr Hs.GhcPs)
shortenLGRHS (Hs.L locGRHS (Hs.GRHS ttg0 guards (Hs.L locExpr _))) =
    Hs.L locGRHS (Hs.GRHS ttg0 guards (Hs.L locExpr dots))
    where
        dots :: Hs.HsExpr Hs.GhcPs
        dots = Hs.HsLit Hs.NoExt (Hs.HsString (Hs.SourceText "...") (Hs.mkFastString "..."))

shorten :: Decl_ -> Decl_
shorten x = case x of
    FunBind sl (Match a b c d _:_) -> FunBind sl [f (Match a b c) d]
    PatBind a b c _ -> f (PatBind a b) c
    x -> x
    where
        dots = Var an ellipses
        f cont (UnGuardedRhs _ _) = cont (UnGuardedRhs an dots) Nothing
        f cont (GuardedRhss _ _) = cont (GuardedRhss an [GuardedRhs an [Qualifier an dots] dots]) Nothing

getNamesNew :: Hs.LHsDecl Hs.GhcPs -> [String]
getNamesNew l@(Hs.L _ decl) = Hs.declName decl : getConstructorNamesNew decl

getConstructorNamesNew :: Hs.HsDecl Hs.GhcPs -> [String]
getConstructorNamesNew (Hs.TyClD _ (Hs.DataDecl _ _ _ _ (Hs.HsDataDefn _ _ _ _ _ cons _))) =
    concatMap (map Hs.unsafePrettyPrint . Hs.getConNames . Hs.unLoc) cons
getConstructorNamesNew _ = []

getNames :: Decl_ -> [String]
getNames x = case x of
    FunBind{} -> name
    PatBind{} -> name
    TypeDecl{} -> name
    DataDecl _ _ _ _ cons _ -> name ++ [fromNamed x | QualConDecl _ _ _ x <- cons, x <- f x]
    GDataDecl _ _ _ _ _ cons _ -> name ++ [fromNamed x | GadtDecl _ x _ _ _ _ <- cons]
    TypeFamDecl{} -> name
    DataFamDecl{} -> name
    ClassDecl{} -> name
    _ -> []
    where
        name = [fromNamed x]

        f (ConDecl _ x _) = [x]
        f (InfixConDecl _ _ x _) = [x]
        f (RecDecl _ x _) = [x]


suggestName :: String -> Maybe String
suggestName x
    | isSym x || good || not (any isLower x) || any isDigit x ||
        any (`isPrefixOf` x) ["prop_","case_","unit_","test_","spec_","scprop_","hprop_"] = Nothing
    | otherwise = Just $ f x
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

replaceNamesNew :: Data a => [(String, String)] -> a -> a
replaceNamesNew rep = transformBi replace
    where
        replace :: Hs.RdrName -> Hs.RdrName
        replace (Hs.Unqual (Hs.unsafePrettyPrint -> name)) = Hs.Unqual $ Hs.mkOccName Hs.srcDataName $ fromMaybe name $ lookup name rep
        replace x = x

replaceNames :: Data a => [(String,String)] -> a -> a
replaceNames rep = transformBi f
    where f (Ident _ x) = Ident an $ fromMaybe x $ lookup x rep
          f x = x

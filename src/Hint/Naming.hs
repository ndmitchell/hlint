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
yes_fooPattern Nothing = 0 -- yesFooPattern Nothing = ...
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
namingHint _ modu = naming $ Set.fromList $ concatMap getNames $ Hs.hsmodDecls $ Hs.unLoc (ghcModule modu)

naming :: Set.Set String -> Hs.LHsDecl Hs.GhcPs -> [Idea]
naming seen originalDecl =
    [ suggest' "Use camelCase"
               (shorten originalDecl)
               (shorten replacedDecl)
               [Replace Bind (toSrcSpan' originalDecl) [] (Hs.unsafePrettyPrint replacedDecl)]
    | not $ null suggestedNames
    ]
    where
        suggestedNames =
            [ (originalName, suggestedName)
            | originalName <- nubOrd $ getNames originalDecl
            , Just suggestedName <- [suggestName originalName]
            , not $ suggestedName `Set.member` seen
            ]
        replacedDecl = replaceNames suggestedNames originalDecl

shorten :: Hs.LHsDecl Hs.GhcPs -> Hs.LHsDecl Hs.GhcPs
shorten (Hs.L locDecl (Hs.ValD ttg0 bind@(Hs.FunBind _ _ matchGroup@(Hs.MG _ (Hs.L locMatches matches) _) _ _))) =
    Hs.L locDecl (Hs.ValD ttg0 bind {Hs.fun_matches = matchGroup {Hs.mg_alts = Hs.L locMatches $ map shortenMatch matches}})
shorten (Hs.L locDecl (Hs.ValD ttg0 bind@(Hs.PatBind _ _ grhss@(Hs.GRHSs _ rhss _) _))) =
    Hs.L locDecl (Hs.ValD ttg0 bind {Hs.pat_rhs = grhss {Hs.grhssGRHSs = map shortenLGRHS rhss}})
shorten x = x

shortenMatch :: Hs.LMatch Hs.GhcPs (Hs.LHsExpr Hs.GhcPs) -> Hs.LMatch Hs.GhcPs (Hs.LHsExpr Hs.GhcPs)
shortenMatch (Hs.L locMatch match@(Hs.Match _ _ _ grhss@(Hs.GRHSs _ rhss _))) =
    Hs.L locMatch match {Hs.m_grhss = grhss {Hs.grhssGRHSs = map shortenLGRHS rhss}}

shortenLGRHS :: Hs.LGRHS Hs.GhcPs (Hs.LHsExpr Hs.GhcPs) -> Hs.LGRHS Hs.GhcPs (Hs.LHsExpr Hs.GhcPs)
shortenLGRHS (Hs.L locGRHS (Hs.GRHS ttg0 guards (Hs.L locExpr _))) =
    Hs.L locGRHS (Hs.GRHS ttg0 guards (Hs.L locExpr dots))
    where
        dots :: Hs.HsExpr Hs.GhcPs
        dots = Hs.HsLit Hs.NoExt (Hs.HsString (Hs.SourceText "...") (Hs.mkFastString "..."))

getNames :: Hs.LHsDecl Hs.GhcPs -> [String]
getNames l@(Hs.L _ decl) = Hs.declName decl : getConstructorNames decl

getConstructorNames :: Hs.HsDecl Hs.GhcPs -> [String]
getConstructorNames (Hs.TyClD _ (Hs.DataDecl _ _ _ _ (Hs.HsDataDefn _ _ _ _ _ cons _))) =
    concatMap (map Hs.unsafePrettyPrint . Hs.getConNames . Hs.unLoc) cons
getConstructorNames _ = []

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

replaceNames :: Data a => [(String, String)] -> a -> a
replaceNames rep = transformBi replace
    where
        replace :: Hs.RdrName -> Hs.RdrName
        replace (Hs.Unqual (Hs.unsafePrettyPrint -> name)) = Hs.Unqual $ Hs.mkOccName Hs.srcDataName $ fromMaybe name $ lookup name rep
        replace x = x

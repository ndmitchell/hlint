{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
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
    Don't suggest for FFI, since they match their C names

<TEST>
data Yes = Foo | Bar'Test
data Yes = Bar | Test_Bar -- data Yes = Bar | TestBar
data No = a :::: b
data Yes = Foo {bar_cap :: Int}
data No = FOO | BarBAR | BarBBar
yes_foo = yes_foo + yes_foo -- yesFoo = ...
yes_fooPattern Nothing = 0 -- yesFooPattern Nothing = ...
no = 1 where yes_foo = 2
a -== b = 1
myTest = 1; my_test = 1
semiring'laws = 1
data Yes = FOO_A | Foo_B -- data Yes = FOO_A | FooB
case_foo = 1
test_foo = 1
cast_foo = 1 -- castFoo = ...
replicateM_ = 1
_foo__ = 1
section_1_1 = 1
runMutator# = 1
foreign import ccall hexml_node_child :: IO ()
</TEST>
-}


module Hint.Naming(namingHint) where

import Hint.Type (Idea,DeclHint,suggest,ghcModule)
import Data.Generics.Uniplate.DataOnly
import Data.List.Extra (nubOrd, isPrefixOf)
import Data.Data
import Data.Char
import Data.Maybe
import qualified Data.Set as Set

import GHC.Types.Basic
import GHC.Types.SourceText
import GHC.Data.FastString
import GHC.Hs.Decls
import GHC.Hs.Extension
import GHC.Hs
import GHC.Types.Name.Occurrence
import GHC.Types.SrcLoc

import Language.Haskell.GhclibParserEx.GHC.Hs.Decls
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import GHC.Util

namingHint :: DeclHint
namingHint _ modu = naming $ Set.fromList $ concatMap getNames $ hsmodDecls $ unLoc (ghcModule modu)

naming :: Set.Set String -> LHsDecl GhcPs -> [Idea]
naming seen originalDecl =
    [ suggest "Use camelCase"
               (reLoc (shorten originalDecl))
               (reLoc (shorten replacedDecl))
               [ -- https://github.com/mpickering/apply-refact/issues/39
               ]
    | not $ null suggestedNames
    ]
    where
        suggestedNames =
            [ (originalName, suggestedName)
            | not $ isForD originalDecl
            , originalName <- nubOrd $ getNames originalDecl
            , Just suggestedName <- [suggestName originalName]
            , not $ suggestedName `Set.member` seen
            ]
        replacedDecl = replaceNames suggestedNames originalDecl

shorten :: LHsDecl GhcPs -> LHsDecl GhcPs
shorten (L locDecl (ValD ttg0 bind@(FunBind _ _ matchGroup@(MG _ (L locMatches matches) FromSource) _))) =
    L locDecl (ValD ttg0 bind {fun_matches = matchGroup {mg_alts = L locMatches $ map shortenMatch matches}})
shorten (L locDecl (ValD ttg0 bind@(PatBind _ _ grhss@(GRHSs _ rhss _) _))) =
    L locDecl (ValD ttg0 bind {pat_rhs = grhss {grhssGRHSs = map shortenLGRHS rhss}})
shorten x = x

shortenMatch :: LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
shortenMatch (L locMatch match@(Match _ _ _ grhss@(GRHSs _ rhss _))) =
    L locMatch match {m_grhss = grhss {grhssGRHSs = map shortenLGRHS rhss}}

shortenLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> LGRHS GhcPs (LHsExpr GhcPs)
shortenLGRHS (L locGRHS (GRHS ttg0 guards (L locExpr _))) =
    L locGRHS (GRHS ttg0 guards (L locExpr dots))
    where
        dots :: HsExpr GhcPs
        dots = HsLit EpAnnNotUsed (HsString (SourceText "...") (mkFastString "..."))

getNames :: LHsDecl GhcPs -> [String]
getNames decl = maybeToList (declName decl) ++ getConstructorNames (unLoc decl)

getConstructorNames :: HsDecl GhcPs -> [String]
getConstructorNames (TyClD _ (DataDecl _ _ _ _ (HsDataDefn _ _ _ _ _ cons _))) =
    concatMap (map unsafePrettyPrint . getConNames' . unLoc) cons
    where
      getConNames' ConDeclH98  {con_name  = name}  = [name]
      getConNames' ConDeclGADT {con_names = names} = names
      getConNames' XConDecl{} = []

getConstructorNames _ = []

isSym :: String -> Bool
isSym (x:_) = not $ isAlpha x || x `elem` "_'"
isSym _ = False

suggestName :: String -> Maybe String
suggestName original
    | isSym original || good || not (any isLower original) || any isDigit original ||
        any (`isPrefixOf` original) ["prop_","case_","unit_","test_","spec_","scprop_","hprop_","tasty_"] = Nothing
    | otherwise = Just $ f original
    where
        good = all isAlphaNum $ drp '_' $ drp '#' $ reverse $ filter (/= '\'') $ drp '_' original
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
        replace :: OccName -> OccName
        replace (unsafePrettyPrint -> name) = mkOccName srcDataName $ fromMaybe name $ lookup name rep

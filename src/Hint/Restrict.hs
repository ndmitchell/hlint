{-# LANGUAGE RecordWildCards #-}

module Hint.Restrict(restrictHint) where

{-
-- These tests rely on the .hlint.yaml file in the root
<TEST>
foo = unsafePerformIO --
module Util where otherFunc = unsafePerformIO $ print 1 --
module Util where exitMessageImpure = unsafePerformIO $ print 1
foo = unsafePerformOI
</TEST>
-}

import qualified Data.Map as Map
import Config.Type
import Hint.Type
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Prelude


-- FIXME: The settings should be partially applied, but that's hard to orchestrate right now
restrictHint :: [Setting] -> ModuHint
restrictHint settings scope m =
        checkPragmas modu (modulePragmas m) restrict ++
        maybe [] (checkImports modu $ moduleImports m) (Map.lookup RestrictModule restrict) ++
        maybe [] (checkFunctions modu $ moduleDecls m) (Map.lookup RestrictFunction restrict)
    where
        modu = moduleName m
        restrict = restrictions settings

---------------------------------------------------------------------
-- UTILITIES

data RestrictItem = RestrictItem
    {riAs :: [String]
    ,riWithin :: [(String, String)]
    }
instance Monoid RestrictItem where
    mempty = RestrictItem [] []
    mappend (RestrictItem x1 x2) (RestrictItem y1 y2) = RestrictItem (x1<>y1) (x2<>y2)

restrictions :: [Setting] -> Map.Map RestrictType (Bool, Map.Map String RestrictItem)
restrictions settings = Map.map f $ Map.fromListWith (++) [(restrictType x, [x]) | SettingRestrict x <- settings]
    where
        f rs = (all restrictDefault rs
               ,Map.fromListWith (<>) [(s, RestrictItem restrictAs restrictWithin) | Restrict{..} <- rs, s <- restrictName])

ideaMayBreak w = w{ideaNote=[Note "may break the code"]}
ideaNoTo w = w{ideaTo=Nothing}

within :: String -> String -> RestrictItem -> Bool
within modu func RestrictItem{..} = any (\(a,b) -> (a == modu || a == "") && (b == func || b == "")) riWithin

---------------------------------------------------------------------
-- CHECKS

checkPragmas :: String -> [ModulePragma S] -> Map.Map RestrictType (Bool, Map.Map String RestrictItem) -> [Idea]
checkPragmas modu xs mps = f RestrictFlag "flags" onFlags ++ f RestrictExtension "extensions" onExtensions
    where
        f tag name sel =
            [ (if null good then ideaNoTo else id) $ ideaMayBreak $ warn ("Avoid restricted " ++ name) o (regen good) []
            | Just mp <- [Map.lookup tag mps]
            , o <- xs, Just (xs, regen) <- [sel o]
            , let (good, bad) = partition (isGood mp) xs, not $ null bad]

        onFlags (OptionsPragma s t x) = Just (words x, OptionsPragma s t . unwords)
        onFlags _ = Nothing

        onExtensions (LanguagePragma s xs) = Just (map fromNamed xs, LanguagePragma (s :: S) . map toNamed)
        onExtensions _ = Nothing

        isGood (def, mp) x = maybe def (within modu "") $ Map.lookup x mp


checkImports :: String -> [ImportDecl S] -> (Bool, Map.Map String RestrictItem) -> [Idea]
checkImports modu imp (def, mp) =
    [ ideaMayBreak $ if not allowImport
      then ideaNoTo $ warn "Avoid restricted module" i i []
      else warn "Avoid restricted qualification" i i{importAs=ModuleName an <$> listToMaybe riAs} []
    | i@ImportDecl{..} <- imp
    , let ri@RestrictItem{..} = Map.findWithDefault (RestrictItem [] [("","") | def]) (fromModuleName importModule) mp
    , let allowImport = within modu "" ri
    , let allowQual = maybe True (\x -> null riAs || fromModuleName x `elem` riAs) importAs
    , not allowImport || not allowQual
    ]


checkFunctions :: String -> [Decl_] -> (Bool, Map.Map String RestrictItem) -> [Idea]
checkFunctions modu decls (def, mp) =
    [ ideaMayBreak $ ideaNoTo $ warn "Avoid restricted function" x x []
    | d <- decls
    , let dname = fromNamed d
    , x@Var{} <- universeBi d
    , not $ maybe def (within modu dname) $ Map.lookup (fromNamed x) mp
    ]

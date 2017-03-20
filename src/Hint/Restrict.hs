{-# LANGUAGE RecordWildCards #-}

module Hint.Restrict(restrictHint) where

import qualified Data.Map as Map
import Config.Type
import Hint.Type
import Data.List
import Data.Monoid
import Prelude


-- FIXME: The settings should be partially applied, but that's hard to orchestrate right now
restrictHint :: [Setting] -> ModuHint
restrictHint settings scope m =
        checkPragmas modu (modulePragmas m) restrict
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

        isGood (def, mp) x = case Map.lookup x mp of
            Nothing -> def
            Just RestrictItem{..} -> any (\(a,b) -> (a == modu || a == "") && b == "") riWithin

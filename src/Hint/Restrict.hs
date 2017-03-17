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
        maybe [] (checkFlags modu pragmas) (Map.lookup RestrictFlag restrict)
    where
        modu = moduleName m
        pragmas = modulePragmas m
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


---------------------------------------------------------------------
-- CHECKS

checkFlags :: String -> [ModulePragma S] -> (Bool, Map.Map String RestrictItem) -> [Idea]
checkFlags modu xs mp =
    [ warn "Avoid restricted flags" o (OptionsPragma s t $ unwords good) []
    | o@(OptionsPragma s t x) <- xs, let (good, bad) = partition isGood $ words x, not $ null bad]
    where
        isGood x = case Map.lookup x $ snd mp of
            Nothing -> fst mp
            Just RestrictItem{..} | null riWithin -> True
                                  | otherwise -> (modu,"") `elem` riWithin

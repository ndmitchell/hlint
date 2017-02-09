{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Config.Yaml(readFileConfigYaml) where

import Config.Type
import Data.Yaml
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as Map
import HSE.All hiding (Rule, String)


readFileConfigYaml :: FilePath -> Maybe String -> IO [Setting]
readFileConfigYaml file contents = do
    val <- case contents of
        Nothing -> decodeFile file
        Just src -> return $ decode $ BS.pack src
    return $ asSettings $ asConfig $ fromJust val


---------------------------------------------------------------------
-- YAML DATA TYPE

data Package = Package
    {packageName :: String
    ,packageModules :: [ImportDecl S]
    } deriving Show

data Group = Group
    {groupName :: String
    ,groupEnabled :: Bool
    ,groupImports :: [Either String (ImportDecl S)] -- Left for package imports
    ,groupRules :: [Rule]
    } deriving Show

data Rule = Rule
    {ruleSeverity :: Severity
    ,ruleLhs :: Exp_
    ,ruleRhs :: Exp_
    ,ruleNote :: String
    } deriving Show


---------------------------------------------------------------------
-- YAML TO DATA TYPE

asConfig :: Value -> [Either Package Group]
asConfig (Object x)
    | "package" `Map.member` x = [Left $ asPackage x]
    | "group" `Map.member` x = [Right $ asGroup x]
asConfig (Array x) = concatMap asConfig x

asPackage :: Map.HashMap T.Text Value -> Package
asPackage x
    | Just (String name) <- Map.lookup "package" x
    , Just (Array modules) <- Map.lookup "modules" x
    , Map.size x == 2
    = Package (T.unpack name) $ map asModule $ V.toList modules
    where
        asModule (String x) = fromParseResult $ parseImportDecl $ T.unpack x

asGroup :: Map.HashMap T.Text Value -> Group
asGroup x
    | Just (String name) <- Map.lookup "group" x
    , Just (Bool enabled) <- Map.lookup "enabled" x
    , Just (Array imports) <- Map.lookup "imports" x
    , Just (Array rules) <- Map.lookup "rules" x
    , Map.size x == 4
    = Group (T.unpack name) enabled (map asImport $ V.toList imports) (map asRule $ V.toList rules)
    where
        asImport (String x)
            | Just x <- T.stripPrefix "package " x = Left $ T.unpack x
            | otherwise = Right $ fromParseResult $ parseImportDecl $ T.unpack x

asRule :: Value -> Rule
asRule (Object (Map.toList -> [(severity, Object x)]))
    | Just sev <- getSeverity $ T.unpack severity
    , Just lhs <- Map.lookup "lhs" x
    , Just rhs <- Map.lookup "rhs" x
    , String note <- Map.lookupDefault (String "") "note" x
    = Rule sev (asExp lhs) (asExp rhs) (T.unpack note)
    where
        asExp (String x) = fromParseResult $ parseExp $ T.unpack x
asRule x = error $ show x


---------------------------------------------------------------------
-- SETTINGS

asSettings :: [Either Package Group] -> [Setting]
asSettings xs = concat $ map f groups
    where
        (packages, groups) = partitionEithers xs
        packageMap = Map.fromListWith (++) [(packageName, packageModules) | Package{..} <- packages]
        groupMap = Map.fromListWith (\old new -> new) [(groupName, groupEnabled) | Group{..} <- groups]

        f Group{..}
            | Map.lookup groupName groupMap == Just False = []
            | otherwise = [SettingMatchExp $ HintRule ruleSeverity "Rule name" scope ruleLhs ruleRhs Nothing []
                          | Rule{..} <- groupRules]
            where scope = asScope packageMap groupImports

asScope :: Map.HashMap String [ImportDecl S] -> [Either String (ImportDecl S)] -> Scope
asScope packages xs = scopeCreate $ Module an Nothing [] (concatMap f xs) []
    where
        f (Right x) = [x]
        f (Left x) | Just pkg <- Map.lookup x packages = pkg

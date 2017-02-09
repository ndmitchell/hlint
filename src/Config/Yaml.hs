{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Config.Yaml(readFileConfigYaml) where

import Config.Type
import Data.Yaml
import Data.Either
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Control.Exception
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as Map
import HSE.All hiding (Rule, String)
import Data.Functor
import Data.Monoid
import Prelude


readFileConfigYaml :: FilePath -> Maybe String -> IO [Setting]
readFileConfigYaml file contents = do
    val <- case contents of
        Nothing -> decodeFileEither file
        Just src -> return $ decodeEither' $ BS.pack src
    case val of
        Left e -> fail $ "Failed to read YAML configuration file " ++ file ++ "\n  " ++ displayException e
        Right v -> return $ asSettings v


---------------------------------------------------------------------
-- YAML DATA TYPE

newtype TopLevel = TopLevel [Either Package Group] deriving Monoid

data Package = Package
    {packageName :: String
    ,packageModules :: [ImportDecl S]
    } deriving Show

data Group = Group
    {groupName :: String
    ,groupEnabled :: Bool
    ,groupImports :: [Either String (ImportDecl S)] -- Left for package imports
    ,groupRules :: [HintRule] -- with scope set to mempty
    } deriving Show


---------------------------------------------------------------------
-- YAML TO DATA TYPE

failJSON :: Value -> String -> Parser a
failJSON x s = fail $ s ++ "\n" ++ BS.unpack (BS.take 200 $ encode x)

instance FromJSON TopLevel where
    parseJSON (Array xs) = mconcat <$> mapM parseJSON (V.toList xs)
    parseJSON (Object x)
        | "package" `Map.member` x = TopLevel . return . Left <$> parseJSON (Object x)
        | "group" `Map.member` x = TopLevel . return . Right <$> parseJSON (Object x)
    parseJSON x = failJSON x "Expected objects containing 'package' or 'group'"

instance FromJSON Package where
    parseJSON (Object x) = return $ asPackage x

instance FromJSON Group where
    parseJSON (Object x) = return $ asGroup x

parseSnippet :: (String -> ParseResult a) -> String -> a
parseSnippet parser x = case parser x of
    ParseOk v -> v
    ParseFailed loc s -> error $ "Failed to parse " ++ s ++ ", when parsing:\n" ++ x

unString (String x) = T.unpack x
unString x = error $ "Expected a String, but got " ++ show x

asPackage :: Map.HashMap T.Text Value -> Package
asPackage x
    | Just (String name) <- Map.lookup "package" x
    , Just (Array modules) <- Map.lookup "modules" x
    , Map.size x == 2
    = Package (T.unpack name) $ map asModule $ V.toList modules
    where
        asModule (String x) = parseSnippet parseImportDecl $ T.unpack x

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
            | otherwise = Right $ parseSnippet parseImportDecl $ T.unpack x

asRule :: Value -> HintRule
asRule (Object (Map.toList -> [(severity, Object x)]))
    | Just sev <- getSeverity $ T.unpack severity
    , Just lhs <- asExp <$> Map.lookup "lhs" x
    , Just rhs <- asExp <$> Map.lookup "rhs" x
    , note <- asNote . unString <$> Map.lookup "note" x
    , name <- maybe (guessName lhs rhs) unString $ Map.lookup "name" x
    , side <- asExp <$> Map.lookup "side" x
    = HintRule sev name mempty lhs rhs side (maybeToList note)
    where
        asExp = parseSnippet parseExp . unString
asRule x = error $ show x

guessName :: Exp_ -> Exp_ -> String
guessName lhs rhs
    | n:_ <- rs \\ ls = "Use " ++ n
    | n:_ <- ls \\ rs = "Redundant " ++ n
    | otherwise = defaultHintName
    where
        (ls, rs) = both f (lhs, rhs)
        f = filter (not . isUnifyVar) . map (\x -> fromNamed (x :: Name S)) . childrenS


asNote :: String -> Note
asNote "IncreasesLaziness" = IncreasesLaziness
asNote "DecreasesLaziness" = DecreasesLaziness
asNote (word1 -> ("RemovesError",x)) = RemovesError x
asNote (word1 -> ("ValidInstance",x)) = uncurry ValidInstance $ word1 x
asNote x = Note x


---------------------------------------------------------------------
-- SETTINGS

asSettings :: TopLevel -> [Setting]
asSettings (TopLevel xs) = concatMap f groups
    where
        (packages, groups) = partitionEithers xs
        packageMap = Map.fromListWith (++) [(packageName, packageModules) | Package{..} <- packages]
        groupMap = Map.fromListWith (\old new -> new) [(groupName, groupEnabled) | Group{..} <- groups]

        f Group{..}
            | Map.lookup groupName groupMap == Just False = []
            | otherwise = [SettingMatchExp r{hintRuleScope=scope} | r <- groupRules]
            where scope = asScope packageMap groupImports

asScope :: Map.HashMap String [ImportDecl S] -> [Either String (ImportDecl S)] -> Scope
asScope packages xs = scopeCreate $ Module an Nothing [] (concatMap f xs) []
    where
        f (Right x) = [x]
        f (Left x) | Just pkg <- Map.lookup x packages = pkg

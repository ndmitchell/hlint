{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, GeneralizedNewtypeDeriving, TupleSections #-}

module Config.Yaml(
    ConfigYaml,
    readFileConfigYaml,
    settingsFromConfigYaml
    ) where

import Config.Type
import Data.Yaml
import Data.Either
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Control.Monad.Extra
import Control.Exception.Extra
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as Map
import HSE.All hiding (Rule, String)
import Data.Functor
import Data.Semigroup
import Timing
import Util
import Prelude

import qualified Lexer as GHC
import qualified ErrUtils
import qualified Outputable
import qualified HsSyn
import GHC.Util (baseDynFlags, Scope',scopeCreate')
import GHC.Util.W

-- | Read a config file in YAML format. Takes a filename, and optionally the contents.
--   Fails if the YAML doesn't parse or isn't valid HLint YAML
readFileConfigYaml :: FilePath -> Maybe String -> IO ConfigYaml
readFileConfigYaml file contents = timedIO "Config" file $ do
    val <- case contents of
        Nothing -> decodeFileEither file
        Just src -> return $ decodeEither' $ BS.pack src
    case val of
        Left e -> fail $ "Failed to read YAML configuration file " ++ file ++ "\n  " ++ displayException e
        Right v -> return v


---------------------------------------------------------------------
-- YAML DATA TYPE

newtype ConfigYaml = ConfigYaml [ConfigItem] deriving (Semigroup,Monoid,Show)

data ConfigItem
    = ConfigPackage Package
    | ConfigGroup Group
    | ConfigSetting [Setting]
      deriving Show

data Package = Package
    {packageName :: String
    ,packageModules :: [ImportDecl S]
    ,packageGhcModules :: [W (HsSyn.LImportDecl HsSyn.GhcPs)]
    } deriving Show

data Group = Group
    {groupName :: String
    ,groupEnabled :: Bool
    ,groupImports :: [Either String (ImportDecl S)] -- Left for package imports
    ,groupGhcImports :: [Either String (W (HsSyn.LImportDecl HsSyn.GhcPs))]
    ,groupRules :: [Either HintRule Classify] -- HintRule has scope set to mempty
    } deriving Show


---------------------------------------------------------------------
-- YAML PARSING LIBRARY

data Val = Val
    Value -- the actual value I'm focused on
    [(String, Value)] -- the path of values I followed (for error messages)

newVal :: Value -> Val
newVal x = Val x [("root", x)]

getVal :: Val -> Value
getVal (Val x _) = x

addVal :: String -> Value -> Val -> Val
addVal key v (Val focus path) = Val v $ (key,v) : path

-- | Failed when parsing some value, give an informative error message.
parseFail :: Val -> String -> Parser a
parseFail (Val focus path) msg = fail $
    "Error when decoding YAML, " ++ msg ++ "\n" ++
    "Along path: " ++ unwords steps ++ "\n" ++
    "When at: " ++ fst (word1 $ show focus) ++ "\n" ++
    -- aim to show a smallish but relevant context
    dotDot (fromMaybe (encode focus) $ listToMaybe $ dropWhile (\x -> BS.length x > 250) $ map encode contexts)
    where
        (steps, contexts) = unzip $ reverse path
        dotDot x = let (a,b) = BS.splitAt 250 x in BS.unpack a ++ (if BS.null b then "" else "...")

parseArray :: Val -> Parser [Val]
parseArray v@(getVal -> Array xs) = concatMapM parseArray $ zipWith (\i x -> addVal (show i) x v) [0..] $ V.toList xs
parseArray v = return [v]

parseObject :: Val -> Parser (Map.HashMap T.Text Value)
parseObject (getVal -> Object x) = return x
parseObject v = parseFail v "Expected an Object"

parseObject1 :: Val -> Parser (String, Val)
parseObject1 v = do
    mp <- parseObject v
    case Map.keys mp of
        [T.unpack -> s] -> (s,) <$> parseField s v
        _ -> parseFail v $ "Expected exactly one key but got " ++ show (Map.size mp)

parseString :: Val -> Parser String
parseString (getVal -> String x) = return $ T.unpack x
parseString v = parseFail v "Expected a String"

parseInt :: Val -> Parser Int
parseInt (getVal -> s@Number{}) = parseJSON s
parseInt v = parseFail v "Expected an Int"

parseArrayString :: Val -> Parser [String]
parseArrayString = parseArray >=> mapM parseString

maybeParse :: (Val -> Parser a) -> Maybe Val -> Parser (Maybe a)
maybeParse parseValue Nothing = return Nothing
maybeParse parseValue (Just value) = Just <$> parseValue value

parseBool :: Val -> Parser Bool
parseBool (getVal -> Bool b) = return b
parseBool v = parseFail v "Expected a Bool"

parseField :: String -> Val -> Parser Val
parseField s v = do
    x <- parseFieldOpt s v
    case x of
        Nothing -> parseFail v $ "Expected a field named " ++ s
        Just v -> return v

parseFieldOpt :: String -> Val -> Parser (Maybe Val)
parseFieldOpt s v = do
    mp <- parseObject v
    case Map.lookup (T.pack s) mp of
        Nothing -> return Nothing
        Just x -> return $ Just $ addVal s x v

allowFields :: Val -> [String] -> Parser ()
allowFields v allow = do
    mp <- parseObject v
    let bad = map T.unpack (Map.keys mp) \\ allow
    when (bad /= []) $
        parseFail v $ "Not allowed keys: " ++ unwords bad

parseHSE :: (ParseMode -> String -> ParseResult v) -> Val -> Parser v
parseHSE parser v = do
    x <- parseString v
    case parser defaultParseMode{extensions=configExtensions} x of
        ParseOk x -> return x
        ParseFailed loc s ->
          parseFail v $ "Failed to parse " ++ s ++ ", when parsing:\n  " ++ x

parseGHC :: (ParseMode -> String -> GHC.ParseResult v) -> Val -> Parser v
parseGHC parser v = do
    x <- parseString v
    case parser defaultParseMode{extensions=configExtensions} x of
        GHC.POk _ x -> return x
        GHC.PFailed _ loc err ->
          let msg = Outputable.showSDoc baseDynFlags $
                ErrUtils.pprLocErrMsg (ErrUtils.mkPlainErrMsg baseDynFlags loc err)
          in parseFail v $ "Failed to parse " ++ msg ++ ", when parsing:\n " ++ x

---------------------------------------------------------------------
-- YAML TO DATA TYPE

instance FromJSON ConfigYaml where
    parseJSON Null = return mempty
    parseJSON x = parseConfigYaml $ newVal x

parseConfigYaml :: Val -> Parser ConfigYaml
parseConfigYaml v = do
    vs <- parseArray v
    fmap ConfigYaml $ forM vs $ \o -> do
        (s, v) <- parseObject1 o
        case s of
            "package" -> ConfigPackage <$> parsePackage v
            "group" -> ConfigGroup <$> parseGroup v
            "arguments" -> ConfigSetting . map SettingArgument <$> parseArrayString v
            "fixity" -> ConfigSetting <$> parseFixity v
            "smell" -> ConfigSetting <$> parseSmell v
            _ | isJust $ getSeverity s -> ConfigGroup . ruleToGroup <$> parseRule o
            _ | Just r <- getRestrictType s -> ConfigSetting . map SettingRestrict <$> (parseArray v >>= mapM (parseRestrict r))
            _ -> parseFail v "Expecting an object with a 'package' or 'group' key, a hint or a restriction"


parsePackage :: Val -> Parser Package
parsePackage v = do
    packageName <- parseField "name" v >>= parseString
    packageModules <- parseField "modules" v >>= parseArray >>= mapM (parseHSE parseImportDeclWithMode)
    packageGhcModules <- parseField "modules" v >>= parseArray >>= mapM (fmap wrap <$> parseGHC parseImportDeclGhcWithMode)
    allowFields v ["name","modules"]
    return Package{..}

parseFixity :: Val -> Parser [Setting]
parseFixity v = parseArray v >>= concatMapM (parseHSE parseDeclWithMode >=> f)
    where
        f x@InfixDecl{} = return $ map Infix $ getFixity x
        f _ = parseFail v "Expected fixity declaration"

parseSmell :: Val -> Parser [Setting]
parseSmell v = do
  smellName <- parseField "type" v >>= parseString
  smellType <- require v "Expected SmellType"  $ getSmellType smellName
  smellLimit <- parseField "limit" v >>= parseInt
  return [SettingSmell smellType smellLimit]
    where
      require :: Val -> String -> Maybe a -> Parser a
      require _ _ (Just a) = return a
      require val err Nothing = parseFail val err

parseGroup :: Val -> Parser Group
parseGroup v = do
    groupName <- parseField "name" v >>= parseString
    groupEnabled <- parseFieldOpt "enabled" v >>= maybe (return True) parseBool
    groupImports <- parseFieldOpt "imports" v >>= maybe (return []) (parseArray >=> mapM parseImport)
    groupGhcImports <- parseFieldOpt "imports" v >>= maybe (return []) (parseArray >=> mapM parseImportGHC)
    groupRules <- parseFieldOpt "rules" v >>= maybe (return []) parseArray >>= concatMapM parseRule
    allowFields v ["name","enabled","imports","rules"]
    return Group{..}
    where
        parseImport v = do
            x <- parseString v
            case word1 x of
                ("package", x) -> return $ Left x
                _ -> Right <$> parseHSE parseImportDeclWithMode v
        parseImportGHC v = do
            x <- parseString v
            case word1 x of
                 ("package", x) -> return $ Left x
                 _ -> Right . wrap <$> parseGHC parseImportDeclGhcWithMode v

ruleToGroup :: [Either HintRule Classify] -> Group
ruleToGroup = Group "" True [] []

parseRule :: Val -> Parser [Either HintRule Classify]
parseRule v = do
    (severity, v) <- parseSeverityKey v
    isRule <- isJust <$> parseFieldOpt "lhs" v
    if isRule then do
        hintRuleLHS <- parseField "lhs" v >>= parseHSE parseExpWithMode
        hintRuleRHS <- parseField "rhs" v >>= parseHSE parseExpWithMode
        hintRuleNotes <- parseFieldOpt "note" v >>= maybe (return []) (fmap (map asNote) . parseArrayString)
        hintRuleName <- parseFieldOpt "name" v >>= maybe (return $ guessName hintRuleLHS hintRuleRHS) parseString
        hintRuleSide <- parseFieldOpt "side" v >>= maybe (return Nothing) (fmap Just . parseHSE parseExpWithMode)

        hintRuleGhcLHS <- parseField "lhs" v >>= fmap wrap . parseGHC parseExpGhcWithMode
        hintRuleGhcRHS <- parseField "rhs" v >>= fmap wrap . parseGHC parseExpGhcWithMode
        hintRuleGhcSide <- parseFieldOpt "side" v >>= maybe (return Nothing) (fmap (Just . wrap) . parseGHC parseExpGhcWithMode)

        allowFields v ["lhs","rhs","note","name","side"]
        let hintRuleScope = mempty :: Scope
        let hintRuleGhcScope = wrap mempty :: W Scope'
        return [Left HintRule{hintRuleSeverity=severity, ..}]
     else do
        names <- parseFieldOpt "name" v >>= maybe (return []) parseArrayString
        within <- parseFieldOpt "within" v >>= maybe (return [("","")]) (parseArray >=> concatMapM parseWithin)
        return [Right $ Classify severity n a b | (a,b) <- within, n <- ["" | null names] ++ names]

parseRestrict :: RestrictType -> Val -> Parser Restrict
parseRestrict restrictType v = do
    def <- parseFieldOpt "default" v
    case def of
        Just def -> do
            b <- parseBool def
            allowFields v ["default"]
            return $ Restrict restrictType b [] [] [] Nothing
        Nothing -> do
            restrictName <- parseFieldOpt "name" v >>= maybe (return []) parseArrayString
            restrictWithin <- parseFieldOpt "within" v >>= maybe (return [("","")]) (parseArray >=> concatMapM parseWithin)
            restrictAs <- parseFieldOpt "as" v >>= maybe (return []) parseArrayString
            restrictMessage <- parseFieldOpt "message" v >>= maybeParse parseString
            allowFields v $ ["as" | restrictType == RestrictModule] ++ ["name","within", "message"]
            return Restrict{restrictDefault=True,..}

parseWithin :: Val -> Parser [(String, String)] -- (module, decl)
parseWithin v = do
    x <- parseHSE parseExpWithMode v
    case x of
        Var _ (UnQual _ name) -> return [("",fromNamed name)]
        Var _ (Qual _ (ModuleName _ mod) name) -> return [(mod, fromNamed name)]
        Con _ (UnQual _ name) -> return [(fromNamed name,""),("",fromNamed name)]
        Con _ (Qual _ (ModuleName _ mod) name) -> return [(mod ++ "." ++ fromNamed name,""),(mod,fromNamed name)]
        _ -> parseFail v "Bad classification rule"

parseSeverityKey :: Val -> Parser (Severity, Val)
parseSeverityKey v = do
    (s, v) <- parseObject1 v
    case getSeverity s of
        Just sev -> return (sev, v)
        _ -> parseFail v $ "Key should be a severity (e.g. warn/error/suggest) but got " ++ s


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
asNote (word1 -> ("RequiresExtension",x)) = RequiresExtension x
asNote x = Note x


---------------------------------------------------------------------
-- SETTINGS

settingsFromConfigYaml :: [ConfigYaml] -> [Setting]
settingsFromConfigYaml (mconcat -> ConfigYaml configs) = settings ++ concatMap f groups
    where
        packages = [x | ConfigPackage x <- configs]
        groups = [x | ConfigGroup x <- configs]
        settings = concat [x | ConfigSetting x <- configs]
        packageMap = Map.fromListWith (++) [(packageName, packageModules) | Package{..} <- packages]
        packageMap' = Map.fromListWith (++) [(packageName, fmap unwrap packageGhcModules) | Package{..} <- packages]
        groupMap = Map.fromListWith (\new old -> new) [(groupName, groupEnabled) | Group{..} <- groups]

        f Group{..}
            | Map.lookup groupName groupMap == Just False = []
            | otherwise = map (either (\r -> SettingMatchExp r{hintRuleScope=scope,hintRuleGhcScope=scope'}) SettingClassify) groupRules
            where
              scope = asScope packageMap groupImports
              scope'= asScope' packageMap' (map (fmap unwrap) groupGhcImports)

asScope :: Map.HashMap String [ImportDecl S] -> [Either String (ImportDecl S)] -> Scope
asScope packages xs = scopeCreate $ Module an Nothing [] (concatMap f xs) []
    where
        f (Right x) = [x]
        f (Left x) | Just pkg <- Map.lookup x packages = pkg
                   | otherwise = error $ "asScope failed to do lookup, " ++ x

asScope' :: Map.HashMap String [HsSyn.LImportDecl HsSyn.GhcPs] -> [Either String (HsSyn.LImportDecl HsSyn.GhcPs)] -> W Scope'
asScope' packages xs = W $ scopeCreate' (HsSyn.HsModule Nothing Nothing (concatMap f xs) [] Nothing Nothing)
    where
        f (Right x) = [x]
        f (Left x) | Just pkg <- Map.lookup x packages = pkg
                   | otherwise = error $ "asScope' failed to do lookup, " ++ x

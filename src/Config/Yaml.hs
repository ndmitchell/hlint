{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, GeneralizedNewtypeDeriving, TupleSections #-}

module Config.Yaml(
    ConfigYaml,
    ConfigYamlBuiltin (..),
    ConfigYamlUser (..),
    readFileConfigYaml,
    settingsFromConfigYaml,
    isBuiltinYaml,
    ) where

#if defined(MIN_VERSION_aeson)
#if MIN_VERSION_aeson(2,0,0)
#define AESON 2
#else
#define AESON 1
#endif
#else
#define AESON 2
#endif

import GHC.Driver.Ppr
import GHC.Parser.Errors.Ppr
import Config.Type
import Data.Either.Extra
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Control.Monad.Extra
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as Map
import Data.Generics.Uniplate.DataOnly
import GHC.All
import Fixity
import Extension
import GHC.Unit.Module
import Data.Functor
import Data.Monoid
import Data.Semigroup
import Timing
import Prelude

import GHC.Data.Bag
import GHC.Parser.Lexer
import GHC.Utils.Error hiding (Severity)
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence
import GHC.Util (baseDynFlags, Scope, scopeCreate)
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
import Data.Char
#if AESON == 2
import Data.Aeson.KeyMap (toHashMapText)
#endif

#ifdef HS_YAML

import Data.YAML (Pos)
import Data.YAML.Aeson (encode1Strict, decode1Strict)
import Data.Aeson hiding (encode)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BSS

decodeFileEither :: FilePath -> IO (Either (Pos, String) ConfigYaml)
decodeFileEither path = decode1Strict <$> BSS.readFile path

decodeEither' :: BSS.ByteString -> Either (Pos, String) ConfigYaml
decodeEither' = decode1Strict

displayException :: (Pos, String) -> String
displayException = show

encode :: Value -> BSS.ByteString
encode = encode1Strict

#else

import Data.Yaml
import Control.Exception.Extra

#endif

#if AESON == 1
toHashMapText :: a -> a
toHashMapText = id
#endif

-- | Read a config file in YAML format. Takes a filename, and optionally the contents.
--   Fails if the YAML doesn't parse or isn't valid HLint YAML
readFileConfigYaml :: FilePath -> Maybe String -> IO ConfigYaml
readFileConfigYaml file contents = timedIO "Config" file $ do
    val <- case contents of
        Nothing ->
            if isBuiltinYaml file
                then mapRight getConfigYamlBuiltin <$> decodeFileEither file
                else mapRight getConfigYamlUser <$> decodeFileEither file
        Just src -> pure $
            if isBuiltinYaml file
                then mapRight getConfigYamlBuiltin $ decodeEither' $ BS.pack src
                else mapRight getConfigYamlUser $ decodeEither' $ BS.pack src
    case val of
        Left e -> fail $ "Failed to read YAML configuration file " ++ file ++ "\n  " ++ displayException e
        Right v -> pure v

isBuiltinYaml :: FilePath -> Bool
isBuiltinYaml = (== "data/hlint.yaml")

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
    ,packageModules :: [HsExtendInstances (LImportDecl GhcPs)]
    } deriving Show

data Group = Group
    {groupName :: String
    ,groupEnabled :: Bool
    ,groupImports :: [Either String (HsExtendInstances (LImportDecl GhcPs))]
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
parseArray v@(getVal -> Array xs) = concatMapM parseArray $ zipWithFrom (\i x -> addVal (show i) x v) 0 $ V.toList xs
parseArray v = pure [v]

parseObject :: Val -> Parser (Map.HashMap T.Text Value)
parseObject (getVal -> Object x) = pure (toHashMapText x)
parseObject v = parseFail v "Expected an Object"

parseObject1 :: Val -> Parser (String, Val)
parseObject1 v = do
    mp <- parseObject v
    case Map.keys mp of
        [T.unpack -> s] -> (s,) <$> parseField s v
        _ -> parseFail v $ "Expected exactly one key but got " ++ show (Map.size mp)

parseString :: Val -> Parser String
parseString (getVal -> String x) = pure $ T.unpack x
parseString v = parseFail v "Expected a String"

parseInt :: Val -> Parser Int
parseInt (getVal -> s@Number{}) = parseJSON s
parseInt v = parseFail v "Expected an Int"

parseArrayString :: Val -> Parser [String]
parseArrayString = parseArray >=> mapM parseString

maybeParse :: (Val -> Parser a) -> Maybe Val -> Parser (Maybe a)
maybeParse parseValue Nothing = pure Nothing
maybeParse parseValue (Just value) = Just <$> parseValue value

maybeParseEnum :: [(T.Text, a)] -> Maybe Val -> Parser (Maybe a)
maybeParseEnum _ Nothing = pure Nothing
maybeParseEnum dict (Just val) = case getVal val of
  String str | Just x <- lookup str dict -> pure $ Just x
  _ -> parseFail val . T.unpack $ "expected '" <> T.intercalate "', '" (fst <$> dict) <> "'"

parseBool :: Val -> Parser Bool
parseBool (getVal -> Bool b) = pure b
parseBool v = parseFail v "Expected a Bool"

parseField :: String -> Val -> Parser Val
parseField s v = do
    x <- parseFieldOpt s v
    case x of
        Nothing -> parseFail v $ "Expected a field named " ++ s
        Just v -> pure v

parseFieldOpt :: String -> Val -> Parser (Maybe Val)
parseFieldOpt s v = do
    mp <- parseObject v
    case Map.lookup (T.pack s) mp of
        Nothing -> pure Nothing
        Just x -> pure $ Just $ addVal s x v

allowFields :: Val -> [String] -> Parser ()
allowFields v allow = do
    mp <- parseObject v
    let bad = map T.unpack (Map.keys mp) \\ allow
    when (bad /= []) $
        parseFail v
          $ "Not allowed keys: " ++ unwords bad
          ++ ", Allowed keys: " ++ unwords allow

parseGHC :: (ParseFlags -> String -> ParseResult v) -> Val -> Parser v
parseGHC parser v = do
    x <- parseString v
    case parser defaultParseFlags{enabledExtensions=configExtensions, disabledExtensions=[]} x of
        POk _ x -> pure x
        PFailed ps ->
          let errMsg = pprError . head . bagToList . snd $ getMessages ps
              msg = showSDoc baseDynFlags $ pprLocMsgEnvelope errMsg
          in parseFail v $ "Failed to parse " ++ msg ++ ", when parsing:\n " ++ x

---------------------------------------------------------------------
-- YAML TO DATA TYPE

newtype ConfigYamlBuiltin = ConfigYamlBuiltin { getConfigYamlBuiltin :: ConfigYaml }
  deriving (Semigroup, Monoid)

newtype ConfigYamlUser = ConfigYamlUser { getConfigYamlUser :: ConfigYaml }
  deriving (Semigroup, Monoid)

instance FromJSON ConfigYamlBuiltin where
    parseJSON Null = pure mempty
    parseJSON x = ConfigYamlBuiltin <$> parseConfigYaml True (newVal x)

instance FromJSON ConfigYamlUser where
  parseJSON Null = pure mempty
  parseJSON x = ConfigYamlUser <$> parseConfigYaml False (newVal x)

parseConfigYaml :: Bool -> Val -> Parser ConfigYaml
parseConfigYaml isBuiltin v = do
    vs <- parseArray v
    fmap ConfigYaml $ forM vs $ \o -> do
        (s, v) <- parseObject1 o
        case s of
            "package" -> ConfigPackage <$> parsePackage v
            "group" -> ConfigGroup <$> parseGroup isBuiltin v
            "arguments" -> ConfigSetting . map SettingArgument <$> parseArrayString v
            "fixity" -> ConfigSetting <$> parseFixity v
            "smell" -> ConfigSetting <$> parseSmell v
            _ | isJust $ getSeverity s -> ConfigGroup . ruleToGroup <$> parseRule isBuiltin o
            _ | Just r <- getRestrictType s -> ConfigSetting . map SettingRestrict <$> (parseArray v >>= mapM (parseRestrict r))
            _ -> parseFail v "Expecting an object with a 'package' or 'group' key, a hint or a restriction"


parsePackage :: Val -> Parser Package
parsePackage v = do
    packageName <- parseField "name" v >>= parseString
    packageModules <- parseField "modules" v >>= parseArray >>= mapM (fmap extendInstances <$> parseGHC parseImportDeclGhcWithMode)
    allowFields v ["name","modules"]
    pure Package{..}

parseFixity :: Val -> Parser [Setting]
parseFixity v = parseArray v >>= concatMapM (parseGHC parseDeclGhcWithMode >=> f)
    where
        f (L _ (SigD _ (FixSig _ x))) = pure $ map Infix $ fromFixitySig x
        f _ = parseFail v "Expected fixity declaration"

parseSmell :: Val -> Parser [Setting]
parseSmell v = do
  smellName <- parseField "type" v >>= parseString
  smellType <- require v "Expected SmellType"  $ getSmellType smellName
  smellLimit <- parseField "limit" v >>= parseInt
  pure [SettingSmell smellType smellLimit]
    where
      require :: Val -> String -> Maybe a -> Parser a
      require _ _ (Just a) = pure a
      require val err Nothing = parseFail val err

parseGroup :: Bool -> Val -> Parser Group
parseGroup isBuiltin v = do
    groupName <- parseField "name" v >>= parseString
    groupEnabled <- parseFieldOpt "enabled" v >>= maybe (pure True) parseBool
    groupImports <- parseFieldOpt "imports" v >>= maybe (pure []) (parseArray >=> mapM parseImport)
    groupRules <- parseFieldOpt "rules" v >>= maybe (pure []) parseArray >>= concatMapM (parseRule isBuiltin)
    allowFields v ["name","enabled","imports","rules"]
    pure Group{..}
    where
        parseImport v = do
            x <- parseString v
            case word1 x of
                 ("package", x) -> pure $ Left x
                 _ -> Right . extendInstances <$> parseGHC parseImportDeclGhcWithMode v

ruleToGroup :: [Either HintRule Classify] -> Group
ruleToGroup = Group "" True []

parseRule :: Bool -> Val -> Parser [Either HintRule Classify]
parseRule isBuiltin v = do
    (severity, v) <- parseSeverityKey v
    isRule <- isJust <$> parseFieldOpt "lhs" v
    if isRule then do
        hintRuleNotes <- parseFieldOpt "note" v >>= maybe (pure []) (fmap (map asNote) . parseArrayString)
        lhs <- parseField "lhs" v >>= parseGHC parseExpGhcWithMode
        rhs <- parseField "rhs" v >>= parseGHC parseExpGhcWithMode
        hintRuleSide <- parseFieldOpt "side" v >>= maybe (pure Nothing) (fmap (Just . extendInstances) . parseGHC parseExpGhcWithMode)
        hintRuleName <- parseFieldOpt "name" v >>= maybe (pure $ guessName lhs rhs) parseString

        allowFields v ["lhs","rhs","note","name","side"]
        let hintRuleScope = mempty
        pure $
          Left HintRule {hintRuleSeverity = severity, hintRuleLHS = extendInstances lhs, hintRuleRHS = extendInstances rhs, ..}
            : [Right $ Classify severity hintRuleName "" "" | not isBuiltin]
     else do
        names <- parseFieldOpt "name" v >>= maybe (pure []) parseArrayString
        within <- parseFieldOpt "within" v >>= maybe (pure [("","")]) (parseArray >=> concatMapM parseWithin)
        pure [Right $ Classify severity n a b | (a,b) <- within, n <- ["" | null names] ++ names]

parseRestrict :: RestrictType -> Val -> Parser Restrict
parseRestrict restrictType v = do
    def <- parseFieldOpt "default" v
    case def of
        Just def -> do
            b <- parseBool def
            allowFields v ["default"]
            pure $ Restrict restrictType b [] mempty mempty mempty mempty [] NoRestrictIdents Nothing
        Nothing -> do
            restrictName <- parseFieldOpt "name" v >>= maybe (pure []) parseArrayString
            restrictWithin <- parseFieldOpt "within" v >>= maybe (pure [("","")]) (parseArray >=> concatMapM parseWithin)
            restrictAs <- parseFieldOpt "as" v >>= maybe (pure []) parseArrayString
            restrictAsRequired <- parseFieldOpt "asRequired" v >>= fmap Alt . maybeParse parseBool
            restrictImportStyle <- parseFieldOpt "importStyle" v >>= fmap Alt . maybeParseEnum
              [ ("qualified"          , ImportStyleQualified)
              , ("unqualified"        , ImportStyleUnqualified)
              , ("explicit"           , ImportStyleExplicit)
              , ("explicitOrQualified", ImportStyleExplicitOrQualified)
              , ("unrestricted"       , ImportStyleUnrestricted)
              ]
            restrictQualifiedStyle <- parseFieldOpt "qualifiedStyle" v >>= fmap Alt . maybeParseEnum
              [ ("pre"         , QualifiedStylePre)
              , ("post"        , QualifiedStylePost)
              , ("unrestricted", QualifiedStyleUnrestricted)
              ]


            restrictBadIdents <- parseFieldOpt "badidents" v
            restrictOnlyAllowedIdents <- parseFieldOpt "only" v
            restrictIdents <-
                case (restrictBadIdents, restrictOnlyAllowedIdents) of
                    (Just badIdents, Nothing) -> ForbidIdents <$> parseArrayString badIdents
                    (Nothing, Just onlyIdents) -> OnlyIdents <$> parseArrayString onlyIdents
                    (Nothing, Nothing) -> pure NoRestrictIdents
                    _ -> parseFail v "The following options are mutually exclusive: badidents, only"

            restrictMessage <- parseFieldOpt "message" v >>= maybeParse parseString
            allowFields v $
                ["name", "within", "message"] ++
                if restrictType == RestrictModule
                    then ["as", "asRequired", "importStyle", "qualifiedStyle", "badidents", "only"]
                    else []
            pure Restrict{restrictDefault=True,..}

parseWithin :: Val -> Parser [(String, String)] -- (module, decl)
parseWithin v = do
    s <- parseString v
    if '*' `elem` s
        then pure [(s, "")]
        else do
            x <- parseGHC parseExpGhcWithMode v
            case x of
                L _ (HsVar _ (L _ (Unqual x))) -> pure $ f "" (occNameString x)
                L _ (HsVar _ (L _ (Qual mod x))) -> pure $ f (moduleNameString mod) (occNameString x)
                _ -> parseFail v "Bad classification rule"
            where
                f mod name@(c:_) | isUpper c = [(mod,name),(mod ++ ['.' | mod /= ""] ++ name, "")]
                f mod name = [(mod, name)]

parseSeverityKey :: Val -> Parser (Severity, Val)
parseSeverityKey v = do
    (s, v) <- parseObject1 v
    case getSeverity s of
        Just sev -> pure (sev, v)
        _ -> parseFail v $ "Key should be a severity (e.g. warn/error/suggest) but got " ++ s


guessName :: LHsExpr GhcPs -> LHsExpr GhcPs -> String
guessName lhs rhs
    | n:_ <- rs \\ ls = "Use " ++ n
    | n:_ <- ls \\ rs = "Redundant " ++ n
    | otherwise = defaultHintName
    where
        (ls, rs) = both f (lhs, rhs)
        f :: LHsExpr GhcPs -> [String]
        f x = [y | L _ (HsVar _ (L _ x)) <- universe x, let y = occNameStr x, not $ isUnifyVar y, y /= "."]


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
        packageMap' = Map.fromListWith (++) [(packageName, fmap unextendInstances packageModules) | Package{..} <- packages]
        groupMap = Map.fromListWith (\new old -> new) [(groupName, groupEnabled) | Group{..} <- groups]

        f Group{..}
            | Map.lookup groupName groupMap == Just False = []
            | otherwise = map (either (\r -> SettingMatchExp r{hintRuleScope=scope'}) SettingClassify) groupRules
            where
              scope'= asScope' packageMap' (map (fmap unextendInstances) groupImports)

asScope' :: Map.HashMap String [LocatedA (ImportDecl GhcPs)] -> [Either String (LocatedA (ImportDecl GhcPs))] -> Scope
asScope' packages xs = scopeCreate (HsModule EpAnnNotUsed NoLayoutInfo Nothing Nothing (concatMap f xs) [] Nothing Nothing)
    where
        f (Right x) = [x]
        f (Left x) | Just pkg <- Map.lookup x packages = pkg
                   | otherwise = error $ "asScope' failed to do lookup, " ++ x

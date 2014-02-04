{-# LANGUAGE PatternGuards, ViewPatterns, RecordWildCards #-}

module Settings(
    Severity(..), Classify(..), HintRule(..), Note(..), showNotes, Setting(..),
    defaultHintName, isUnifyVar,
    findSettings, readSettings,
    readSettings2, readPragma, findSettings2
    ) where

import HSE.All
import Control.Monad
import Data.Char
import Data.List
import Data.Monoid
import System.FilePath
import Util


defaultHintName :: String
defaultHintName = "Use alternative"


-- | How severe an issue is.
data Severity
    = Ignore -- ^ The issue has been explicitly ignored and will usually be hidden (pass @--show@ on the command line to see ignored ideas).
    | Warning -- ^ Warnings are things that some people may consider improvements, but some may not.
    | Error -- ^ Errors are suggestions that are nearly always a good idea to apply.
      deriving (Eq,Ord,Show,Read,Bounded,Enum)

getSeverity :: String -> Maybe Severity
getSeverity "ignore" = Just Ignore
getSeverity "warn" = Just Warning
getSeverity "warning" = Just Warning
getSeverity "error"  = Just Error
getSeverity "hint"  = Just Error
getSeverity _ = Nothing


-- Any 1-letter variable names are assumed to be unification variables
isUnifyVar :: String -> Bool
isUnifyVar [x] = x == '?' || isAlpha x
isUnifyVar _ = False


addInfix = parseFlagsAddFixities $ infix_ (-1) ["==>"]


---------------------------------------------------------------------
-- TYPE

-- | A note describing the impact of the replacement.
data Note
    = IncreasesLaziness -- ^ The replacement is increases laziness, for example replacing @reverse (reverse x)@ with @x@ makes the code lazier.
    | DecreasesLaziness -- ^ The replacement is decreases laziness, for example replacing @(fst x, snd x)@ with @x@ makes the code stricter.
    | RemovesError String -- ^ The replacement removes errors, for example replacing @foldr1 (+)@ with @sum@ removes an error on @[]@, and might contain the text @\"on []\"@.
    | ValidInstance String String -- ^ The replacement assumes standard type class lemmas, a hint with the note @ValidInstance \"Eq\" \"x\"@ might only be valid if
                                  --   the @x@ variable has a reflexive @Eq@ instance.
    | Note String -- ^ An arbitrary note.
      deriving (Eq,Ord)

instance Show Note where
    show IncreasesLaziness = "increases laziness"
    show DecreasesLaziness = "decreases laziness"
    show (RemovesError x) = "removes error " ++ x
    show (ValidInstance x y) = "requires a valid " ++ x ++ " instance for " ++ y
    show (Note x) = x


showNotes :: [Note] -> String
showNotes = intercalate ", " . map show . filter use
    where use ValidInstance{} = False -- Not important enough to tell an end user
          use _ = True

-- | How to classify an 'Idea'. If any matching field is @\"\"@ then it matches everything.
data Classify = Classify
    {classifySeverity :: Severity -- ^ Severity to set the 'Idea' to.
    ,classifyHint :: String -- ^ 'ideaHint'.
    ,classifyModule :: String -- ^ 'ideaModule'.
    ,classifyDecl :: String -- ^ 'ideaDecl'.
    }
    deriving Show

-- | A @LHS ==> RHS@ style hint rule.
data HintRule = HintRule
    {hintRuleSeverity :: Severity -- ^ Default severity for the hint.
    ,hintRuleName :: String -- ^ Name for the hint.
    ,hintRuleScope :: Scope -- ^ Module scope in which the hint operates.
    ,hintRuleLHS :: Exp SrcSpanInfo -- ^ LHS
    ,hintRuleRHS :: Exp SrcSpanInfo -- ^ RHS
    ,hintRuleSide :: Maybe (Exp SrcSpanInfo) -- ^ Side condition, typically specified with @where _ = ...@.
    ,hintRuleNotes :: [Note] -- ^ Notes about application of the hint.
    }
    deriving Show

data Setting
    = SettingClassify Classify
    | SettingMatchExp HintRule
    | Builtin String -- use a builtin hint set
    | Infix Fixity
      deriving Show


---------------------------------------------------------------------
-- READ A SETTINGS FILE

-- Given a list of hint files to start from
-- Return the list of settings commands
readSettings2 :: FilePath -> [FilePath] -> [String] -> IO [Setting]
readSettings2 dataDir files hints = do
    (builtin,mods) <- fmap unzipEither $ concatMapM (readHints dataDir) $ map Right files ++ map Left hints
    return $ map Builtin builtin ++ concatMap moduleSettings_ mods

moduleSettings_ :: Module SrcSpanInfo -> [Setting]
moduleSettings_ m = concatMap (readSetting $ scopeCreate m) $ concatMap getEquations $
                       [AnnPragma l x | AnnModulePragma l x <- modulePragmas m] ++ moduleDecls m

-- | Given a module containing HLint settings information return the 'Classify' rules and the 'HintRule' expressions.
--   Any fixity declarations will be discarded, but any other unrecognised elements will result in an exception.
readSettings :: Module SrcSpanInfo -> ([Classify], [HintRule])
readSettings m = ([x | SettingClassify x <- xs], [x | SettingMatchExp x <- xs])
    where xs = moduleSettings_ m


readHints :: FilePath -> Either String FilePath -> IO [Either String Module_]
readHints datadir x = do
    (builtin,errs,ms) <- case x of
        Left src -> findSettings datadir "CommandLine" (Just src)
        Right file -> findSettings datadir file Nothing
    forM_ errs $ \(ParseError sl msg _) -> return $! fromParseResult $ ParseFailed sl msg
    return $ map Left builtin ++ map Right ms


-- | Given the data directory (where the @hlint@ data files reside), and a filename to read, and optionally that file's
--   contents, produce a triple containing:
--
-- 1. Builtin hints to use, e.g. @"List"@, which should be resolved using 'builtinHints'.
--
-- 1. A list of parse errors produced while parsing settings files.
--
-- 1. A list of modules containing hints, suitable for processing with 'readSettings'.
findSettings :: FilePath -> FilePath -> Maybe String -> IO ([String], [ParseError], [Module SrcSpanInfo])
findSettings dataDir file contents = do
    let flags = addInfix defaultParseFlags
    res <- parseModuleEx flags file contents
    case res of
        Left err -> return ([], [err], [])
        Right m -> do
            ys <- sequence [f $ fromNamed $ importModule i | i <- moduleImports m, importPkg i `elem` [Just "hint", Just "hlint"]]
            return $ concat3 $ ([],[],[m]) : ys
    where
        f x | Just x <- "HLint.Builtin." `stripPrefix` x = return ([x],[],[])
            | Just x <- "HLint." `stripPrefix` x = findSettings dataDir (dataDir </> x <.> "hs") Nothing
            | otherwise = findSettings dataDir (x <.> "hs") Nothing


readSetting :: Scope -> Decl_ -> [Setting]
readSetting s (FunBind _ [Match _ (Ident _ (getSeverity -> Just severity)) pats (UnGuardedRhs _ bod) bind])
    | InfixApp _ lhs op rhs <- bod, opExp op ~= "==>" =
        let (a,b) = readSide $ childrenBi bind in
        [SettingMatchExp $ HintRule severity (headDef defaultHintName names) s (fromParen lhs) (fromParen rhs) a b]
    | otherwise = [SettingClassify $ Classify severity n a b | n <- names2, (a,b) <- readFuncs bod]
    where
        names = filter notNull $ getNames pats bod
        names2 = ["" | null names] ++ names

readSetting s x | "test" `isPrefixOf` map toLower (fromNamed x) = []
readSetting s (AnnPragma _ x) | Just y <- readPragma x = [SettingClassify y]
readSetting s (PatBind an (PVar _ name) _ bod bind) = readSetting s $ FunBind an [Match an name [] bod bind]
readSetting s (FunBind an xs) | length xs /= 1 = concatMap (readSetting s . FunBind an . return) xs
readSetting s (SpliceDecl an (App _ (Var _ x) (Lit _ y))) = readSetting s $ FunBind an [Match an (toNamed $ fromNamed x) [PLit an y] (UnGuardedRhs an $ Lit an $ String an "" "") Nothing]
readSetting s x@InfixDecl{} = map Infix $ getFixity x
readSetting s x = errorOn x "bad hint"


-- return Nothing if it is not an HLint pragma, otherwise all the settings
readPragma :: Annotation S -> Maybe Classify
readPragma o = case o of
    Ann _ name x -> f (fromNamed name) x
    TypeAnn _ name x -> f (fromNamed name) x
    ModuleAnn _ x -> f "" x
    where
        f name (Lit _ (String _ s _)) | "hlint:" `isPrefixOf` map toLower s =
                case getSeverity a of
                    Nothing -> errorOn o "bad classify pragma"
                    Just severity -> Just $ Classify severity (ltrim b) "" name
            where (a,b) = break isSpace $ ltrim $ drop 6 s
        f name (Paren _ x) = f name x
        f name (ExpTypeSig _ x _) = f name x
        f _ _ = Nothing


readSide :: [Decl_] -> (Maybe Exp_, [Note])
readSide = foldl f (Nothing,[])
    where f (Nothing,notes) (PatBind _ PWildCard{} Nothing (UnGuardedRhs _ side) Nothing) = (Just side, notes)
          f (Nothing,notes) (PatBind _ (fromNamed -> "side") Nothing (UnGuardedRhs _ side) Nothing) = (Just side, notes)
          f (side,[]) (PatBind _ (fromNamed -> "note") Nothing (UnGuardedRhs _ note) Nothing) = (side,g note)
          f _ x = errorOn x "bad side condition"

          g (Lit _ (String _ x _)) = [Note x]
          g (List _ xs) = concatMap g xs
          g x = case fromApps x of
              [con -> Just "IncreasesLaziness"] -> [IncreasesLaziness]
              [con -> Just "DecreasesLaziness"] -> [DecreasesLaziness]
              [con -> Just "RemovesError",str -> Just a] -> [RemovesError a]
              [con -> Just "ValidInstance",str -> Just a,var -> Just b] -> [ValidInstance a b]
              _ -> errorOn x "bad note"

          con :: Exp_ -> Maybe String
          con c@Con{} = Just $ prettyPrint c; con _ = Nothing
          var c@Var{} = Just $ prettyPrint c; var _ = Nothing
          str c = if isString c then Just $ fromString c else Nothing


-- Note: Foo may be ("","Foo") or ("Foo",""), return both
readFuncs :: Exp_ -> [(String, String)]
readFuncs (App _ x y) = readFuncs x ++ readFuncs y
readFuncs (Lit _ (String _ "" _)) = [("","")]
readFuncs (Var _ (UnQual _ name)) = [("",fromNamed name)]
readFuncs (Var _ (Qual _ (ModuleName _ mod) name)) = [(mod, fromNamed name)]
readFuncs (Con _ (UnQual _ name)) = [(fromNamed name,""),("",fromNamed name)]
readFuncs (Con _ (Qual _ (ModuleName _ mod) name)) = [(mod ++ "." ++ fromNamed name,""),(mod,fromNamed name)]
readFuncs x = errorOn x "bad classification rule"


getNames :: [Pat_] -> Exp_ -> [String]
getNames ps _ | ps /= [] && all isPString ps = map fromPString ps
getNames [] (InfixApp _ lhs op rhs) | opExp op ~= "==>" = map ("Use "++) names
    where
        lnames = map f $ childrenS lhs
        rnames = map f $ childrenS rhs
        names = filter (not . isUnifyVar) $ (rnames \\ lnames) ++ rnames
        f (Ident _ x) = x
        f (Symbol _ x) = x
getNames _ _ = []


errorOn :: (Annotated ast, Pretty (ast S)) => ast S -> String -> b
errorOn val msg = exitMessage $
    showSrcLoc (getPointLoc $ ann val) ++
    ": Error while reading hint file, " ++ msg ++ "\n" ++
    prettyPrint val


---------------------------------------------------------------------
-- FIND SETTINGS IN A SOURCE FILE

-- find definitions in a source file
findSettings2 :: ParseFlags -> FilePath -> IO (String, [Setting])
findSettings2 flags file = do
    x <- parseModuleEx flags file Nothing
    case x of
        Left (ParseError sl msg _) ->
            return ("-- Parse error " ++ showSrcLoc sl ++ ": " ++ msg, [])
        Right m -> do
            let xs = concatMap (findSetting $ UnQual an) (moduleDecls m)
                s = unlines $ ["-- hints found in " ++ file] ++ map prettyPrint xs ++ ["-- no hints found" | null xs]
                r = concatMap (readSetting mempty) xs
            return (s,r)


findSetting :: (Name S -> QName S) -> Decl_ -> [Decl_]
findSetting qual (InstDecl _ _ _ (Just xs)) = concatMap (findSetting qual) [x | InsDecl _ x <- xs]
findSetting qual (PatBind _ (PVar _ name) Nothing (UnGuardedRhs _ bod) Nothing) = findExp (qual name) [] bod
findSetting qual (FunBind _ [InfixMatch _ p1 name ps rhs bind]) = findSetting qual $ FunBind an [Match an name (p1:ps) rhs bind]
findSetting qual (FunBind _ [Match _ name ps (UnGuardedRhs _ bod) Nothing]) = findExp (qual name) [] $ Lambda an ps bod
findSetting _ x@InfixDecl{} = [x]
findSetting _ _ = []


-- given a result function name, a list of variables, a body expression, give some hints
findExp :: QName S -> [String] -> Exp_ -> [Decl_]
findExp name vs (Lambda _ ps bod) | length ps2 == length ps = findExp name (vs++ps2) bod
                                  | otherwise = []
    where ps2 = [x | PVar_ x <- map view ps]
findExp name vs Var{} = []
findExp name vs (InfixApp _ x dot y) | isDot dot = findExp name (vs++["_hlint"]) $ App an x $ Paren an $ App an y (toNamed "_hlint")

findExp name vs bod = [PatBind an (toNamed "warn") Nothing (UnGuardedRhs an $ InfixApp an lhs (toNamed "==>") rhs) Nothing]
    where
        lhs = g $ transform f bod
        rhs = apps $ Var an name : map snd rep

        rep = zip vs $ map (toNamed . return) ['a'..]
        f xx | Var_ x <- view xx, Just y <- lookup x rep = y
        f (InfixApp _ x dol y) | isDol dol = App an x (paren y)
        f x = x

        g o@(InfixApp _ _ _ x) | isAnyApp x || isAtom x = o
        g o@App{} = o
        g o = paren o

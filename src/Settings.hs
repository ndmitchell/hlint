{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Settings(
    Severity(..), Note(..), showNotes, FuncName, Setting(..), isClassify, isMatchExp,
    defaultHintName, isUnifyVar,
    readSettings, readPragma, findSettings
    ) where

import HSE.All
import Data.Char
import Data.List
import System.FilePath
import Util


defaultHintName = "Use alternative"


-- | How severe an error is.
data Severity
    = Ignore -- ^ Ignored errors are only returned when @--show@ is passed.
    | Warning -- ^ Warnings are things that some people may consider improvements, but some may not.
    | Error -- ^ Errors are suggestions that should nearly always be a good idea to apply.
      deriving (Eq,Ord,Show,Read,Bounded,Enum)

getSeverity :: String -> Maybe Severity
getSeverity "ignore" = Just Ignore
getSeverity "warn" = Just Warning
getSeverity "warning" = Just Warning
getSeverity "error"  = Just Error
getSeverity "hint"  = Just Error
getSeverity _ = Nothing


-- (modulename,functionname)
-- either being blank implies universal matching
type FuncName = (String,String)


-- Any 1-letter variable names are assumed to be unification variables
isUnifyVar :: String -> Bool
isUnifyVar [x] = x == '?' || isAlpha x
isUnifyVar _ = False


addInfix x = x{infixes = infix_ (-1) ["==>"] ++ infixes x}


---------------------------------------------------------------------
-- TYPE

data Note
    = IncreasesLaziness
    | DecreasesLaziness
    | RemovesError String -- RemovesError "on []", RemovesError "when x is negative"
    | ValidInstance String String -- ValidInstance "Eq" "x"
    | Note String
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


data Setting
    = Classify {severityS :: Severity, hintS :: String, funcS :: FuncName}
    | MatchExp {severityS :: Severity, hintS :: String, scope :: Scope, lhs :: Exp_, rhs :: Exp_, side :: Maybe Exp_, notes :: [Note]}
    | Builtin String -- use a builtin hint set
    | Infix Fixity
      deriving Show

isClassify Classify{} = True; isClassify _ = False
isMatchExp MatchExp{} = True; isMatchExp _ = False


---------------------------------------------------------------------
-- READ A SETTINGS FILE

-- Given a list of hint files to start from
-- Return the list of settings commands
readSettings :: FilePath -> [FilePath] -> [String] -> IO [Setting]
readSettings dataDir files hints = do
    (builtin,mods) <- fmap unzipEither $ concatMapM (readHints dataDir) $ map Right files ++ map Left hints
    let f m = concatMap (readSetting $ moduleScope m) $ concatMap getEquations $ moduleDecls m
    return $ map Builtin builtin ++ concatMap f mods


-- Read a hint file, and all hint files it imports
readHints :: FilePath -> Either String FilePath -> IO [Either String Module_]
readHints dataDir file = do
    let flags = addInfix parseFlags
    y <- parseResult $ either (parseString flags "CommandLine") (parseFile flags) file
    ys <- concatM [f $ fromNamed $ importModule i | i <- moduleImports y, importPkg i `elem` [Just "hint", Just "hlint"]]
    return $ Right y:ys
    where
        f x | "HLint.Builtin." `isPrefixOf` x = return [Left $ drop 14 x]
            | "HLint." `isPrefixOf` x = readHints dataDir $ Right $ dataDir </> drop 6 x <.> "hs"
            | otherwise = readHints dataDir $ Right $ x <.> "hs"


readSetting :: Scope -> Decl_ -> [Setting]
readSetting s (FunBind _ [Match _ (Ident _ (getSeverity -> Just severity)) pats (UnGuardedRhs _ bod) bind])
    | InfixApp _ lhs op rhs <- bod, opExp op ~= "==>" =
        let (a,b) = readSide $ childrenBi bind in
        [MatchExp severity (headDef defaultHintName names) s (fromParen lhs) (fromParen rhs) a b]
    | otherwise = [Classify severity n func | n <- names2, func <- readFuncs bod]
    where
        names = filter notNull $ getNames pats bod
        names2 = ["" | null names] ++ names

readSetting s x | "test" `isPrefixOf` map toLower (fromNamed x) = []
readSetting s x@AnnPragma{} | Just y <- readPragma x = [y]
readSetting s (PatBind an (PVar _ name) _ bod bind) = readSetting s $ FunBind an [Match an name [] bod bind]
readSetting s (FunBind an xs) | length xs /= 1 = concatMap (readSetting s . FunBind an . return) xs
readSetting s (SpliceDecl an (App _ (Var _ x) (Lit _ y))) = readSetting s $ FunBind an [Match an (toNamed $ fromNamed x) [PLit an y] (UnGuardedRhs an $ Lit an $ String an "" "") Nothing]
readSetting s x@InfixDecl{} = map Infix $ getFixity x
readSetting s x = errorOn x "bad hint"


-- return Nothing if it is not an HLint pragma, otherwise all the settings
readPragma :: Decl_ -> Maybe Setting
readPragma o@(AnnPragma _ p) = f p
    where
        f (Ann _ name x) = g (fromNamed name) x
        f (TypeAnn _ name x) = g (fromNamed name) x
        f (ModuleAnn _ x) = g "" x

        g name (Lit _ (String _ s _)) | "hlint:" `isPrefixOf` map toLower s =
                case getSeverity a of
                    Nothing -> errorOn o "bad classify pragma"
                    Just severity -> Just $ Classify severity (ltrim b) ("",name)
            where (a,b) = break isSpace $ ltrim $ drop 6 s
        g name (Paren _ x) = g name x
        g name (ExpTypeSig _ x _) = g name x
        g _ _ = Nothing
readPragma _ = Nothing


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
readFuncs :: Exp_ -> [FuncName]
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
findSettings :: ParseFlags -> FilePath -> IO (String, [Setting])
findSettings flags file = do
    x <- parseFile flags file
    case snd x of
        ParseFailed sl msg ->
            return ("-- Parse error " ++ showSrcLoc sl ++ ": " ++ msg, [])
        ParseOk m -> do
            let xs = concatMap (findSetting $ UnQual an) (moduleDecls m)
                s = unlines $ ["-- hints found in " ++ file] ++ map prettyPrint xs ++ ["-- no hints found" | null xs]
                r = concatMap (readSetting emptyScope) xs
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

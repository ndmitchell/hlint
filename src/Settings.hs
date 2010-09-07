{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Settings(
    Rank(..), FuncName, Setting(..), isClassify, isMatchExp,
    defaultHintName, isUnifyVar,
    readSettings, readPragma, findSettings
    ) where

import HSE.All
import Data.Char
import Data.List
import System.FilePath
import Util


defaultHintName = "Use alternative"


data Rank = Ignore | Warning | Error
            deriving (Eq,Ord,Show)

getRank :: String -> Maybe Rank
getRank "ignore" = Just Ignore
getRank "warn" = Just Warning
getRank "warning" = Just Warning
getRank "error"  = Just Error
getRank _ = Nothing


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

data Setting
    = Classify {rankS :: Rank, hintS :: String, funcS :: FuncName}
    | MatchExp {rankS :: Rank, hintS :: String, scope :: Scope, lhs :: Exp_, rhs :: Exp_, side :: Maybe Exp_}
    | Builtin String -- use a builtin hint set
    | Infix Fixity
      deriving Show

isClassify Classify{} = True; isClassify _ = False
isMatchExp MatchExp{} = True; isMatchExp _ = False


---------------------------------------------------------------------
-- READ A SETTINGS FILE

-- Given a list of hint files to start from
-- Return the list of settings commands
readSettings :: FilePath -> [FilePath] -> IO [Setting]
readSettings dataDir xs = do
    (builtin,mods) <- fmap unzipEither $ concatMapM (readHints dataDir) xs
    let f m = concatMap (readSetting $ moduleScope m) $ concatMap getEquations $ moduleDecls m
    return $ map Builtin builtin ++ concatMap f mods


-- Read a hint file, and all hint files it imports
readHints :: FilePath -> FilePath -> IO [Either String Module_]
readHints dataDir file = do
    y <- parseResult $ parseFile (addInfix parseFlags) file
    ys <- concatM [f $ fromNamed $ importModule i | i <- moduleImports y, importPkg i == Just "hint"]
    return $ Right y:ys
    where
        f x | "HLint.Builtin." `isPrefixOf` x = return [Left $ drop 14 x]
            | "HLint." `isPrefixOf` x = readHints dataDir $ dataDir </> drop 6 x <.> "hs"
            | otherwise = readHints dataDir $ x <.> "hs"


readSetting :: Scope -> Decl_ -> [Setting]
readSetting s (FunBind _ [Match _ (Ident _ (getRank -> Just rank)) pats (UnGuardedRhs _ bod) bind])
    | InfixApp _ lhs op rhs <- bod, opExp op ~= "==>" =
        [MatchExp rank (if null names then defaultHintName else head names) s (fromParen lhs) (fromParen rhs) (readSide $ childrenBi bind)]
    | otherwise = [Classify rank n func | n <- names2, func <- readFuncs bod]
    where
        names = filter notNull $ getNames pats bod
        names2 = ["" | null names] ++ names

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
                case getRank a of
                    Nothing -> errorOn o "bad classify pragma"
                    Just rank -> Just $ Classify rank (ltrim b) ("",name)
            where (a,b) = break isSpace $ ltrim $ drop 6 s
readPragma _ = Nothing


readSide :: [Decl_] -> Maybe Exp_
readSide [] = Nothing
readSide [PatBind _ PWildCard{} Nothing (UnGuardedRhs _ bod) Nothing] = Just bod
readSide (x:_) = errorOn x "bad side condition"


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
    showSrcLoc (getPointLoc $ ann val)  ++
    " Error while reading hint file, " ++ msg ++ "\n" ++
    prettyPrint val


---------------------------------------------------------------------
-- FIND SETTINGS IN A SOURCE FILE

-- find definitions in a source file
findSettings :: ParseFlags -> FilePath -> IO (String, [Setting])
findSettings flags file = do
    x <- parseResult $ parseFile flags file
    let xs = concatMap (findSetting $ UnQual an) (moduleDecls x)
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

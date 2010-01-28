{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Settings(readSettings, classify, defaultHintName) where

import HSE.All
import Type
import Data.List
import System.FilePath
import Util


-- Given a list of hint files to start from
-- Return the list of settings commands
readSettings :: FilePath -> [FilePath] -> IO [Setting]
readSettings dataDir xs = do
    (builtin,mods) <- fmap unzipEither $ concatMapM (readHints dataDir) xs
    return $ map Builtin builtin ++ concatMap (concatMap readSetting . concatMap getEquations . moduleDecls) mods


-- read all the files
-- in future this should also do import chasing, but
-- currently it doesn't
readHints :: FilePath -> FilePath -> IO [Either String Module_]
readHints dataDir file = do
    y <- fromParseResult `fmap` parseFile parseFlags{implies=True} file
    ys <- concatMapM (f . fromNamed . importModule) $ moduleImports y
    return $ Right y:ys
    where
        f x | "HLint.Builtin." `isPrefixOf` x = return [Left $ drop 14 x]
            | "HLint." `isPrefixOf` x = readHints dataDir $ dataDir </> drop 6 x <.> "hs"
            | otherwise = readHints dataDir $ x <.> "hs"


-- Eta bound variable lifted so the filter only happens once per classify
classify :: [Setting] -> Idea -> Idea
classify xs = \i -> if isParseError i then i else i{rank = foldl' (rerank i) (rank i) xs2}
    where
        xs2 = filter isClassify xs

        -- figure out if we need to change the rank
        rerank :: Idea -> Rank -> Setting -> Rank
        rerank i r c | matchHint (hintS c) (hint i) && matchFunc (funcS c) (func i) = rankS c
                     | otherwise = r

        matchHint = (~=)
        matchFunc (x1,x2) (y1,y2) = (x1~=y1) && (x2~=y2)
        x ~= y = null x || x == y


---------------------------------------------------------------------
-- READ A HINT

defaultHintName = "Use alternative"

readSetting :: Decl_ -> [Setting]
readSetting (FunBind _ [Match _ (Ident _ (getRank -> Just rank)) pats (UnGuardedRhs _ bod) bind])
    | InfixApp _ lhs op rhs <- bod, opExp op ~= "==>" =
        [MatchExp rank (if null names then defaultHintName else head names) (fromParen lhs) (fromParen rhs) (readSide $ childrenBi bind)]
    | otherwise = [Classify rank n func | n <- names2, func <- readFuncs bod]
    where
        names = getNames pats bod
        names2 = ["" | null names] ++ names


readSetting (PatBind _ (PVar _ name) _ bod bind) = readSetting $ FunBind an [Match an name [PLit an (String an "" "")] bod bind]
readSetting (FunBind _ xs) | length xs /= 1 = concatMap (readSetting . FunBind an . (:[])) xs
readSetting (SpliceDecl an (App _ (Var _ x) (Lit _ y))) = readSetting $ FunBind an [Match an (toNamed $ fromNamed x) [PLit an y] (UnGuardedRhs an $ Lit an $ String an "" "") Nothing]
readSetting x = errorOn x "bad hint"


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


-- errorOn :: Pretty x => x -> String -> a
errorOn val msg = exitMessage $
    showSrcLoc (getPointLoc $ ann val)  ++
    " Error while reading hint file, " ++ msg ++ "\n" ++
    prettyPrint val


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


getRank :: String -> Maybe Rank
getRank "ignore" = Just Ignore
getRank "warn" = Just Warning
getRank "warning" = Just Warning
getRank "error"  = Just Error
getRank _ = Nothing
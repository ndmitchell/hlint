{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Settings where

import HSE.All
import Paths_hlint
import Type
import Data.Char
import Data.List
import System.Directory
import Data.Generics.PlateData


-- Given a list of hint files to start from ([] = use default)
-- Return the list of settings commands
readSettings :: [FilePath] -> IO [Setting]
readSettings xs = do
    dat <- getDataDir
    b <- doesFileExist "Hints.hs"
    mods <- pickFiles $
        [dat ++ "/Hints.hs"] ++ ["Hints.hs" | b && null xs] ++ xs
    return $ concatMap (concatMap readSetting . concatMap getEquations . moduleDecls) mods


-- read all the files
-- in future this should also do import chasing, but
-- currently it doesn't
pickFiles :: [FilePath] -> IO [Module]
pickFiles = mapM parseFile


-- Eta bound variable lifted so the filter only happens once per classify
classify :: [Setting] -> Idea -> Rank
classify xs = \i -> foldl'
        (\r c -> if matchHint (hint c) (hint i) && matchFunc (func c) (func i) then rank c else r)
        Warn xs2
    where
        xs2 = filter isClassify xs

        matchHint x y = x ~= y
        matchFunc (x1,x2) (y1,y2) = (x1~=y1) && (x2~=y2)
        x ~= y = null x || x == y


---------------------------------------------------------------------
-- READ A HINT

readSetting :: Decl -> [Setting]
readSetting (FunBind [Match src (Ident (getRank -> rank)) pats
           (UnGuardedRhs bod) (BDecls bind)])
    | InfixApp lhs op rhs <- bod, opExp op ~= "==>", [name] <- names =
        [MatchExp rank name (fromParen lhs) (fromParen rhs) (readSide bind)]
    | otherwise = [Classify func rank n | n <- names2, func <- readFuncs bod]
    where
        names = getNames pats bod
        names2 = ["" | null names] ++ names


readSetting (PatBind src (PVar name) bod bind) = readSetting $ FunBind [Match src name [PLit (String "")] bod bind]
readSetting (FunBind xs) | length xs /= 1 = concatMap (readSetting . FunBind . (:[])) xs
readSetting x = error $ "Failed to read hint " ++ maybe "" showSrcLoc (getSrcLoc x) ++ "\n" ++ prettyPrint x


readSide :: [Decl] -> Maybe Exp
readSide [] = Nothing
readSide [PatBind src PWildCard (UnGuardedRhs bod) (BDecls [])] = Just bod
readSide (x:_) = error $ "Failed to read side condition " ++ maybe "" showSrcLoc (getSrcLoc x) ++ "\n" ++ prettyPrint x


readFuncs :: Exp -> [FuncName]
readFuncs (App x y) = readFuncs x ++ readFuncs y
readFuncs (Lit (String "")) = [("","")]
readFuncs (Var (UnQual name)) = [("",fromName name)]
readFuncs (Var (Qual (ModuleName mod) name)) = [(mod, fromName name)]
readFuncs x = error $ "Failed to read classification rule\n" ++ prettyPrint x



getNames :: [Pat] -> Exp -> [String]
getNames ps _ | ps /= [] && all isPString ps = map fromPString ps
getNames [] (InfixApp lhs op rhs) | opExp op ~= "==>" = ["Use " ++ head names]
    where
        names = filter (not . isFreeVar) $ map f (childrenBi rhs) \\ map f (childrenBi lhs) 
        f (Ident x) = x
        f (Symbol x) = x
getNames [] _ = [""]

-- TODO: Duplicated from Hint.Match
isFreeVar :: String -> Bool
isFreeVar [x] = x == '?' || isAlpha x
isFreeVar _ = False


getRank :: String -> Rank
getRank "skip" = Skip
getRank "warn" = Warn
getRank "fix" = Error

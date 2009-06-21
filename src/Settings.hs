{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Settings where

import HSE.All
import Paths_hlint
import Type
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import Util
import Data.Generics.PlateData


-- Given a list of hint files to start from
-- Return the list of settings commands
readSettings :: [FilePath] -> IO [Setting]
readSettings xs = do
    mods <- concatMapM readHints xs
    return $ concatMap (concatMap readSetting . concatMap getEquations . moduleDecls) mods


-- read all the files
-- in future this should also do import chasing, but
-- currently it doesn't
readHints :: FilePath -> IO [Module]
readHints file = do
    y <- parseFile file
    ys <- concatMapM (f . fromNamed . importModule) $ moduleImports y
    return $ y:ys
    where
        f x | "HLint." `isPrefixOf` x = do
                dat <- getDataDir
                readHints $ dat </> drop 6 x <.> "hs"
            | otherwise = readHints $ x <.> "hs"


-- Eta bound variable lifted so the filter only happens once per classify
classify :: [Setting] -> Idea -> Idea
classify xs = \i -> i{rank=foldl'
        (\r c -> if matchHint (hint c) (hint i) && matchFunc (func c) (func i) then rank c else r)
        (rank i) xs2}
    where
        xs2 = filter isClassify xs

        matchHint x y = x ~= y
        matchFunc (x1,x2) (y1,y2) = (x1~=y1) && (x2~=y2)
        x ~= y = null x || x == y


---------------------------------------------------------------------
-- READ A HINT

readSetting :: Decl -> [Setting]
readSetting (FunBind [Match src (Ident (getRank -> rank)) pats _
           (UnGuardedRhs bod) (BDecls bind)])
    | InfixApp lhs op rhs <- bod, opExp op ~= "==>" =
        [MatchExp rank (head names) (fromParen lhs) (fromParen rhs) (readSide bind)]
    | otherwise = [Classify func rank n | n <- names2, func <- readFuncs bod]
    where
        names = getNames pats bod
        names2 = ["" | null names] ++ names


readSetting (PatBind src (PVar name) typ bod bind) = readSetting $ FunBind [Match src name [PLit (String "")] typ bod bind]
readSetting (FunBind xs) | length xs /= 1 = concatMap (readSetting . FunBind . (:[])) xs
readSetting x = error $ "Failed to read hint " ++ maybe "" showSrcLoc (getSrcLoc x) ++ "\n" ++ prettyPrint x


readSide :: [Decl] -> Maybe Exp
readSide [] = Nothing
readSide [PatBind src PWildCard Nothing (UnGuardedRhs bod) (BDecls [])] = Just bod
readSide (x:_) = error $ "Failed to read side condition " ++ maybe "" showSrcLoc (getSrcLoc x) ++ "\n" ++ prettyPrint x


readFuncs :: Exp -> [FuncName]
readFuncs (App x y) = readFuncs x ++ readFuncs y
readFuncs (Lit (String "")) = [("","")]
readFuncs (Var (UnQual name)) = [("",fromNamed name)]
readFuncs (Var (Qual (ModuleName mod) name)) = [(mod, fromNamed name)]
readFuncs (Con (UnQual name)) = [(fromNamed name,"")]
readFuncs (Con (Qual (ModuleName mod) name)) = [(mod ++ "." ++ fromNamed name,"")]
readFuncs x = error $ "Failed to read classification rule\n" ++ prettyPrint x



getNames :: [Pat] -> Exp -> [String]
getNames ps _ | ps /= [] && all isPString ps = map fromPString ps
getNames [] (InfixApp lhs op rhs) | opExp op ~= "==>" = ["Use " ++ head names]
    where
        lnames = map f $ childrenBi lhs
        rnames = map f $ childrenBi rhs
        names = filter (not . isUnifyVar) $ (rnames \\ lnames) ++ rnames
        f (Ident x) = x
        f (Symbol x) = x
getNames [] _ = [""]


getRank :: String -> Rank
getRank "ignore" = Ignore
getRank "warn" = Warning
getRank "warning" = Warning
getRank "error"  = Error

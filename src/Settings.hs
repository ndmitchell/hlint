{-# LANGUAGE PatternGuards #-}

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
    return $ concatMap (concatMap readSetting . moduleDecls) mods


-- read all the files
-- in future this should also do import chasing, but
-- currently it doesn't
pickFiles :: [FilePath] -> IO [Module]
pickFiles files = mapM parseFile files



classify :: [Setting] -> String -> FuncName -> Rank
classify _ _ _ = Warn



---------------------------------------------------------------------
-- READ A HINT

readSetting :: Decl -> [Setting]
readSetting (FunBind [Match src (Ident name) [PLit (String msg)]
           (UnGuardedRhs bod) (BDecls bind)])
    | name == "hint", InfixApp lhs op rhs <- bod, opExp op ~= "==>" =
        [Hint (if null msg then pickName lhs rhs else msg) (fromParen lhs) (fromParen rhs) (readSide bind)]
    | Just rank <- getRank name =
        map (Classify rank msg) $ readFuncs bod

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


pickName :: Exp -> Exp -> String
pickName lhs rhs | null names = "Unnamed suggestion"
                 | otherwise = "Use " ++ head names
    where
        names = filter (not . isFreeVar) $ map f (childrenBi rhs) \\ map f (childrenBi lhs) 
        f (Ident x) = x
        f (Symbol x) = x


-- TODO: Duplicated from Hint.Match
isFreeVar :: String -> Bool
isFreeVar [x] = x == '?' || isAlpha x
isFreeVar _ = False


getRank :: String -> Maybe Rank
getRank "skip" = Just Skip
getRank "warn" = Just Warn
getRank "error" = Just Error
getRank _ = Nothing

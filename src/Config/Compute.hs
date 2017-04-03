{-# LANGUAGE RecordWildCards #-}

-- | Given a file, guess settings from it by looking at the hints.
module Config.Compute(computeSettings) where

import HSE.All
import Config.Type
import Config.Haskell
import Data.Monoid
import Prelude


-- | Given a source file, guess some hints that might apply.
--   Returns the text of the hints (if you want to save it down) along with the settings to be used.
computeSettings :: ParseFlags -> FilePath -> IO (String, [Setting])
computeSettings flags file = do
    x <- parseModuleEx flags file Nothing
    case x of
        Left (ParseError sl msg _) ->
            return ("# Parse error " ++ showSrcLoc sl ++ ": " ++ msg, [])
        Right (m, _) -> do
            let xs = concatMap (findSetting $ UnQual an) (moduleDecls m)
                r = concatMap (readSetting mempty) xs
                s = unlines $ ["# hints found in " ++ file] ++ concatMap renderSetting r ++ ["# no hints found" | null xs]
            return (s,r)

renderSetting :: Setting -> [String]
renderSetting (SettingMatchExp HintRule{..}) =
    ["- warn: {lhs: " ++ show (prettyPrint hintRuleLHS) ++ ", rhs: " ++ show (prettyPrint hintRuleRHS) ++ "}"]
renderSetting (Infix x) = ["- infix: " ++ show (prettyPrint (toInfixDecl x))]
renderSetting _ = []

findSetting :: (Name S -> QName S) -> Decl_ -> [Decl_]
findSetting qual (InstDecl _ _ _ (Just xs)) = concatMap (findSetting qual) [x | InsDecl _ x <- xs]
findSetting qual (PatBind _ (PVar _ name) (UnGuardedRhs _ bod) Nothing) = findExp (qual name) [] bod
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

findExp name vs bod = [PatBind an (toNamed "warn") (UnGuardedRhs an $ InfixApp an lhs (toNamed "==>") rhs) Nothing]
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

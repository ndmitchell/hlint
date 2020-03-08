{-# LANGUAGE RecordWildCards #-}

-- | Given a file, guess settings from it by looking at the hints.
module Config.Compute(computeSettings) where

import HSE.All
import GHC.Util
import Config.Type
import Language.Haskell.Exts.Util(isAtom, paren)
import qualified HsSyn as GHC
import qualified BasicTypes as GHC
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import SrcLoc as GHC
import Prelude


-- | Given a source file, guess some hints that might apply.
--   Returns the text of the hints (if you want to save it down) along with the settings to be used.
computeSettings :: ParseFlags -> FilePath -> IO (String, [Setting])
computeSettings flags file = do
    x <- parseModuleEx flags file Nothing
    case x of
        Left (ParseError sl msg _) ->
            pure ("# Parse error " ++ showSrcSpan' sl ++ ": " ++ msg, [])
        Right ModuleEx{hseModule=m} -> do
            let xs = concatMap findSetting (moduleDecls m)
                s = unlines $ ["# hints found in " ++ file] ++ concatMap renderSetting xs ++ ["# no hints found" | null xs]
            pure (s,xs)



renderSetting :: Setting -> [String]
renderSetting (SettingMatchExp HintRule{..}) =
    ["- warn: {lhs: " ++ show (prettyPrint hintRuleLHS) ++ ", rhs: " ++ show (prettyPrint hintRuleRHS) ++ "}"]
renderSetting (Infix x) = ["- infix: " ++ show (prettyPrint (toInfixDecl x))]
renderSetting _ = []

findSetting :: Decl_ -> [Setting]
findSetting (InstDecl _ _ _ (Just xs)) = concatMap findSetting [x | InsDecl _ x <- xs]
findSetting (PatBind _ (PVar _ name) (UnGuardedRhs _ bod) Nothing) = findExp name [] bod
findSetting (FunBind _ [InfixMatch _ p1 name ps rhs bind]) = findSetting $ FunBind an [Match an name (p1:ps) rhs bind]
findSetting (FunBind _ [Match _ name ps (UnGuardedRhs _ bod) Nothing]) = findExp name [] $ Lambda an ps bod
findSetting x@InfixDecl{} = map Infix $ getFixity x
findSetting _ = []


-- given a result function name, a list of variables, a body expression, give some hints
findExp :: Name S -> [String] -> Exp_ -> [Setting]
findExp name vs (Lambda _ ps bod) | length ps2 == length ps = findExp name (vs++ps2) bod
                                  | otherwise = []
    where ps2 = [x | PVar_ x <- map view ps]
findExp name vs Var{} = []
findExp name vs (InfixApp _ x dot y) | isDot dot = findExp name (vs++["_hlint"]) $ App an x $ Paren an $ App an y (toNamed "_hlint")

findExp name vs bod = [SettingMatchExp $
        HintRule Warning defaultHintName (fromParen lhs) (fromParen rhs) Nothing []
        -- Todo : Replace these with "proper" GHC expressions.
        mempty (extendInstances unit) (extendInstances unit) Nothing]
    where
        unit = GHC.noLoc $ GHC.ExplicitTuple GHC.noExt [] GHC.Boxed

        lhs = g $ transform f bod
        rhs = apps $ Var an (UnQual an name) : map snd rep

        rep = zip vs $ map (toNamed . pure) ['a'..]
        f xx | Var_ x <- view xx, Just y <- lookup x rep = y
        f (InfixApp _ x dol y) | isDol dol = App an x (paren y)
        f x = x

        g o@(InfixApp _ _ _ x) | isAnyApp x || isAtom x = o
        g o@App{} = o
        g o = paren o

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Given a file, guess settings from it by looking at the hints.
module Config.Compute(computeSettings) where

import HSE.All
import GHC.Util
import Config.Type
import Fixity
import Data.Generics.Uniplate.Data
import HsSyn hiding (Warning)
import RdrName
import Name
import Bag
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import SrcLoc
import Prelude


-- | Given a source file, guess some hints that might apply.
--   Returns the text of the hints (if you want to save it down) along with the settings to be used.
computeSettings :: ParseFlags -> FilePath -> IO (String, [Setting])
computeSettings flags file = do
    x <- parseModuleEx flags file Nothing
    case x of
        Left (ParseError sl msg _) ->
            pure ("# Parse error " ++ showSrcSpan' sl ++ ": " ++ msg, [])
        Right ModuleEx{ghcModule=m} -> do
            let xs = concatMap findSetting (hsmodDecls $ unLoc m)
                s = unlines $ ["# hints found in " ++ file] ++ concatMap renderSetting xs ++ ["# no hints found" | null xs]
            pure (s,xs)


renderSetting :: Setting -> [String]
-- Only need to convert the subset of Setting we generate
renderSetting (SettingMatchExp HintRule{..}) =
    ["- warn: {lhs: " ++ show (unsafePrettyPrint hintRuleLHS) ++ ", rhs: " ++ show (unsafePrettyPrint hintRuleRHS) ++ "}"]
renderSetting (Infix x) =
    ["- infix: " ++ show (unsafePrettyPrint $ toFixitySig x)]
renderSetting _ = []

findSetting :: LHsDecl GhcPs -> [Setting]
findSetting (L _ (ValD _ x)) = findBind x
findSetting (L _ (InstD _ (ClsInstD _ ClsInstDecl{cid_binds}))) =
    concatMap (findBind . unLoc) $ bagToList cid_binds
findSetting (L _ (SigD _ (FixSig _ x))) = map Infix $ fromFixitySig x
findSetting x = []


findBind :: HsBind GhcPs -> [Setting]
findBind VarBind{var_id, var_rhs} = findExp var_id [] $ unLoc var_rhs
findBind FunBind{fun_id, fun_matches} = findExp (unLoc fun_id) [] $ HsLam NoExt fun_matches
findBind _ = []

findExp :: IdP GhcPs -> [String] -> HsExpr GhcPs -> [Setting]
findExp name vs (HsLam _ MG{mg_alts=L _ [L _ Match{m_pats, m_grhss=GRHSs{grhssGRHSs=[L _ (GRHS _ [] x)], grhssLocalBinds=L _ (EmptyLocalBinds _)}}]})
    = if length m_pats == length ps then findExp name (vs++ps) $ unLoc x else []
    where ps = [occNameString $ occName $ unLoc x | XPat (L _ (VarPat _ x)) <- m_pats]
findExp name vs HsLam{} = []
findExp name vs HsVar{} = []
findExp name vs (OpApp _ x dot y) | isDot dot = findExp name (vs++["_hlint"]) $
    HsApp NoExt x $ noLoc $ HsPar NoExt $ noLoc $ HsApp NoExt y $ noLoc $ mkVar "_hlint"

findExp name vs bod = [SettingMatchExp $
        HintRule Warning defaultHintName []
        mempty (extendInstances lhs) (extendInstances $ fromParen' rhs) Nothing]
    where
        lhs = fromParen' $ noLoc $ transform f bod
        rhs = apps' $ map noLoc $ HsVar NoExt (noLoc name) : map snd rep

        rep = zip vs $ map (mkVar . pure) ['a'..]
        f (HsVar _ x) | Just y <- lookup (occNameString $ occName $ unLoc x) rep = y
        f (OpApp _ x dol y) | isDol dol = HsApp NoExt x $ noLoc $ HsPar NoExt y
        f x = x


mkVar :: String -> HsExpr GhcPs
mkVar = HsVar NoExt . noLoc . Unqual . mkVarOcc

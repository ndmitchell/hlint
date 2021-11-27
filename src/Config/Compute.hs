{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Given a file, guess settings from it by looking at the hints.
module Config.Compute(computeSettings) where

import GHC.All
import GHC.Util
import Config.Type
import Fixity
import Data.Generics.Uniplate.DataOnly
import GHC.Hs hiding (Warning)
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Data.Bag
import GHC.Types.SrcLoc
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
import Prelude


-- | Given a source file, guess some hints that might apply.
--   Returns the text of the hints (if you want to save it down) along with the settings to be used.
computeSettings :: ParseFlags -> FilePath -> IO (String, [Setting])
computeSettings flags file = do
    x <- parseModuleEx flags file Nothing
    case x of
        Left (ParseError sl msg _) ->
            pure ("# Parse error " ++ showSrcSpan sl ++ ": " ++ msg, [])
        Right ModuleEx{ghcModule=m} -> do
            let xs = concatMap findSetting (hsmodDecls $ unLoc m)
                s = unlines $ ["# hints found in " ++ file] ++ concatMap renderSetting xs ++ ["# no hints found" | null xs]
            pure (s,xs)


renderSetting :: Setting -> [String]
-- Only need to convert the subset of Setting we generate
renderSetting (SettingMatchExp HintRule{..}) =
    ["- warn: {lhs: " ++ show (unsafePrettyPrint hintRuleLHS) ++ ", rhs: " ++ show (unsafePrettyPrint hintRuleRHS) ++ "}"]
renderSetting (Infix x) =
    ["- fixity: " ++ show (unsafePrettyPrint $ toFixitySig x)]
renderSetting _ = []

findSetting :: LocatedA (HsDecl GhcPs) -> [Setting]
findSetting (L _ (ValD _ x)) = findBind x
findSetting (L _ (InstD _ (ClsInstD _ ClsInstDecl{cid_binds}))) =
    concatMap (findBind . unLoc) $ bagToList cid_binds
findSetting (L _ (SigD _ (FixSig _ x))) = map Infix $ fromFixitySig x
findSetting x = []


findBind :: HsBind GhcPs -> [Setting]
findBind VarBind{var_id, var_rhs} = findExp var_id [] $ unLoc var_rhs
findBind FunBind{fun_id, fun_matches} = findExp (unLoc fun_id) [] $ HsLam noExtField fun_matches
findBind _ = []

findExp :: IdP GhcPs -> [String] -> HsExpr GhcPs -> [Setting]
findExp name vs (HsLam _ MG{mg_alts=L _ [L _ Match{m_pats, m_grhss=GRHSs{grhssGRHSs=[L _ (GRHS _ [] x)], grhssLocalBinds=(EmptyLocalBinds _)}}]})
    = if length m_pats == length ps then findExp name (vs++ps) $ unLoc x else []
    where ps = [rdrNameStr x | L _ (VarPat _ x) <- m_pats]
findExp name vs HsLam{} = []
findExp name vs HsVar{} = []
findExp name vs (OpApp _ x dot y) | isDot dot = findExp name (vs++["_hlint"]) $
    HsApp EpAnnNotUsed x $ nlHsPar $ noLocA $ HsApp EpAnnNotUsed y $ noLocA $ mkVar "_hlint"

findExp name vs bod = [SettingMatchExp $
        HintRule Warning defaultHintName []
        mempty (extendInstances lhs) (extendInstances $ fromParen rhs) Nothing]
    where
        lhs = fromParen $ noLocA $ transform f bod
        rhs = apps $ map noLocA $ HsVar noExtField (noLocA name) : map snd rep

        rep = zip vs $ map (mkVar . pure) ['a'..]
        f (HsVar _ x) | Just y <- lookup (rdrNameStr x) rep = y
        f (OpApp _ x dol y) | isDol dol = HsApp EpAnnNotUsed x $ nlHsPar y
        f x = x


mkVar :: String -> HsExpr GhcPs
mkVar = HsVar noExtField . noLocA . Unqual . mkVarOcc

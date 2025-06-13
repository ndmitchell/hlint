module Hint.NameHelpers where

import Data.List.Extra as E
import Data.List.NonEmpty as NE
import Data.Maybe

import GHC.Types.Basic
import GHC.Types.SourceText
import GHC.Data.FastString
import GHC.Hs.Decls
import GHC.Hs.Extension
import GHC.Hs
import GHC.Types.SrcLoc

import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import GHC.Util

-- | Replace RHSs of top-level value declarations with an ellipsis
shorten :: LHsDecl GhcPs -> LHsDecl GhcPs
shorten (L locDecl (ValD ttg0 bind@(FunBind _ _ matchGroup@(MG FromSource (L locMatches matches))))) =
    L locDecl (ValD ttg0 bind {fun_matches = matchGroup {mg_alts = L locMatches $ E.map shortenMatch matches}})
shorten (L locDecl (ValD ttg0 bind@(PatBind _ _ _ grhss@(GRHSs _ rhss _)))) =
    L locDecl (ValD ttg0 bind {pat_rhs = grhss {grhssGRHSs = E.map shortenLGRHS rhss}})
shorten x = x

shortenMatch :: LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
shortenMatch (L locMatch match@(Match _ _ _ grhss@(GRHSs _ rhss _))) =
    L locMatch match {m_grhss = grhss {grhssGRHSs = E.map shortenLGRHS rhss}}

shortenLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> LGRHS GhcPs (LHsExpr GhcPs)
shortenLGRHS (L locGRHS (GRHS ttg0 guards (L locExpr _))) =
    L locGRHS (GRHS ttg0 guards (L locExpr dots))
    where
        dots :: HsExpr GhcPs
        dots = HsLit noExtField (HsString (SourceText (fsLit "...")) (fsLit "..."))

-- | Get the names from all top-level declarations including constructor names
getNames :: LHsDecl GhcPs -> [String]
getNames decl = maybeToList (declName decl) ++ getConstructorNames (unLoc decl)

getConstructorNames :: HsDecl GhcPs -> [String]
getConstructorNames tycld = case tycld of
    (TyClD _ (DataDecl _ _ _ _ (HsDataDefn _ _ _ _ (NewTypeCon con) _))) -> conNames [con]
    (TyClD _ (DataDecl _ _ _ _ (HsDataDefn _ _ _ _ (DataTypeCons _ cons) _))) -> conNames cons
    _ -> []
  where
    conNames :: [LConDecl GhcPs] -> [String]
    conNames =  concatMap (E.map unsafePrettyPrint . conNamesInDecl . unLoc)

    conNamesInDecl :: ConDecl GhcPs -> [LIdP GhcPs]
    conNamesInDecl ConDeclH98  {con_name  = name}  = [name]
    conNamesInDecl ConDeclGADT {con_names = names} = NE.toList names

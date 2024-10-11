{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-
    Detect uses of capitalisms

    Only allow up to two consecutive capital letters in identifiers.

    Identifiers containing underscores are exempted from thus rule.
    Identifiers of FFI bindings are exempted from thus rule.

<TEST>
module SSL.Foo -- ???
data LHsDecl
class FOO a where -- ???
class Foo a where getFOO -- ???
data Foo = Bar | BAAZ -- ???
data Foo = B_ar | BAAZ -- ???
data Foo = Bar | B_AAZ
data OTPToken = OTPToken -- ???
data OTP_Token = Foo 
sendSMS = ... -- ???
runTLS = ... -- ???
runTLSSocket = ... -- ???
runTLS_Socket
newtype TLSSettings = ... -- ???
tlsSettings
data CertSettings = CertSettings
tlsServerHooks
tlsServerDHEParams = ... -- ???
type WarpTLSException = () -- ???
get_SMS
runCI
foreign import ccall _FIREMISSLES :: IO ()
let getSMS = x in foo --- ???
</TEST>
-}


module Hint.NoCapitalisms(noCapitalismsHint) where

import Hint.Type (DeclHint,remark, Severity (Ignore))
import Data.List.Extra (nubOrd)
import Data.List.NonEmpty (toList)
import Data.Char
import Data.Maybe

import GHC.Types.Basic
import GHC.Types.SourceText
import GHC.Data.FastString
import GHC.Hs.Decls
import GHC.Hs.Extension
import GHC.Hs
import GHC.Types.SrcLoc

import Language.Haskell.GhclibParserEx.GHC.Hs.Decls
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import GHC.Util

noCapitalismsHint :: DeclHint
noCapitalismsHint _ _ decl = [ remark Ignore "Avoid capitalisms" (reLoc (shorten decl))
                             | not $ isForD decl
                             , name <- nubOrd $ getNames decl
                             , not $ hasUnderscore name
                             , hasCapitalism name
                             ]

hasUnderscore :: String -> Bool
hasUnderscore = elem '_'

hasCapitalism :: String -> Bool
hasCapitalism s = any isAllUpper (trigrams s)
  where
    isAllUpper = all isUpper
    trigrams = \case
      a:b:c:as -> [a,b,c] : trigrams (c:as)
      _otherwise -> []

--- these are copied from Hint.Naming ---

shorten :: LHsDecl GhcPs -> LHsDecl GhcPs
shorten (L locDecl (ValD ttg0 bind@(FunBind _ _ matchGroup@(MG FromSource (L locMatches matches))))) =
    L locDecl (ValD ttg0 bind {fun_matches = matchGroup {mg_alts = L locMatches $ map shortenMatch matches}})
shorten (L locDecl (ValD ttg0 bind@(PatBind _ _ grhss@(GRHSs _ rhss _)))) =
    L locDecl (ValD ttg0 bind {pat_rhs = grhss {grhssGRHSs = map shortenLGRHS rhss}})
shorten x = x

shortenMatch :: LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
shortenMatch (L locMatch match@(Match _ _ _ grhss@(GRHSs _ rhss _))) =
    L locMatch match {m_grhss = grhss {grhssGRHSs = map shortenLGRHS rhss}}

shortenLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> LGRHS GhcPs (LHsExpr GhcPs)
shortenLGRHS (L locGRHS (GRHS ttg0 guards (L locExpr _))) =
    L locGRHS (GRHS ttg0 guards (L locExpr dots))
    where
        dots :: HsExpr GhcPs
        dots = HsLit EpAnnNotUsed (HsString (SourceText (fsLit "...")) (fsLit "..."))

getNames :: LHsDecl GhcPs -> [String]
getNames decl = maybeToList (declName decl) ++ getConstructorNames (unLoc decl)

getConstructorNames :: HsDecl GhcPs -> [String]
getConstructorNames tycld = case tycld of
    (TyClD _ (DataDecl _ _ _ _ (HsDataDefn _ _ _ _ (NewTypeCon con) _))) -> conNames [con]
    (TyClD _ (DataDecl _ _ _ _ (HsDataDefn _ _ _ _ (DataTypeCons _ cons) _))) -> conNames cons
    _ -> []
  where
    conNames :: [LConDecl GhcPs] -> [String]
    conNames =  concatMap (map unsafePrettyPrint . conNamesInDecl . unLoc)

    conNamesInDecl :: ConDecl GhcPs -> [LIdP GhcPs]
    conNamesInDecl ConDeclH98  {con_name  = name}  = [name]
    conNamesInDecl ConDeclGADT {con_names = names} = Data.List.NonEmpty.toList names


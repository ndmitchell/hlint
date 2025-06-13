{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-
    Detect uses of capitalisms

    Do not allow two consecutive capital letters in top level
    identifiers of types, classes, values and constructors.

    Identifiers containing underscores are exempted from thus rule.
    Identifiers of FFI bindings are exempted from thus rule.

    Locally bound identifiers and module names are not checked.

<TEST>
data IO -- @Ignore
data PersonID = P -- @Ignore
sendIO :: IO () -- @Ignore
sendIO = _ -- @Ignore
class HasIO where -- @Ignore
data Foo = FO -- @Ignore
data LHsDecl -- @Ignore
class FOO a where -- @Ignore
class Foo a where getFOO :: Bool
data Foo = Bar | BAAZ -- @Ignore
data Foo = B_ar | BAAZ -- @Ignore
data Foo = Bar | B_AAZ
data OTPToken = OTPToken -- @Ignore
data OTP_Token = Foo 
sendSMS = _ -- @Ignore
runTLS = _ -- @Ignore
runTLSSocket = _ -- @Ignore
runTLS_Socket
newtype TLSSettings = TLSSettings -- @Ignore
tlsSettings
data CertSettings = CertSettings
tlsServerHooks
tlsServerDHEParams = _  -- @Ignore
type WarpTLSException = () -- @Ignore
get_SMS
runCI
foreign import ccall _FIREMISSLES :: IO ()
getSMS :: IO () -- @Ignore
gFOO = _ -- @Ignore
geFOO = _ -- @Ignore
getFOO = _ -- @Ignore
</TEST>
-}

module Hint.NoCapitalisms(noCapitalismsHint) where

import Hint.Type
import Data.List.Extra as E
import Data.List.NonEmpty as NE
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
hasCapitalism s = any isAllUpper (bigrams s)
  where
    isAllUpper = all isUpper

bigrams :: String -> [String]
bigrams = \case
  a:b:as -> [a,b] : bigrams (b:as)
  _otherwise -> []

--- these are copied from Hint.Naming ---

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


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-
    Detect uses of capitalisms

    Do not allow two consecutive capital letters in top level
    identifiers of types, classes, values and constructors.

    Identifiers containing underscores are exempted from thus rule.
    Identifiers of FFI bindings are exempted from thus rule.

    Locally bound identifiers, field names and module names are not
    checked.

<TEST>
data Foo = MkFoo { getID :: String }
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
import Hint.NameHelpers
import Data.List.Extra as E
import Data.Char

import GHC.Hs

import Language.Haskell.GhclibParserEx.GHC.Hs.Decls

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



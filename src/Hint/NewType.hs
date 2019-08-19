{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-
    Suggest newtype instead of data for type declarations that have
    only one field. Don't suggest newtype for existentially
    quantified data types because it is not valid.

<TEST>
data Foo = Foo Int -- newtype Foo = Foo Int
data Foo = Foo Int deriving (Show, Eq) -- newtype Foo = Foo Int deriving (Show, Eq)
data Foo = Foo { field :: Int } deriving Show -- newtype Foo = Foo { field :: Int } deriving Show
data Foo a b = Foo a -- newtype Foo a b = Foo a
data Foo = Foo { field1, field2 :: Int}
data S a = forall b . Show b => S b
{-# LANGUAGE RankNTypes #-}; data Foo = Foo (forall a. a) -- newtype Foo = Foo (forall a. a)
data Color a = Red a | Green a | Blue a
data Pair a b = Pair a b
data Foo = Bar
data Foo a = Eq a => MkFoo a
data Foo a = () => Foo a -- newtype Foo a = Foo a
data X = Y {-# UNPACK #-} !Int -- newtype X = Y Int
data A = A {b :: !C} -- newtype A = A {b :: C}
data A = A Int#
{-# LANGUAGE UnboxedTuples #-}; data WithAnn x = WithAnn (# Ann, x #)
{-# LANGUAGE UnboxedTuples #-}; data WithAnn x = WithAnn {getWithAnn :: (# Ann, x #)}
data A = A () -- newtype A = A ()
newtype Foo = Foo Int deriving (Show, Eq) --
newtype Foo = Foo { getFoo :: Int } deriving (Show, Eq) --
newtype Foo = Foo Int deriving stock Show
</TEST>
-}
module Hint.NewType (newtypeHint) where

import Hint.Type (Idea, DeclHint', Note(DecreasesLaziness), ideaNote, ignoreNoSuggestion', suggestN')

import Data.List (isSuffixOf)
import "ghc-lib-parser" HsDecls
import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" Outputable
import "ghc-lib-parser" SrcLoc

newtypeHint :: DeclHint'
newtypeHint _ _ x = newtypeHintDecl x ++ newTypeDerivingStrategiesHintDecl x

newtypeHintDecl :: LHsDecl GhcPs -> [Idea]
newtypeHintDecl old
    | Just WarnNewtype{newDecl, insideType} <- singleSimpleField old
    = [(suggestN' "Use newtype instead of data" old newDecl)
            {ideaNote = [DecreasesLaziness | warnBang insideType]}]
newtypeHintDecl _ = []

newTypeDerivingStrategiesHintDecl :: LHsDecl GhcPs -> [Idea]
newTypeDerivingStrategiesHintDecl decl@(dL -> L _ (TyClD _ (DataDecl _ _ _ _ dataDef))) =
    [ignoreNoSuggestion' "Use DerivingStrategies" decl | not $ isData dataDef, not $ hasAllStrategies dataDef]
newTypeDerivingStrategiesHintDecl _ = []

hasAllStrategies :: HsDataDefn GhcPs -> Bool
hasAllStrategies (HsDataDefn _ NewType _ _ _ _ (dL -> L _ xs)) = all hasStrategyClause xs
hasAllStrategies _ = False

isData :: HsDataDefn GhcPs -> Bool
isData (HsDataDefn _ NewType _ _ _ _ _) = False
isData (HsDataDefn _ DataType _ _ _ _ _) = True
isData _ = False

hasStrategyClause :: LHsDerivingClause GhcPs -> Bool
hasStrategyClause (dL -> L _ (HsDerivingClause _ (Just _) _)) = True
hasStrategyClause _ = False

data WarnNewtype = WarnNewtype
    { newDecl :: LHsDecl GhcPs
    , insideType :: HsType GhcPs
    }

-- | Given a declaration, returns the suggested \"newtype\"ized declaration following these guidelines:
-- * Types ending in a \"#\" are __ignored__, because they are usually unboxed primitives - @data X = X Int#@
-- * @ExistentialQuantification@ stuff is __ignored__ - @data X = forall t. X t@
-- * Constructors with (nonempty) constraints are __ignored__ - @data X a = (Eq a) => X a@
-- * Single field constructors get newtyped - @data X = X Int@ -> @newtype X = X Int@
-- * Single record field constructors get newtyped - @data X = X {getX :: Int}@ -> @newtype X = X {getX :: Int}@
-- * All other declarations are ignored.
singleSimpleField :: LHsDecl GhcPs -> Maybe WarnNewtype
singleSimpleField (dL -> L loc (TyClD ext decl@(DataDecl _ _ _ _ dataDef@(HsDataDefn _ DataType _ _ _ [dL -> L _ constructor] _))))
    | Just inType <- simpleCons constructor =
        Just WarnNewtype
              { newDecl = cL loc $ TyClD ext decl {tcdDataDefn = dataDef
                  { dd_ND = NewType
                  , dd_cons = map (\(dL -> L consloc x) -> cL consloc $ dropConsBang x) $ dd_cons dataDef
                  }}
              , insideType = inType
              }
singleSimpleField _ = Nothing

-- | Checks whether its argument is a \"simple constructor\" (see criteria in 'singleSimpleFieldNew')
-- returning the type inside the constructor if it is. This is needed for strictness analysis.
simpleCons :: ConDecl GhcPs -> Maybe (HsType GhcPs)
simpleCons (ConDeclH98 _ _ _ [] context (PrefixCon [dL -> L _ inType]) _)
    | emptyOrNoContext context
    , not $ isUnboxedTuple inType
    , not $ isHashy inType
    = Just inType
simpleCons (ConDeclH98 _ _ _ [] context (RecCon (dL -> L _ [dL -> L _ (ConDeclField _ [_] (dL -> L _ inType) _)])) _)
    | emptyOrNoContext context
    , not $ isUnboxedTuple inType
    , not $ isHashy inType
    = Just inType
simpleCons _ = Nothing

isHashy :: HsType GhcPs -> Bool
isHashy (HsTyVar _ _ identifier) = "#" `isSuffixOf` showSDocUnsafe (ppr identifier)
isHashy _ = False

warnBang :: HsType GhcPs -> Bool
warnBang (HsBangTy _ (HsSrcBang _ _ SrcStrict) _) = False
warnBang _ = True

emptyOrNoContext :: Maybe (LHsContext GhcPs) -> Bool
emptyOrNoContext Nothing = True
emptyOrNoContext (Just (dL -> L _ [])) = True
emptyOrNoContext _ = False

-- | The \"Bang\" here refers to 'HsSrcBang', which notably also includes @UNPACK@ pragmas!
dropConsBang :: ConDecl GhcPs -> ConDecl GhcPs
dropConsBang decl@(ConDeclH98 _ _ _ _ _ (PrefixCon fields) _) =
    decl {con_args = PrefixCon $ map getBangType fields}
dropConsBang decl@(ConDeclH98 _ _ _ _ _ (RecCon (dL -> L recloc conDeclFields)) _) =
    decl {con_args = RecCon $ cL recloc $ removeUnpacksRecords conDeclFields}
    where
        removeUnpacksRecords :: [LConDeclField GhcPs] -> [LConDeclField GhcPs]
        removeUnpacksRecords = map (\(dL -> L conDeclFieldLoc x) -> cL conDeclFieldLoc $ removeConDeclFieldUnpacks x)

        removeConDeclFieldUnpacks :: ConDeclField GhcPs -> ConDeclField GhcPs
        removeConDeclFieldUnpacks conDeclField@(ConDeclField _ _ fieldType _) =
            conDeclField {cd_fld_type = getBangType fieldType}
        removeConDeclFieldUnpacks x = x
dropConsBang x = x

isUnboxedTuple :: HsType GhcPs -> Bool
isUnboxedTuple (HsTupleTy _ HsUnboxedTuple _) = True
isUnboxedTuple _ = False

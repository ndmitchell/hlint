{-# LANGUAGE NamedFieldPuns #-}
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
{-# LANGUAGE RankNTypes #-}; data S a = forall b . Show b => S b
{-# LANGUAGE RankNTypes #-}; data Foo = Foo (forall a . a) -- newtype Foo = Foo (forall a. a)
data Color a = Red a | Green a | Blue a
data Pair a b = Pair a b
data Foo = Bar
data Foo a = Eq a => MkFoo a
data Foo a = () => Foo a -- newtype Foo a = () => Foo a
data X = Y {-# UNPACK #-} !Int -- newtype X = Y Int
data A = A {b :: !C} -- newtype A = A {b :: C}
data A = A Int#
data A = A (MutableByteArray# s)
{-# LANGUAGE UnboxedTuples #-}; data WithAnn x = WithAnn (# Ann, x #)
{-# LANGUAGE UnboxedTuples #-}; data WithAnn x = WithAnn {getWithAnn :: (# Ann, x #)}
data A = A () -- newtype A = A ()
newtype Foo = Foo Int deriving (Show, Eq) --
newtype Foo = Foo { getFoo :: Int } deriving (Show, Eq) --
newtype Foo = Foo Int deriving stock Show
data instance Foo Int = Bar Bool -- newtype instance Foo Int = Bar Bool
data instance Foo Int = Bar {field :: Bool} -- newtype instance Foo Int = Bar {field :: Bool}
data instance Foo Int = Bar {field :: Int#}
data instance Foo Int = Bar
data instance Foo Int = Bar {field1 :: Bool, field2 :: ()}
newtype instance Foo Int = Bar Bool deriving (Show, Eq) --
newtype instance Foo Int = Bar {field :: Bool} deriving Show --
newtype instance Foo Int = Bar {field :: Bool} deriving stock Show
{-# LANGUAGE RankNTypes #-}; data instance Foo Int = forall a. Show a => Foo a
</TEST>
-}
module Hint.NewType (newtypeHint) where

import Hint.Type (Idea, DeclHint, Note(DecreasesLaziness), ideaNote, ignoreNoSuggestion, suggestN)

import Data.List (isSuffixOf)
import GHC.Hs.Decls
import GHC.Hs
import GHC.Types.SrcLoc
import Data.Generics.Uniplate.Data
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

newtypeHint :: DeclHint
newtypeHint _ _ x = newtypeHintDecl x ++ newTypeDerivingStrategiesHintDecl x

newtypeHintDecl :: LHsDecl GhcPs -> [Idea]
newtypeHintDecl old
    | Just WarnNewtype{newDecl, insideType} <- singleSimpleField old
    = [(suggestN "Use newtype instead of data" (reLoc old) (reLoc newDecl))
            {ideaNote = [DecreasesLaziness | warnBang insideType]}]
newtypeHintDecl _ = []

newTypeDerivingStrategiesHintDecl :: LHsDecl GhcPs -> [Idea]
newTypeDerivingStrategiesHintDecl decl@(L _ (TyClD _ (DataDecl _ _ _ _ dataDef))) =
    [ignoreNoSuggestion "Use DerivingStrategies" (reLoc decl) | shouldSuggestStrategies dataDef]
newTypeDerivingStrategiesHintDecl decl@(L _ (InstD _ (DataFamInstD _ (DataFamInstDecl ((FamEqn _ _ _ _ _ dataDef)))))) =
    [ignoreNoSuggestion "Use DerivingStrategies" (reLoc decl) | shouldSuggestStrategies dataDef]
newTypeDerivingStrategiesHintDecl _ = []

-- | Determine if the given data definition should use deriving strategies.
shouldSuggestStrategies :: HsDataDefn GhcPs -> Bool
shouldSuggestStrategies dataDef = not (isData dataDef) && not (hasAllStrategies dataDef)

hasAllStrategies :: HsDataDefn GhcPs -> Bool
hasAllStrategies (HsDataDefn _ NewType _ _ _ _  xs) = all hasStrategyClause xs
hasAllStrategies _ = False

isData :: HsDataDefn GhcPs -> Bool
isData (HsDataDefn _ NewType _ _ _ _ _) = False
isData (HsDataDefn _ DataType _ _ _ _ _) = True

hasStrategyClause :: LHsDerivingClause GhcPs -> Bool
hasStrategyClause (L _ (HsDerivingClause _ (Just _) _)) = True
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
singleSimpleField (L loc (TyClD ext decl@(DataDecl _ _ _ _ dataDef)))
    | Just inType <- simpleHsDataDefn dataDef =
        Just WarnNewtype
              { newDecl = L loc $ TyClD ext decl {tcdDataDefn = dataDef
                  { dd_ND = NewType
                  , dd_cons = dropBangs dataDef
                  }}
              , insideType = inType
              }
singleSimpleField (L loc (InstD ext (DataFamInstD instExt (DataFamInstDecl famEqn@(FamEqn _ _ _ _ _ dataDef)))))
    | Just inType <- simpleHsDataDefn dataDef =
        Just WarnNewtype
          { newDecl = L loc $ InstD ext $ DataFamInstD instExt $ DataFamInstDecl $ famEqn {feqn_rhs = dataDef
                  { dd_ND = NewType
                  , dd_cons = dropBangs dataDef
                  }}
              , insideType = inType
              }
singleSimpleField _ = Nothing

dropBangs :: HsDataDefn GhcPs -> [LConDecl GhcPs]
dropBangs = map (fmap dropConsBang) . dd_cons

-- | Checks whether its argument is a \"simple\" data definition (see 'singleSimpleField')
-- returning the type inside its constructor if it is.
simpleHsDataDefn :: HsDataDefn GhcPs -> Maybe (HsType GhcPs)
simpleHsDataDefn (HsDataDefn _ DataType _ _ _ [L _ constructor] _) = simpleCons constructor
simpleHsDataDefn _ = Nothing

-- | Checks whether its argument is a \"simple\" constructor (see criteria in 'singleSimpleField')
-- returning the type inside the constructor if it is. This is needed for strictness analysis.
simpleCons :: ConDecl GhcPs -> Maybe (HsType GhcPs)
simpleCons (ConDeclH98 _ _ _ [] context (PrefixCon [] [HsScaled _ (L _ inType)]) _)
    | emptyOrNoContext context
    , not $ isUnboxedTuple inType
    , not $ isHashy inType
    = Just inType
simpleCons (ConDeclH98 _ _ _ [] context (RecCon (L _ [L _ (ConDeclField _ [_] (L _ inType) _)])) _)
    | emptyOrNoContext context
    , not $ isUnboxedTuple inType
    , not $ isHashy inType
    = Just inType
simpleCons _ = Nothing

isHashy :: HsType GhcPs -> Bool
isHashy x = or ["#" `isSuffixOf` unsafePrettyPrint v | v@HsTyVar{} <- universe x]

warnBang :: HsType GhcPs -> Bool
warnBang (HsBangTy _ (HsSrcBang _ _ SrcStrict) _) = False
warnBang _ = True

emptyOrNoContext :: Maybe (LHsContext GhcPs) -> Bool
emptyOrNoContext Nothing = True
emptyOrNoContext (Just (L _ [])) = True
emptyOrNoContext _ = False

-- | The \"Bang\" here refers to 'HsSrcBang', which notably also includes @UNPACK@ pragmas!
dropConsBang :: ConDecl GhcPs -> ConDecl GhcPs
-- fields [HsScaled GhcPs (LBangType GhcPs)]
dropConsBang decl@(ConDeclH98 _ _ _ _ _ (PrefixCon [] fields) _) =
    -- decl {con_args = PrefixCon $ map getBangType fields}
    let fs' = map (\(HsScaled s lt) -> HsScaled s (getBangType lt)) fields  :: [HsScaled GhcPs (LBangType GhcPs)]
    in decl {con_args = PrefixCon [] fs'}
dropConsBang decl@(ConDeclH98 _ _ _ _ _ (RecCon (L recloc conDeclFields)) _) =
    decl {con_args = RecCon $ L recloc $ removeUnpacksRecords conDeclFields}
    where
        removeUnpacksRecords :: [LConDeclField GhcPs] -> [LConDeclField GhcPs]
        removeUnpacksRecords = map (\(L conDeclFieldLoc x) -> L conDeclFieldLoc $ removeConDeclFieldUnpacks x)

        removeConDeclFieldUnpacks :: ConDeclField GhcPs -> ConDeclField GhcPs
        removeConDeclFieldUnpacks conDeclField@(ConDeclField _ _ fieldType _) =
            conDeclField {cd_fld_type = getBangType fieldType}
dropConsBang x = x

isUnboxedTuple :: HsType GhcPs -> Bool
isUnboxedTuple (HsTupleTy _ HsUnboxedTuple _) = True
isUnboxedTuple _ = False

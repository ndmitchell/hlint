{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-
    Suggest newtype instead of data for type declarations that have
    only one field. Don't suggest newtype for existentially
    quantified data types because it is not valid.

<TEST>
data Foo = Foo Int -- newtype Foo = Foo Int
data Foo where Foo :: Int -> Foo -- newtype Foo where Foo :: Int -> Foo
data Foo = Foo Int deriving (Show, Eq) -- newtype Foo = Foo Int deriving (Show, Eq)
data Foo = Foo { field :: Int } deriving Show -- newtype Foo = Foo { field :: Int } deriving Show
data Foo a b = Foo a -- newtype Foo a b = Foo a
data Foo = Foo { field1, field2 :: Int}
data S a = forall b . Show b => S b
data Color a = Red a | Green a | Blue a
data Pair a b = Pair a b
data Pair a b where Pair :: a -> b -> Pair a b
data Foo = Bar
data Foo a = Eq a => MkFoo a
data Foo a = () => Foo a -- newtype Foo a = Foo a
data X = Y {-# UNPACK #-} !Int -- newtype X = Y Int
data A = A {b :: !C} -- newtype A = A {b :: C}
data A = A Int#
{-# LANGUAGE UnboxedTuples #-}; data WithAnn x = WithAnn (# Ann, x #)
data A = A () -- newtype A = A ()
newtype Foo = Foo Int deriving (Show, Eq) --
newtype Foo = Foo { getFoo :: Int } deriving (Show, Eq) --
newtype Foo = Foo Int deriving stock Show
</TEST>
-}
module Hint.NewType (newtypeHint) where

import Hint.Type
-- TODO: remove these qualifieds
import qualified "ghc-lib-parser" HsDecls as Hs
import qualified "ghc-lib-parser" HsSyn   as Hs
import qualified "ghc-lib-parser" HsTypes as Hs
import qualified "ghc-lib-parser" SrcLoc  as Hs

-- TODO: get deriving strategies to work
newtypeHint :: DeclHint'
newtypeHint _ _ x = newtypeHintDecl x -- ++ newTypeDerivingStrategiesHintDecl x

newtypeHintDecl :: Hs.LHsDecl Hs.GhcPs -> [Idea]
newtypeHintDecl old
    | Just WarnNewtype{newDecl, insideType} <- singleSimpleFieldNew old
    = [(suggestN' "Use newtype instead of data" old newDecl)
            {ideaNote = [DecreasesLaziness | warnBang insideType]}]
newtypeHintDecl _ = []


singleSimpleField :: Decl_ -> Maybe (DataOrNew S, Type_, DataOrNew S -> Type_ -> Decl_)
singleSimpleField (DataDecl x1 dt x2 x3 [QualConDecl y1 Nothing Nothing ctor] x4)
    | Just (t, ft) <- f ctor = Just (dt, t, \dt t -> DataDecl x1 dt x2 x3 [QualConDecl y1 Nothing Nothing $ ft t] x4)
    where
        f (ConDecl x1 x2 [t]) | not $ isKindHash t = Just (t, \t -> ConDecl x1 x2 [t])
        f (RecDecl x1 x2 [FieldDecl y1 [y2] t]) = Just (t, \t -> RecDecl x1 x2 [FieldDecl y1 [y2] t])
        f _ = Nothing
singleSimpleField _ = Nothing

-- TODO: implement with ghc-lib-parser
newTypeDerivingStrategiesHintDecl :: Decl_ -> [Idea]
newTypeDerivingStrategiesHintDecl x =
    [rawIdeaN Ignore "Use DerivingStrategies" (srcInfoSpan $ ann x) (prettyPrint x) Nothing [] | lacksStrategy x]

lacksStrategy :: Decl_ -> Bool
lacksStrategy (DataDecl _ (NewType _) _ _ _ derivingClause)
    = any hasNoStrategy derivingClause
lacksStrategy (GDataDecl _ (NewType _) _ _ _ _ derivingClause)
    = any hasNoStrategy derivingClause
lacksStrategy _ = False

hasNoStrategy :: Deriving a -> Bool
hasNoStrategy (Deriving _ Nothing _) = True
hasNoStrategy _                      = False

withNewtype :: Deriving a -> Deriving a
withNewtype (Deriving l Nothing rs)  = Deriving l (Just $ DerivNewtype l) rs
withNewtype d                        = d

data WarnNewtype = WarnNewtype
    { newDecl :: Hs.LHsDecl Hs.GhcPs
    , insideType :: Hs.HsType Hs.GhcPs
    }

-- | Given a declaration, returns the suggested \"newtype\"ized declaration following these guidelines:
-- * @MagicHash@'d stuff is __ignored__ - @data X = X Int#@
-- * @ExistentialQuantification@ stuff is __ignored__ - @data X = forall t. X t@
-- * Constructors with (nonempty) constraints are __ignored__ - @data X a = (Eq a) => X a@
-- * Single field constructors get newtyped - @data X = X Int@ -> @newtype X = X Int@
-- * Single record field constructors get newtyped - @data X = X {getX :: Int}@ -> @newtype X = X {getX :: Int}@
-- * All other declarations are ignored.
singleSimpleFieldNew :: Hs.LHsDecl Hs.GhcPs -> Maybe WarnNewtype
singleSimpleFieldNew (Hs.L loc (Hs.TyClD ext decl@(Hs.DataDecl _ name _ _ def@(Hs.HsDataDefn _ Hs.DataType ctx _ _ [Hs.L _ constructor] derives))))
    | Just inType <- simpleCons constructor =
        Just WarnNewtype
              { newDecl = Hs.L loc $ Hs.TyClD ext decl {Hs.tcdDataDefn = def
                  { Hs.dd_ND = Hs.NewType
                  , Hs.dd_cons = map (\(Hs.L consloc x) -> Hs.L consloc $ dropConsBang x) $ Hs.dd_cons def
                  }}
              , insideType = inType
              }
singleSimpleFieldNew _ = Nothing

-- TODO: check for MAGIC#HASH
-- TODO: get tests to pass
--
-- | Checks whether its argument is a \"simple constructor\" (see criteria in 'singleSimpleFieldNew')
-- returning the type inside the constructor if it is. This is needed for bang/MagicHash analysis.
simpleCons :: Hs.ConDecl Hs.GhcPs -> Maybe (Hs.HsType Hs.GhcPs)
simpleCons (Hs.ConDeclH98 _ _ _ [] context (Hs.PrefixCon [Hs.L _ inType]) _)
    | emptyOrNoContext context = Just inType
simpleCons (Hs.ConDeclH98 _ _ _ [] context (Hs.RecCon (Hs.L _ [Hs.L _ (Hs.ConDeclField _ [_] (Hs.L _ inType) _)])) _)
    | emptyOrNoContext context = Just inType
simpleCons _ = Nothing

warnBang :: Hs.HsType Hs.GhcPs -> Bool
warnBang (Hs.HsBangTy _ (Hs.HsSrcBang _ _ Hs.SrcStrict) _) = False
warnBang _ = True

emptyOrNoContext :: Maybe (Hs.LHsContext Hs.GhcPs) -> Bool
emptyOrNoContext Nothing = True
emptyOrNoContext (Just (Hs.L _ [])) = True
emptyOrNoContext _ = False

-- | The \"Bang\" here refers to 'HsSrcBang', which notably also include @UNPACK@ pragmas!
dropConsBang :: Hs.ConDecl Hs.GhcPs -> Hs.ConDecl Hs.GhcPs
dropConsBang decl@(Hs.ConDeclH98 _ _ _ _ _ (Hs.PrefixCon fields) _) =
    decl {Hs.con_args = Hs.PrefixCon $ map Hs.getBangType fields}
dropConsBang decl@(Hs.ConDeclH98 _ _ _ _ _ (Hs.RecCon (Hs.L recloc conDeclFields)) _) =
    decl {Hs.con_args = Hs.RecCon $ Hs.L recloc $ removeUnpacksRecords conDeclFields}
    where
        removeUnpacksRecords :: [Hs.LConDeclField Hs.GhcPs] -> [Hs.LConDeclField Hs.GhcPs]
        removeUnpacksRecords = map (\(Hs.L conDeclFieldLoc x) -> Hs.L conDeclFieldLoc $ removeConDeclFieldUnpacks x)

        removeConDeclFieldUnpacks :: Hs.ConDeclField Hs.GhcPs -> Hs.ConDeclField Hs.GhcPs
        removeConDeclFieldUnpacks decl@(Hs.ConDeclField _ _ fieldType _) =
            decl {Hs.cd_fld_type = Hs.getBangType fieldType}
        removeConDeclFieldUnpacks x = x
dropConsBang x = x

{-# LANGUAGE PackageImports #-}
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
data Color a = Red a | Green a | Blue a
data Pair a b = Pair a b
data Foo = Bar
data Foo a = Eq a => MkFoo a
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
    | Just new <- singleSimpleFieldNew old
    = [(suggestN' "Use newtype instead of data" old new)]
            --{ideaNote = [DecreasesLaziness | not $ isTyBang t]}]
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

-- | Given a declaration, returns the suggested \"newtype\"ized declaration following these guidelines:
-- * @MagicHash@'d stuff is __ignored__ - @data X = X Int#@
-- * @ExistentialQuantification@ stuff is __ignored__ - @data X = forall t. X t@
-- * Single field constructors get newtyped - @data X = X Int@ -> @newtype X = X Int@
-- * Single record field constructors get newtyped - @data X = X {getX :: Int}@ -> @newtype X = X {getX :: Int}@
-- * All other declarations are ignored.
--
-- TODO: insert ! warning (or lack thereof) somewhere
singleSimpleFieldNew :: Hs.LHsDecl Hs.GhcPs -> Maybe (Hs.LHsDecl Hs.GhcPs)
singleSimpleFieldNew (Hs.L loc (Hs.TyClD ext decl@(Hs.DataDecl _ name _ _ def@(Hs.HsDataDefn _ Hs.DataType ctx _ _ [Hs.L _ constructor] derives))))
    | simpleCons constructor = Just $ Hs.L loc $ Hs.TyClD ext decl {Hs.tcdDataDefn = def {Hs.dd_ND = Hs.NewType}}
singleSimpleFieldNew _ = Nothing

-- TODO: check for MAGIC#HASH
-- TODO: get tests to pass
--
-- TODO: eventually check GADTs too
simpleCons :: Hs.ConDecl Hs.GhcPs -> Bool
simpleCons (Hs.ConDeclH98 _ _ _ [] _ (Hs.PrefixCon [_]) _) = True
simpleCons (Hs.ConDeclH98 _ _ _ [] _ (Hs.RecCon (Hs.L _ [Hs.L _ (Hs.ConDeclField _ [_] _ _)])) _) = True 
                                     -- TODO: ^ why is there a list in RecCon, if the actualy fields are inside the ConDeclField???
simpleCons _ = False

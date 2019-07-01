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

newtypeHint :: DeclHint
newtypeHint _ _ x = newtypeHintDecl x ++ newTypeDerivingStrategiesHintDecl x

newtypeHintDecl :: Decl_ -> [Idea]
newtypeHintDecl x
    | Just (DataType s, t, f) <- singleSimpleField x
    = [(suggestN "Use newtype instead of data" x $ f (NewType s) $ fromTyBang t)
            {ideaNote = [DecreasesLaziness | not $ isTyBang t]}]
newtypeHintDecl _ = []


singleSimpleField :: Decl_ -> Maybe (DataOrNew S, Type_, DataOrNew S -> Type_ -> Decl_)
singleSimpleField (DataDecl x1 dt x2 x3 [QualConDecl y1 Nothing Nothing ctor] x4)
    | Just (t, ft) <- f ctor = Just (dt, t, \dt t -> DataDecl x1 dt x2 x3 [QualConDecl y1 Nothing Nothing $ ft t] x4)
    where
        f (ConDecl x1 x2 [t]) | not $ isKindHash t = Just (t, \t -> ConDecl x1 x2 [t])
        f (RecDecl x1 x2 [FieldDecl y1 [y2] t]) = Just (t, \t -> RecDecl x1 x2 [FieldDecl y1 [y2] t])
        f _ = Nothing
singleSimpleField _ = Nothing

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

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
</TEST>
-}
module Hint.NewType (newtypeHint) where

import Hint.Type

newtypeHint :: DeclHint
newtypeHint _ _ = newtypeHintDecl

newtypeHintDecl :: Decl_ -> [Idea]
newtypeHintDecl d@(DataDecl sp (DataType dtA) ctx dclH
                   [qcD@(QualConDecl _ tvb _ cd)] der) =
  case tvb of
    Just _  -> []
    Nothing -> case cd of
                 ConDecl _ _ [_] -> wrn
                 RecDecl _ _ [FieldDecl _ [_] _] -> wrn
                 _ -> []
  where suggestion = DataDecl sp (NewType dtA) ctx dclH [qcD] der
        wrn = [(suggestN "Use newtype instead of data" d suggestion){ideaNote = [DecreasesLaziness]}]
newtypeHintDecl _ = []

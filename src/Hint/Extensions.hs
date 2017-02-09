{-
    Suggest removal of unnecessary extensions
    i.e. They have {-# LANGUAGE RecursiveDo #-} but no mdo keywords
<TEST>
{-# LANGUAGE Arrows #-} \
f = id --
{-# LANGUAGE TotallyUnknown #-} \
f = id
{-# LANGUAGE Foo, ParallelListComp, ImplicitParams #-} \
f = [(a,c) | a <- b | c <- d] -- {-# LANGUAGE Foo, ParallelListComp #-}
{-# LANGUAGE EmptyDataDecls #-} \
data Foo
{-# LANGUAGE TemplateHaskell #-} \
$(deriveNewtypes typeInfo)
{-# LANGUAGE TemplateHaskell #-} \
main = foo ''Bar
{-# LANGUAGE PatternGuards #-} \
test = case x of _ | y <- z -> w
{-# LANGUAGE TemplateHaskell,EmptyDataDecls #-} \
$(fmap return $ dataD (return []) (mkName "Void") [] [] [])
{-# LANGUAGE RecursiveDo #-} \
main = mdo x <- y; return y
{-# LANGUAGE RecursiveDo #-} \
main = do {rec {x <- return 1}; print x}
{-# LANGUAGE ImplicitParams, BangPatterns #-} \
sort :: (?cmp :: a -> a -> Bool) => [a] -> [a] \
sort !f = undefined
{-# LANGUAGE KindSignatures #-} \
data Set (cxt :: * -> *) a = Set [a]
{-# LANGUAGE RecordWildCards #-} \
record field = Record{..}
{-# LANGUAGE RecordWildCards #-} \
record = 1 --
{-# LANGUAGE UnboxedTuples #-} \
record = 1 --
{-# LANGUAGE TemplateHaskell #-} \
foo
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} \
record = 1 --
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} \
newtype Foo = Foo Int deriving Data -- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} \
data Foo = Foo Int deriving Data -- {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} \
newtype Foo = Foo Int deriving Class -- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-} \
data Foo = Foo Int deriving Class --
{-# LANGUAGE DeriveFunctor #-} \
data Foo = Foo Int deriving Functor
{-# LANGUAGE DeriveFunctor #-} \
newtype Foo = Foo Int deriving Functor
{-# LANGUAGE GeneralizedNewtypeDeriving #-} \
newtype Foo = Foo Int deriving Functor
{-# LANGUAGE GeneralizedNewtypeDeriving #-} \
newtype Foo = Foo Int deriving Data --
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-} \
deriving instance Functor Bar
{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, StandaloneDeriving #-} \
deriving instance Show Bar -- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-} \
newtype Micro = Micro Int deriving Generic -- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} \
instance Class Int where {newtype MyIO a = MyIO a deriving NewClass}
{-# LANGUAGE UnboxedTuples #-} \
f :: Int -> (# Int, Int #)
{-# LANGUAGE UnboxedTuples #-} \
f :: x -> (x, x); f x = (x, x) --
{-# LANGUAGE DefaultSignatures #-} \
class Val a where; val :: a --
{-# LANGUAGE DefaultSignatures #-} \
class Val a where; val :: a; default val :: Int
{-# LANGUAGE TypeApplications #-} \
foo = id --
{-# LANGUAGE TypeApplications #-} \
foo = id @Int
{-# LANGUAGE LambdaCase #-} \
foo = id --
{-# LANGUAGE LambdaCase #-} \
foo = \case () -> ()
</TEST>
-}


module Hint.Extensions(extensionsHint) where

import Hint.Type
import Data.Maybe
import Data.List.Extra
import Refact.Types


extensionsHint :: ModuHint
extensionsHint _ x = [rawIdea Warning "Unused LANGUAGE pragma" (toSrcSpan sl)
          (prettyPrint o) (Just newPragma)
          (warnings old new) [refact]
    | not $ used TemplateHaskell x -- if TH is on, can use all other extensions programmatically
    , o@(LanguagePragma sl exts) <- modulePragmas x
    , let old = map (parseExtension . prettyPrint) exts
    , let new = minimalExtensions x old
    , let newPragma = if null new then "" else prettyPrint $ LanguagePragma sl $ map (toNamed . prettyExtension) new
    , let refact = ModifyComment (toSS o) newPragma
    , sort new /= sort old]


minimalExtensions :: Module_ -> [Extension] -> [Extension]
minimalExtensions x es = nub $ concatMap f es
    where f e = [e | usedExt e x]


-- RecordWildCards implies DisambiguateRecordFields, but most people probably don't want it
warnings old new | wildcards `elem` old && wildcards `notElem` new = [Note "you may need to add DisambiguateRecordFields"]
    where wildcards = EnableExtension RecordWildCards
warnings _ _ = []


-- | Classes that don't work with newtype deriving
noNewtypeDeriving :: [String]
noNewtypeDeriving = ["Read","Show","Data","Typeable","Generic","Generic1"]


usedExt :: Extension -> Module_ -> Bool
usedExt (EnableExtension x) = used x
usedExt _ = const True


used :: KnownExtension -> Module_ -> Bool
used RecursiveDo = hasS isMDo & hasS isRecStmt
used ParallelListComp = hasS isParComp
used FunctionalDependencies = hasT (un :: FunDep S)
used ImplicitParams = hasT (un :: IPName S)
used TypeApplications = hasS isTypeApp
used EmptyDataDecls = hasS f
    where f (DataDecl _ _ _ _ [] _) = True
          f (GDataDecl _ _ _ _ _ [] _) = True
          f _ = False
used KindSignatures = hasT (un :: Kind S)
used BangPatterns = hasS isPBangPat
used TemplateHaskell = hasT2 (un :: (Bracket S, Splice S)) & hasS f & hasS isSpliceDecl
    where f VarQuote{} = True
          f TypQuote{} = True
          f _ = False
used ForeignFunctionInterface = hasT (un :: CallConv S)
used PatternGuards = hasS f
    where f (GuardedRhs _ xs _) = g xs
          g [] = False
          g [Qualifier{}] = False
          g _ = True
used StandaloneDeriving = hasS isDerivDecl
used PatternSignatures = hasS isPatTypeSig
used RecordWildCards = hasS isPFieldWildcard & hasS isFieldWildcard
used RecordPuns = hasS isPFieldPun & hasS isFieldPun
used UnboxedTuples = has (not . isBoxed)
used PackageImports = hasS (isJust . importPkg)
used QuasiQuotes = hasS isQuasiQuote & hasS isTyQuasiQuote
used ViewPatterns = hasS isPViewPat
used DefaultSignatures = hasS isClsDefSig
used DeriveDataTypeable = hasDerive ["Data","Typeable"]
used DeriveFunctor = hasDerive ["Functor"]
used DeriveFoldable = hasDerive ["Foldable"]
used DeriveTraversable = hasDerive ["Traversable"]
used DeriveGeneric = hasDerive ["Generic","Generic1"]
used GeneralizedNewtypeDeriving = any (`notElem` noNewtypeDeriving) . fst . derives
used LambdaCase = hasS isLCase
used Arrows = hasS f
    where f Proc{} = True
          f LeftArrApp{} = True
          f RightArrApp{} = True
          f LeftArrHighApp{} = True
          f RightArrHighApp{} = True
          f _ = False
used TransformListComp = hasS f
    where f QualStmt{} = False
          f _ = True

-- for forwards compatibility, if things ever get added to the extension enumeration
used x = usedExt $ UnknownExtension $ show x


hasDerive :: [String] -> Module_ -> Bool
hasDerive want m = any (`elem` want) $ new ++ dat
    where (new,dat) = derives m


-- | What is derived on newtype, and on data type
--   'deriving' declarations may be on either, so we approximate as both newtype and data
derives :: Module_ -> ([String],[String])
derives m = concatUnzip $ map decl (childrenBi m) ++ map idecl (childrenBi m)
    where
        idecl :: InstDecl S -> ([String], [String])
        idecl (InsData _ dn _ _ ds) = g dn ds
        idecl (InsGData _ dn _ _ _ ds) = g dn ds
        idecl _ = ([], [])

        decl :: Decl_ -> ([String], [String])
        decl (DataDecl _ dn _ _ _ ds) = g dn ds
        decl (GDataDecl _ dn _ _ _ _ ds) = g dn ds
        decl (DataInsDecl _ dn _ _ ds) = g dn ds
        decl (GDataInsDecl _ dn _ _ _ ds) = g dn ds
        decl (DerivDecl _ _ hd) = (xs, xs) -- don't know whether this was on newtype or not
            where xs = [ir hd]
        decl _ = ([], [])

        g dn ds = if isNewType dn then (xs,[]) else ([],xs)
            where xs = maybe [] (map ir . fromDeriving) ds

        ir (IRule _ _ _ x) = ih x
        ir (IParen _ x) = ir x

        ih (IHCon _ a) = prettyPrint $ unqual a
        ih (IHInfix _ _ a) = prettyPrint $ unqual a
        ih (IHParen _ a) = ih a
        ih (IHApp _ a _) = ih a

un = undefined

(&) f g x = f x || g x

hasT t x = not $ null (universeBi x `asTypeOf` [t])
hasT2 ~(t1,t2) = hasT t1 & hasT t2

hasS :: Biplate x (f S) => (f S -> Bool) -> x -> Bool
hasS test = any test . universeBi

has f = any f . universeBi


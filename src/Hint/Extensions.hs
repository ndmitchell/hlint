{-
    Suggest removal of unnecessary extensions
    i.e. They have {-# LANGUAGE RecursiveDo #-} but no mdo keywords
<TEST>
{-# LANGUAGE Arrows #-} \
f = id --
{-# LANGUAGE Foo, Generics, ParallelListComp, ImplicitParams #-} \
f = [(a,c) | a <- b | c <- d] -- {-# LANGUAGE Foo, ParallelListComp #-}
{-# LANGUAGE EmptyDataDecls #-} \
data Foo
{-# LANGUAGE TemplateHaskell #-} \
$(deriveNewtypes typeInfo)
{-# LANGUAGE TemplateHaskell #-} \
main = foo ''Bar
</TEST>
-}


module Hint.Extensions where

import HSE.All
import Type
import Data.List
import Data.Maybe
import Data.Function


extensionsHint :: ModuHint
extensionsHint _ x = [rawIdea Error "Unused LANGUAGE pragma" sl
          (prettyPrint o) (if null new then "" else prettyPrint $ LanguagePragma sl new)
    | o@(LanguagePragma sl old) <- modulePragmas x
    , let new = filter (flip used x . classifyExtension . prettyPrint) old
    , length new /= length old]


used :: Extension -> Module -> Bool
used RecursiveDo = has isMDo
used ParallelListComp = has isParComp
used FunctionalDependencies = has isFunDep
used ImplicitParams = hasT (un :: IPName)
used EmptyDataDecls = has f
    where f (DataDecl _ _ _ _ _ [] _) = True
          f (GDataDecl _ _ _ _ _ _ [] _) = True
          f _ = False
used KindSignatures = hasT (un :: Kind)
used BangPatterns = has isPBangPat
used TemplateHaskell = hasT2 (un :: (Bracket,Splice)) & has f
    where f VarQuote{} = True
          f TypQuote{} = True
          f _ = False
used ForeignFunctionInterface = hasT (un :: CallConv)
used Generics = has isPExplTypeArg
used PatternGuards = has f
    where f (GuardedRhs _ [] _) = False
          f (GuardedRhs _ [Qualifier _] _) = False
          f _ = True
used StandaloneDeriving = has isDerivDecl
used PatternSignatures = has isPatTypeSig
used RecordWildCards = has isPFieldWildcard
used RecordPuns = has isPFieldPun
used UnboxedTuples = has isBoxed
used PackageImports = has (isJust . importPkg)
used QuasiQuotes = has isQuasiQuote
used ViewPatterns = has isPViewPat
used Arrows = has f
    where f Proc{} = True
          f LeftArrApp{} = True
          f RightArrApp{} = True
          f LeftArrHighApp{} = True
          f RightArrHighApp{} = True
          f _ = False
used TransformListComp = has f
    where f QualStmt{} = False
          f _ = True

used _ = const True



un = undefined

(&) f g x = f x || g x

hasT t x = not $ null (universeBi x `asTypeOf` [t])
hasT2 ~(t1,t2) = hasT t1 & hasT t2
hasT3 ~(t1,t2,t3) = hasT t1 & hasT t2 & hasT t3

has f = any f . universeBi


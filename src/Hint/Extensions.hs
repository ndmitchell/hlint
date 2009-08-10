{-
    Suggest removal of unnecessary extensions
    i.e. They have {-# LANGUAGE RecursiveDo #-} but no mdo keywords
<TEST>
</TEST>
-}


module Hint.Extensions where

import HSE.All
import Type
import Data.List
import Data.Maybe
import Data.Function


extensionsHint :: ModuHint
extensionsHint _ x = [idea Error "Unused LANGUAGE pragma" sl o (LanguagePragma sl new)
    | o@(LanguagePragma sl old) <- modulePragmas x
    , let new = filter (flip used x . classifyExtension . prettyPrint) old
    , length new /= length old]


used :: Extension -> Module -> Bool
used RecursiveDo x = any isMDo $ universeBi x
used ParallelListComp x = any isParComp $ universeBi x
used MultiParamTypeClasses x = any f $ universeBi x
    where f (ClassA _ xs) = length xs > 1
          f InfixA{} = True
          f _ = False
used FunctionalDependencies x = any isFunDep $ universeBi x
used ImplicitParams x = not $ null (universeBi x :: [IPName])
used EmptyDataDecls x = any f $ universeBi x
    where f (DataDecl _ _ _ _ _ [] _) = True
          f (GDataDecl _ _ _ _ _ _ [] _) = True
          f _ = False
used KindSignatures x = not $ null (universeBi x :: [Kind])
used BangPatterns x = any isPBangPat $ universeBi x
used TemplateHaskell x = not (null (universeBi x :: [Bracket])) && not (null (universeBi x :: [Splice]))
used ForeignFunctionInterface x = not $ null (universeBi x :: [CallConv])
used Generics x = any isPExplTypeArg $ universeBi x
used PatternGuards x = any f $ universeBi x
    where f (GuardedRhs _ xs _) = length xs > 1
          f _ = False
used StandaloneDeriving x = any isDerivDecl $ universeBi x
used PatternSignatures x = any isPatTypeSig $ universeBi x
used RecordWildCards x = any isPFieldWildcard $ universeBi x
used RecordPuns x = any isPFieldPun $ universeBi x
used UnboxedTuples x = any isBoxed $ universeBi x
used PackageImports x = any (isJust . importPkg) $ universeBi x
used QuasiQuotes x = any isQuasiQuote $ universeBi x
used ViewPatterns x = any isPViewPat $ universeBi x
used Arrows x = any f $ universeBi x
    where f Proc{} = True
          f LeftArrApp{} = True
          f RightArrApp{} = True
          f LeftArrHighApp{} = True
          f RightArrHighApp{} = True
used TransformListComp x = any f $ universeBi x
    where f QualStmt{} = False
          f _ = True

used _ _ = True

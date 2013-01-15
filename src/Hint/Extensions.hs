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
{-# LANGUAGE PatternGuards #-} \
test = case x of _ | y <- z -> w
{-# LANGUAGE TemplateHaskell,EmptyDataDecls #-} \
$(fmap return $ dataD (return []) (mkName "Void") [] [] [])
{-# LANGUAGE RecursiveDo #-} \
main = mdo x <- y; return y
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
</TEST>
-}


module Hint.Extensions where

import Hint.Type
import Data.Maybe
import Data.List
import Util


extensionsHint :: ModuHint
extensionsHint _ x = [rawIdea Error "Unused LANGUAGE pragma" (toSrcLoc sl)
          (prettyPrint o) (if null new then "" else prettyPrint $ LanguagePragma sl $ map (toNamed . showExt) new)
          (warnings old new)
    | not $ used TemplateHaskell x -- if TH is on, can use all other extensions programmatically
    , o@(LanguagePragma sl exts) <- modulePragmas x
    , let old = map (classifyExtension . prettyPrint) exts
    , let new = minimalExtensions x old
    , sort new /= sort old]
    where
        showExt (UnknownExtension x) = x
        showExt x = show x


minimalExtensions :: Module_ -> [Extension] -> [Extension]
minimalExtensions x es = nub $ concatMap f es
    where f e = [e | used e x]


-- RecordWildCards implies DisambiguateRecordFields, but most people probably don't want it
warnings old new | RecordWildCards `elem` old && RecordWildCards `notElem` new = [Note "you may need to add DisambiguateRecordFields"]
warnings _ _ = []


used :: Extension -> Module_ -> Bool
used RecursiveDo = hasS isMDo
used ParallelListComp = hasS isParComp
used FunctionalDependencies = hasT (un :: FunDep S)
used ImplicitParams = hasT (un :: IPName S)
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
used Generics = hasS isPExplTypeArg
used PatternGuards = hasS f1 & hasS f2
    where f1 (GuardedRhs _ xs _) = g xs
          f2 (GuardedAlt _ xs _) = g xs
          g [] = False
          g [Qualifier{}] = False
          g _ = True
used StandaloneDeriving = hasS isDerivDecl
used PatternSignatures = hasS isPatTypeSig
used RecordWildCards = hasS isPFieldWildcard & hasS isFieldWildcard
used RecordPuns = hasS isPFieldPun & hasS isFieldPun
used UnboxedTuples = has isBoxed
used PackageImports = hasS (isJust . importPkg)
used QuasiQuotes = hasS isQuasiQuote
used ViewPatterns = hasS isPViewPat
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

used _ = const True



un = undefined

(&) f g x = f x || g x

hasT t x = notNull (universeBi x `asTypeOf` [t])
hasT2 ~(t1,t2) = hasT t1 & hasT t2

hasS :: Biplate x (f S) => (f S -> Bool) -> x -> Bool
hasS test = any test . universeBi

has f = any f . universeBi


{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Hint.Smell (
  smellModuleHint,
  smellHint
  ) where

{-
<TEST> [{smell: { type: many arg functions, limit: 2 }}]
f :: Int -> Int \
f = undefined

f :: Int -> Int -> Int \
f = undefined --
</TEST>

<TEST>
f :: Int -> Int \
f = undefined

f :: Int -> Int -> Int \
f = undefined
</TEST>

<TEST> [{smell: { type: long functions, limit: 3}}]
f = do \
 x <- y \
 return x --

f = do \
  return z \
\
  where \
   z = do \
    a \
    b --

f = do \
  return z \
\
  where \
   z = a

f = Con \
  { a = x \
  , b = y \
  , c = z \
  }

f = return x
</TEST>

<TEST>
f = do \
 x <- y \
 return x

f = return x
</TEST>

<TEST> [{smell: { type: long type lists, limit: 2}}]
f :: Bool -> Int -> (Int -> Proxy '[a, b]) --
f :: Proxy '[a]
</TEST>

<TEST>
f :: Proxy '[a, b]
f :: Proxy '[a]
</TEST>

<TEST> [{smell: { type: many imports, limit: 2}}]
import A; import B --
import A
</TEST>

<TEST>
import A; import B
import A
</TEST>
-}

import Hint.Type
import Config.Type
import Data.List.Extra
import qualified Data.Map as Map

import "ghc-lib-parser" BasicTypes
import qualified "ghc-lib-parser" HsSyn as GHC
import "ghc-lib-parser" HsDecls
import "ghc-lib-parser" HsImpExp
import "ghc-lib-parser" HsTypes
import "ghc-lib-parser" HsExtension
import "ghc-lib-parser" RdrName
import "ghc-lib-parser" Outputable
import "ghc-lib-parser" Bag
import qualified "ghc-lib-parser" SrcLoc as GHC
import GHC.Util

smellModuleHint :: [Setting] -> ModuHint
smellModuleHint settings scope m =
  let (dL -> GHC.L _ mod) = ghcModule m
      imports = GHC.hsmodImports mod in
  case Map.lookup SmellManyImports (smells settings) of
    Just n | length imports >= n ->
             let span = foldl1 GHC.combineSrcSpans $ getloc <$> imports
                 displayImports = unlines $ f <$> imports
             in [rawIdea Config.Type.Warning "Many imports" (ghcSpanToHSE span) displayImports  Nothing [] [] ]
      where
        f :: LImportDecl GhcPs -> String
        f = trimStart . unsafePrettyPrint
    _ -> []

smellHint :: [Setting] -> DeclHint'
smellHint settings scope m d =
  sniff smellLongFunctions SmellLongFunctions ++
  sniff smellLongTypeLists SmellLongTypeLists ++
  sniff smellManyArgFunctions SmellManyArgFunctions
  where
    sniff f t = fmap (\i -> i {ideaTo = Nothing }) . take 1 $ maybe [] (f d) $ Map.lookup t (smells settings)

smellLongFunctions :: LHsDecl GhcPs -> Int -> [Idea]
smellLongFunctions d n = [ idea
                         | (span, idea) <- declSpans d
                         , spanLength span >= n
                         ]

-- I've tried to be faithful to the original here but I'm doubtful
-- about it. I think I've replicated the behavior of the original but
-- is the original correctly honoring the intent?

-- A function with with one alternative, one rhs and its 'where'
-- clause (perhaps we should be looping over alts and all guarded
-- right hand sides?)
declSpans :: LHsDecl GhcPs -> [(GHC.SrcSpan, Idea)]
declSpans
   (dL -> GHC.L _ (ValD _
     GHC.FunBind {GHC.fun_matches=GHC.MG {
                  GHC.mg_alts=(dL -> GHC.L _ [dL -> GHC.L _ GHC.Match {
                       GHC.m_ctxt=ctx
                     , GHC.m_grhss=GHC.GRHSs{GHC.grhssGRHSs=[locGrhs]
                                 , GHC.grhssLocalBinds=where_}}])}})) =
 -- The span of the right hand side and the spans of each binding in
 -- the where clause.
 rhsSpans ctx locGrhs ++ whereSpans where_
-- Any other kind of function.
declSpans f@(dL -> GHC.L l (ValD _ GHC.FunBind {})) = [(l, warn' "Long function" f f [])]
declSpans _ = []

-- The span of a guarded right hand side.
rhsSpans :: GHC.HsMatchContext RdrName -> GHC.LGRHS GhcPs (GHC.LHsExpr GhcPs) -> [(GHC.SrcSpan, Idea)]
rhsSpans _ (dL -> GHC.L _ (GHC.GRHS _ _ (dL -> GHC.L _ GHC.RecordCon {}))) = [] -- record constructors get a pass
rhsSpans ctx (dL -> GHC.L _ r@(GHC.GRHS _ _ (GHC.L l _))) =
  [(l, rawIdea' Config.Type.Warning "Long function" l (showSDocUnsafe (GHC.pprGRHS ctx r)) Nothing [] [])]
rhsSpans _ _ = []

-- The spans of a 'where' clause are the spans of its bindings.
whereSpans :: GHC.LHsLocalBinds GhcPs -> [(GHC.SrcSpan, Idea)]
whereSpans (dL -> GHC.L l (GHC.HsValBinds _ (GHC.ValBinds _ bs _))) =
  concatMap (declSpans . (\(dL -> GHC.L loc bind) -> cL loc (ValD noext bind))) (bagToList bs)
whereSpans _ = []

spanLength :: GHC.SrcSpan -> Int
spanLength (GHC.RealSrcSpan span) = GHC.srcSpanEndLine span - GHC.srcSpanStartLine span + 1
spanLength (GHC.UnhelpfulSpan _) = -1

smellLongTypeLists :: LHsDecl GhcPs -> Int -> [Idea]
smellLongTypeLists d@(dL -> GHC.L _ (SigD _ (GHC.TypeSig _ _ (HsWC _ (HsIB _ (dL -> GHC.L _ t)))))) n =
  warn' "Long type list" d d [] <$ filter longTypeList (universe t)
  where
    longTypeList (HsExplicitListTy _ IsPromoted x) = length x >= n
    longTypeList _ = False
smellLongTypeLists _ _ = []

smellManyArgFunctions :: LHsDecl GhcPs -> Int -> [Idea]
smellManyArgFunctions d@(dL -> GHC.L _ (SigD _ (GHC.TypeSig _ _ (HsWC _ (HsIB _ (dL -> GHC.L _ t)))))) n =
  warn' "Many arg function" d d [] <$  filter manyArgFunction (universe t)
  where
    manyArgFunction t = countFunctionArgs t >= n
smellManyArgFunctions _ _ = []

countFunctionArgs :: HsType GhcPs -> Int
countFunctionArgs (HsFunTy _ _ t) = 1 + countFunctionArgs (unloc t)
countFunctionArgs (HsParTy _ t) = countFunctionArgs (unloc t)
countFunctionArgs _ = 0

smells :: [Setting] -> Map.Map SmellType Int
smells settings = Map.fromList [ (smellType, smellLimit) | SettingSmell smellType smellLimit  <- settings]


module Hint.Smell (smellModuleHint,smellHint) where

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

import Hint.Type(ModuHint,ModuleEx(..),DeclHint,Idea(..),rawIdea,warn)
import Config.Type

import Data.Generics.Uniplate.DataOnly
import Data.List.Extra
import qualified Data.Map as Map

import GHC.Utils.Outputable
import GHC.Types.Basic
import GHC.Hs
import GHC.Data.Bag
import GHC.Types.SrcLoc
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

smellModuleHint :: [Setting] -> ModuHint
smellModuleHint settings scope m =
  let (L _ mod) = ghcModule m
      imports = hsmodImports mod in
  case Map.lookup SmellManyImports (smells settings) of
    Just n | length imports >= n ->
             let span = foldl1 combineSrcSpans $ locA . getLoc <$> imports
                 displayImports = unlines $ f <$> imports
             in [rawIdea Config.Type.Warning "Many imports" span displayImports  Nothing [] [] ]
      where
        f :: LImportDecl GhcPs -> String
        f = trimStart . unsafePrettyPrint
    _ -> []

smellHint :: [Setting] -> DeclHint
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
declSpans :: LHsDecl GhcPs -> [(SrcSpan, Idea)]
declSpans
   (L _ (ValD _
     FunBind {fun_matches=MG {
                   mg_origin=FromSource
                 , mg_alts=(L _ [L _ Match {
                       m_ctxt=ctx
                     , m_grhss=GRHSs{grhssGRHSs=[locGrhs]
                                 , grhssLocalBinds=where_}}])}})) =
 -- The span of the right hand side and the spans of each binding in
 -- the where clause.
 rhsSpans ctx locGrhs ++ whereSpans where_
-- Any other kind of function.
declSpans f@(L l (ValD _ FunBind {})) = [(locA l, warn "Long function" (reLoc f) (reLoc f) [])]
declSpans _ = []

-- The span of a guarded right hand side.
rhsSpans :: HsMatchContext GhcPs -> LGRHS GhcPs (LHsExpr GhcPs) -> [(SrcSpan, Idea)]
rhsSpans _ (L _ (GRHS _ _ (L _ RecordCon {}))) = [] -- record constructors get a pass
rhsSpans ctx (L _ r@(GRHS _ _ (L l _))) =
  [(locA l, rawIdea Config.Type.Warning "Long function" (locA l) (showSDocUnsafe (pprGRHS ctx r)) Nothing [] [])]

-- The spans of a 'where' clause are the spans of its bindings.
whereSpans :: HsLocalBinds GhcPs -> [(SrcSpan, Idea)]
whereSpans (HsValBinds _ (ValBinds _ bs _)) =
  concatMap (declSpans . (\(L loc bind) -> L loc (ValD noExtField bind))) (bagToList bs)
whereSpans _ = []

spanLength :: SrcSpan -> Int
spanLength (RealSrcSpan span _) = srcSpanEndLine span - srcSpanStartLine span + 1
spanLength (UnhelpfulSpan _) = -1

smellLongTypeLists :: LHsDecl GhcPs -> Int -> [Idea]
smellLongTypeLists d@(L _ (SigD _ (TypeSig _ _ (HsWC _ (L _ (HsSig _ _ (L _ t))))))) n =
  warn "Long type list" (reLoc d) (reLoc d) [] <$ filter longTypeList (universe t)
  where
    longTypeList (HsExplicitListTy _ IsPromoted x) = length x >= n
    longTypeList _ = False
smellLongTypeLists _ _ = []

smellManyArgFunctions :: LHsDecl GhcPs -> Int -> [Idea]
smellManyArgFunctions d@(L _ (SigD _ (TypeSig _ _ (HsWC _ (L _ (HsSig _ _ (L _ t))))))) n =
  warn "Many arg function" (reLoc d) (reLoc d) [] <$  filter manyArgFunction (universe t)
  where
    manyArgFunction t = countFunctionArgs t >= n
smellManyArgFunctions _ _ = []

countFunctionArgs :: HsType GhcPs -> Int
countFunctionArgs (HsFunTy _ _ _ t) = 1 + countFunctionArgs (unLoc t)
countFunctionArgs (HsParTy _ t) = countFunctionArgs (unLoc t)
countFunctionArgs _ = 0

smells :: [Setting] -> Map.Map SmellType Int
smells settings = Map.fromList [ (smellType, smellLimit) | SettingSmell smellType smellLimit  <- settings]

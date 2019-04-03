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

smellModuleHint :: [Setting] -> ModuHint
smellModuleHint settings scope m@(Module _ _ _ imports _) = case Map.lookup SmellManyImports (smells settings) of
  Just n | length imports >= n ->
           let span = foldl1 mergeSrcSpan $ srcInfoSpan . ann <$> imports
               displayImports = unlines $ f <$> imports
           in [rawIdea Warning "Many imports" span displayImports  Nothing [] [] ]
    where
      f = trimStart . prettyPrint
  _ -> []

smellHint :: [Setting] -> DeclHint
smellHint settings scope m d =
  sniff smellLongFunctions SmellLongFunctions ++
  sniff smellLongTypeLists SmellLongTypeLists ++
  sniff smellManyArgFunctions SmellManyArgFunctions
  where
    sniff f t = fmap (\i -> i {ideaTo = Nothing }) . take 1 $ maybe [] (f d) $ Map.lookup t (smells settings)

smellLongFunctions :: Decl_ -> Int -> [Idea]
smellLongFunctions d n = [ idea
                         | (span, idea) <- declSpans d
                         , spanLength span >= n
                         ]

declSpans :: Decl_ -> [(SrcSpanInfo, Idea)]
declSpans (FunBind _ [Match _ _ _ rhs where_]) = rhsSpans rhs ++ whereSpans where_
declSpans f@(FunBind  l match)         = [(l, warn "Long function" f f [])] -- count where clauses
declSpans (PatBind _ _ rhs where_) = rhsSpans rhs ++ whereSpans where_
declSpans _                          = []

whereSpans :: Maybe (Binds SrcSpanInfo) ->  [(SrcSpanInfo, Idea)]
whereSpans Nothing = []
whereSpans (Just (BDecls _ decls)) = concatMap declSpans decls

rhsSpans :: Rhs SrcSpanInfo -> [(SrcSpanInfo, Idea)]
rhsSpans (UnGuardedRhs l RecConstr{}) = [] --- record constructors get a pass
rhsSpans r@(UnGuardedRhs l _) = [(l, warn "Long function" r r [])]
rhsSpans r@(GuardedRhss l _) = [(l, warn "Long function" r r [])]

spanLength :: SrcSpanInfo -> Int
spanLength (SrcSpanInfo span _) = srcSpanEndLine span - srcSpanStartLine span + 1

smellLongTypeLists :: Decl_ -> Int -> [Idea]
smellLongTypeLists d@(TypeSig _ _ t) n = warn "Long type list" d d [] <$ filter longTypeList (universe t)
  where
    longTypeList (TyPromoted _ (PromotedList _ _ x)) = length x >= n
    longTypeList _ = False
smellLongTypeLists _ _ = []

smellManyArgFunctions :: Decl_ -> Int -> [Idea]
smellManyArgFunctions d@(TypeSig _ _ t) n = warn "Many arg function" d d [] <$  filter manyArgFunction (universe t)
  where
    manyArgFunction x = countFunctionArgs x >= n
smellManyArgFunctions _ _ = []

countFunctionArgs :: Type l -> Int
countFunctionArgs (TyFun _ _ b) = 1 + countFunctionArgs b
countFunctionArgs (TyParen _ t) = countFunctionArgs t
countFunctionArgs _ = 0

smells :: [Setting] -> Map.Map SmellType Int
smells settings = Map.fromList [ (smellType, smellLimit) | SettingSmell smellType smellLimit  <- settings]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Hint.Smell where

import Hint.Type
import Config.Type
import Control.Monad
import Data.List.Extra
import Data.Maybe
import qualified Data.Map as Map

smellModuleHint :: [Setting] -> ModuHint
smellModuleHint settings scope m@(Module _ _ _ imports _) = case Map.lookup SmellManyImports (smells settings) of
  Just n | length imports >= n ->
           let span = srcInfoSpan $ ann $ head imports
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
smellLongFunctions d n
  | Just length <- spanLength <$> declBind d
  , length >= n
  = [warn "Long function" d d []]
smellLongFunctions _ _ = []

declBind :: Decl l -> Maybe l
declBind (FunBind  l match)         = Just l
declBind (PatBind l (PVar _ n) _ _) = Just l
declBind _                          = Nothing

spanLength :: SrcSpanInfo -> Int
spanLength (SrcSpanInfo span _) = srcSpanEndLine span - srcSpanStartLine span + 1

smellLongTypeLists :: Decl_ -> Int -> [Idea]
smellLongTypeLists d@(TypeSig _ _ t) n = warn "Long type list" d d [] <$ filter longTypeList (unrollType t)
  where
    longTypeList (TyPromoted _ (PromotedList _ _ x)) = length x >= n
    longTypeList _ = False
smellLongTypeLists _ _ = []

smellManyArgFunctions :: Decl_ -> Int -> [Idea]
smellManyArgFunctions d@(TypeSig _ _ t) n = warn "Many arg function" d d [] <$  filter manyArgFunction (unrollType t)
  where
    manyArgFunction x = countFunctionArgs x >= n
smellManyArgFunctions _ _ = []

countFunctionArgs :: Type l -> Int
countFunctionArgs (TyFun _ _ b) = 1 + countFunctionArgs b
countFunctionArgs (TyParen _ t) = countFunctionArgs t
countFunctionArgs _ = 0

subTypes :: Type l -> [Type l]
subTypes (TyForall _ _ _ t) = [t]
subTypes (TyFun _ a b) = [a, b]
subTypes (TyTuple _ _ x) = x
subTypes (TyUnboxedSum _ x) = x
subTypes (TyList _ a) = [a]
subTypes (TyParArray _ a) = [a]
subTypes (TyApp _ a b) = [a, b]
subTypes (TyParen _ a) = [a]
subTypes (TyInfix _ a _ b) = [a, b]
subTypes (TyKind _ a _) = [a]
subTypes (TyPromoted _ (PromotedList _ _ x)) = x
subTypes (TyPromoted _ (PromotedTuple _ x)) = x
subTypes (TyEquals _ a b) = [a, b]
subTypes (TyBang _ _ _ a) = [a]
subTypes _ = []

unrollType :: Type l -> [Type l]
unrollType t = t : (subTypes t >>= unrollType)

smells :: [Setting] -> Map.Map SmellType Int
smells settings = Map.fromList [ (smellType, smellLimit) | SettingSmell Smell{..} <- settings]

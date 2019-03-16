{-# LANGUAGE RecordWildCards #-}
module Hint.Smell where

import Hint.Type
import Config.Type
import Data.Maybe
import qualified Data.Map as Map

smellHint :: [Setting] -> DeclHint
smellHint settings scope m d = sniff smellLongFunctions SmellLongFunctions
  where
    sniff f t = maybe [] (f d) $ Map.lookup t (smells settings)

smellLongFunctions :: Decl_ -> Int -> [Idea]
smellLongFunctions d n
  | Just length <- spanLength <$> declBind d
  , length >= n
  = [(warn "Long function" d d []) { ideaTo = Nothing}]
smellLongFunctions _ _ = []


declBind :: Decl l -> Maybe l
declBind (FunBind  l match)         = Just l
declBind (PatBind l (PVar _ n) _ _) = Just l
declBind _                          = Nothing

spanLength :: SrcSpanInfo -> Int
spanLength (SrcSpanInfo span _) = srcSpanEndLine span - srcSpanStartLine span + 1

smells :: [Setting] -> Map.Map SmellType Int
smells settings = Map.fromList [ (smellType, smellLimit) | SettingSmell Smell{..} <- settings]

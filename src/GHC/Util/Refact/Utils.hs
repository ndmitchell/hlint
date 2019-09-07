-- Adapted from https://github.com/mpickering/apply-refact.git.

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module GHC.Util.Refact.Utils ( -- * Synonyms
                      Module
                    , Stmt
                    , Expr
                    , Decl
                    , Name
                    , Pat
                    , Type
                    , Import
                    , FunBind
                    -- * Monad
                    , M
                    -- * Utility

                    , mergeAnns
                    , modifyAnnKey
                    , replaceAnnKey
                    , toGhcSrcSpan
                    , findParent

                    ) where

import "ghc-lib-parser" HsSyn as GHC hiding (Stmt, Pat)
import "ghc-lib-parser" SrcLoc
import qualified "ghc-lib-parser" SrcLoc as GHC
import qualified "ghc-lib-parser" RdrName as GHC
import qualified "ghc-lib-parser" ApiAnnotation as GHC
import qualified "ghc-lib-parser" FastString    as GHC

import GHC.Util.Language.Haskell.GHC.ExactPrint.Types

import Data.Data

import Control.Monad.State

import qualified Data.Map as Map
import Data.Maybe


import qualified Refact.Types as R

import Data.Generics.Schemes
import Unsafe.Coerce

-- | Left bias pair union
mergeAnns :: Anns -> Anns -> Anns
mergeAnns
  = Map.union

-- Types
--
type M a = State Anns a

type Module = (GHC.Located (GHC.HsModule GHC.GhcPs))

type Expr = GHC.Located (GHC.HsExpr GHC.GhcPs)

type Type = GHC.Located (GHC.HsType GHC.GhcPs)

type Decl = GHC.Located (GHC.HsDecl GHC.GhcPs)

type Pat = GHC.LPat GHC.GhcPs

type Name = GHC.Located GHC.RdrName

type Stmt = ExprLStmt GHC.GhcPs

type Import = LImportDecl GHC.GhcPs

type FunBind = HsMatchContext GHC.RdrName

-- | Replaces an old expression with a new expression
--
-- Note that usually, new, inp and parent are all the same.
replace :: AnnKey  -- The thing we are replacing
        -> AnnKey  -- The thing which has the annotations we need for the new thing
        -> AnnKey  -- The thing which is going to be inserted
        -> AnnKey  -- The "parent", the largest thing which has he same SrcSpan
                   -- Usually the same as inp and new
        -> Anns -> Maybe Anns
replace old new inp parent anns = do
  oldan <- Map.lookup old anns
  newan <- Map.lookup new anns
  oldDelta <- annEntryDelta  <$> Map.lookup parent anns
  return $ Map.insert inp (combine oldDelta oldan newan) anns

combine :: DeltaPos -> Annotation -> Annotation -> Annotation
combine oldDelta oldann newann =
  Ann { annEntryDelta = newEntryDelta
      , annPriorComments = annPriorComments oldann ++ annPriorComments newann
      , annFollowingComments = annFollowingComments oldann ++ annFollowingComments newann
      , annsDP = removeComma (annsDP newann) ++ extraComma (annsDP oldann)
      , annSortKey = annSortKey newann
      , annCapturedSpan = annCapturedSpan newann}
  where
    -- Get rid of structural information when replacing, we assume that the
    -- structural information is already there in the new expression.
    removeComma = filter (\(kw, _) -> case kw of
                                         G GHC.AnnComma -> False
                                         AnnSemiSep -> False
                                         _ -> True)

    -- Make sure to keep structural information in the template.
    extraComma [] = []
    extraComma (last -> x) = case fst x of
                              G GHC.AnnComma -> [x]
                              AnnSemiSep -> [x]
                              G GHC.AnnSemi -> [x]
                              _ -> []

    -- Keep the same delta if moving onto a new row
    newEntryDelta | deltaRow oldDelta > 0 = oldDelta
                  | otherwise = annEntryDelta oldann


-- | A parent in this case is an element which has the same SrcSpan
findParent :: Data a => GHC.SrcSpan -> Anns -> a -> Maybe AnnKey
findParent ss as = something (findParentWorker ss as)

-- Note that a parent must also have an annotation.
findParentWorker :: forall a . (Data a)
           => GHC.SrcSpan -> Anns -> a -> Maybe AnnKey
findParentWorker oldSS as a
  | con == typeRepTyCon (typeRep (Proxy :: Proxy (GHC.Located GHC.RdrName))) && x == typeRep (Proxy :: Proxy GHC.SrcSpan)
      = if ss == oldSS
            && isJust (Map.lookup (AnnKey ss cn) as)
          then Just $ AnnKey ss cn
          else Nothing
  | otherwise = Nothing
  where
    (con, ~[x, _]) = splitTyConApp (typeOf a)
    ss :: GHC.SrcSpan
    ss = gmapQi 0 unsafeCoerce a
    cn = gmapQi 1 (CN . show . toConstr) a


-- | Perform the necessary adjustments to annotations when replacing
-- one Located thing with another Located thing.
--
-- For example, this function will ensure the correct relative position and
-- make sure that any trailing semi colons or commas are transferred.
modifyAnnKey :: (Data old, Data new, Data mod) => mod -> Located old -> Located new -> M (Located new)
modifyAnnKey m e1 e2 = do
    as <- get
    let parentKey = fromMaybe (mkAnnKey e2) (findParent (getLoc e2) as m)
    e2 <$ modify (\m' -> replaceAnnKey m' (mkAnnKey e1) (mkAnnKey e2) (mkAnnKey e2) parentKey)


-- | Lower level version of @modifyAnnKey@
replaceAnnKey ::
  Anns -> AnnKey -> AnnKey -> AnnKey -> AnnKey -> Anns
replaceAnnKey a old new inp deltainfo =
  fromMaybe a (replace old new inp deltainfo a)


-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan :: FilePath -> R.SrcSpan -> SrcSpan
toGhcSrcSpan file R.SrcSpan{..} = mkSrcSpan (f startLine startCol) (f endLine endCol)
  where
    f = mkSrcLoc (GHC.mkFastString file)

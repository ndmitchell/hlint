{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

module GHC.FreeVars
  (
  ) where

import "ghc-lib-parser" RdrName
import "ghc-lib-parser" OccName
import "ghc-lib-parser" Name
import "ghc-lib-parser" HsSyn
import "ghc-lib-parser" HsPat
import "ghc-lib-parser" HsExtension
import "ghc-lib-parser" SrcLoc

import Control.Monad
import Data.Data
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Set                      (Set)
import qualified Data.Set as Set

( ^+ ) :: Set OccName -> Set OccName -> Set OccName
( ^+ ) = Set.union
( ^- ) :: Set OccName -> Set OccName -> Set OccName
( ^- ) = Set.difference

data Vars' = Vars' {bound' :: Set OccName, free' :: Set OccName}

instance Semigroup Vars' where
    Vars' x1 x2 <> Vars' y1 y2 = Vars' (x1 ^+ y1) (x2 ^+ y2)

instance Monoid Vars' where
    mempty = Vars' Set.empty Set.empty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif
    mconcat fvs = Vars' (Set.unions $ map bound' fvs) (Set.unions $ map free' fvs)

class AllVars' a where
    -- | Return the variables, erring on the side of more free
    -- variables.
    allVars' :: a -> Vars'

class FreeVars' a where
    -- | Return the variables, erring on the side of more free
    -- variables.
    freeVars' :: a -> Set OccName

freeVars_ :: (FreeVars' a) => a -> Vars'
freeVars_ = Vars' Set.empty . freeVars'

-- `inFree' a b` is the set of free variables in 'a' together with the
-- free variables in 'b' not bound in 'a'.
inFree' :: (AllVars' a, FreeVars' b) => a -> b -> Set OccName
inFree' a b = free' aa ^+ (freeVars' b ^- bound' aa)
    where aa = allVars' a

-- `inVars' a b` is a value of `Vars_'` with bound variables the union
-- of the bound variables of 'a' and 'b' and free variables the union
-- of the free variables of 'a' and the free variables of 'b' not
-- bound by 'a'.
inVars' :: (AllVars' a, AllVars' b) => a -> b -> Vars'
inVars' a b =
  Vars' (bound' aa ^+ bound' bb) (free' aa ^+ (free' bb ^- bound' aa))
    where aa = allVars' a
          bb = allVars' b

unqualNames' :: Located RdrName -> [OccName]
unqualNames' (L _ (Unqual x)) = [x]
unqualNames' (L _ (Exact x)) = [nameOccName x]
unqualNames' _ = []

instance FreeVars' (Set OccName) where
  freeVars' = id

instance AllVars' Vars'  where
  allVars' = id

instance FreeVars' (LHsExpr GhcPs) where -- never has any bound variables
  freeVars' (L _ (HsVar _ x)) = Set.fromList $ unqualNames' x
  freeVars' (L _ (RecordCon _ _ (HsRecFields flds _))) = Set.unions $ map freeVars' flds
  freeVars' (L _ (RecordUpd _ e flds)) = Set.unions $ freeVars' e : map freeVars' flds
  -- More to do.
  freeVars' x = freeVars' $ children x

instance FreeVars' [LHsExpr GhcPs] where
  freeVars' = Set.unions . map freeVars'

instance FreeVars' (LHsRecField GhcPs (LHsExpr GhcPs)) where
   freeVars' (L _ (HsRecField _ x _)) = freeVars' x

instance FreeVars' (LHsRecUpdField GhcPs) where
  freeVars' (L _ (HsRecField _ x _)) = freeVars' x

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Util.FreeVars (
    vars', varss', pvars', freeVarSet'
  , Vars' (..), FreeVars'(..) , AllVars' (..)
  ) where

import RdrName
import HsTypes
import OccName
import Name
import HsSyn
import SrcLoc
import Bag (bagToList)

import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Operations
import Data.Monoid
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude

( ^+ ) :: Set OccName -> Set OccName -> Set OccName
( ^+ ) = Set.union
( ^- ) :: Set OccName -> Set OccName -> Set OccName
( ^- ) = Set.difference

-- See [Note : Spack leaks lurking here?] below.
data Vars' = Vars'{bound' :: Set OccName, free' :: Set OccName}

-- Useful for debugging.
instance Show Vars' where
  show (Vars' bs fs) = "bound : " ++
    show (map occNameString (Set.toList bs)) ++
    ", free : " ++ show (map occNameString (Set.toList fs))

instance Semigroup Vars' where
    Vars' x1 x2 <> Vars' y1 y2 = Vars' (x1 ^+ y1) (x2 ^+ y2)

instance Monoid Vars' where
    mempty = Vars' Set.empty Set.empty
    mconcat vs = Vars' (Set.unions $ map bound' vs) (Set.unions $ map free' vs)

-- A type `a` is a model of `AllVars' a` if exists a function
-- `allVars'` for producing a pair of the bound and free varaiable
-- sets in a value of `a`.
class AllVars' a where
    -- | Return the variables, erring on the side of more free
    -- variables.
    allVars' :: a -> Vars'

-- A type `a` is a model of `FreeVars' a` if exists a function
-- `freeVars'` for producing a set of free varaiable of a value of
-- `a`.
class FreeVars' a where
    -- | Return the variables, erring on the side of more free
    -- variables.
    freeVars' :: a -> Set OccName

-- Trivial instances.
instance AllVars' Vars'  where allVars' = id
instance FreeVars' (Set OccName) where freeVars' = id
-- [Note : Space leaks lurking here?]
-- ==================================
-- We make use of `foldr`. @cocreature suggests we want bangs on `data
-- Vars` and replace usages of `mconcat` with `foldl'`.
instance (AllVars' a) => AllVars' [a] where  allVars' = mconcat . map allVars'
instance (FreeVars' a) => FreeVars' [a] where  freeVars' = Set.unions . map freeVars'

-- Construct a `Vars` value with no bound vars.
freeVars_' :: (FreeVars' a) => a -> Vars'
freeVars_' = Vars' Set.empty . freeVars'

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

-- Get an `OccName` out of a reader name.
unqualNames' :: Located RdrName -> [OccName]
unqualNames' (dL -> L _ (Unqual x)) = [x]
unqualNames' (dL -> L _ (Exact x)) = [nameOccName x]
unqualNames' _ = []

instance FreeVars' (LHsExpr GhcPs) where
  freeVars' (dL -> L _ (HsVar _ x)) = Set.fromList $ unqualNames' x -- Variable.
  freeVars' (dL -> L _ (HsUnboundVar _ x)) = Set.fromList [unboundVarOcc x] -- Unbound variable; also used for "holes".
  freeVars' (dL -> L _ (HsLam _ mg)) = free' (allVars' mg) -- Lambda abstraction. Currently always a single match.
  freeVars' (dL -> L _ (HsLamCase _ mg)) = free' (allVars' mg) -- Lambda-case.
  freeVars' (dL -> L _ (HsCase _ of_ MG{mg_alts=(dL -> L _ ms)})) = freeVars' of_ ^+ free' (allVars' ms) -- Case expr.
  freeVars' (dL -> L _ (HsLet _ binds e)) = inFree' binds e -- Let (rec).
  freeVars' (dL -> L _ (HsDo _ ctxt (dL -> L _ stmts))) = free' (allVars' stmts) -- Do block.
  freeVars' (dL -> L _ (RecordCon _ _ (HsRecFields flds _))) = Set.unions $ map freeVars' flds -- Record construction.
  freeVars' (dL -> L _ (RecordUpd _ e flds)) = Set.unions $ freeVars' e : map freeVars' flds -- Record update.
  freeVars' (dL -> L _ (HsMultiIf _ grhss)) = free' (allVars' grhss) -- Multi-way if.

  freeVars' (dL -> L _ HsConLikeOut{}) = mempty -- After typechecker.
  freeVars' (dL -> L _ HsRecFld{}) = mempty -- Variable pointing to a record selector.
  freeVars' (dL -> L _ HsOverLabel{}) = mempty -- Overloaded label. The id of the in-scope 'fromLabel'.
  freeVars' (dL -> L _ HsIPVar{}) = mempty -- Implicit parameter.
  freeVars' (dL -> L _ HsOverLit{}) = mempty -- Overloaded literal.
  freeVars' (dL -> L _ HsLit{}) = mempty -- Simple literal.
  freeVars' (dL -> L _ HsRnBracketOut{}) = mempty -- Renamer produces these.
  freeVars' (dL -> L _ HsTcBracketOut{}) = mempty -- Typechecker produces these.
  freeVars' (dL -> L _ HsWrap{}) = mempty -- Typechecker output.

  -- freeVars' (dL -> e@(L _ HsAppType{})) = freeVars' $ children e -- Visible type application e.g. 'f @ Int x y'.
  -- freeVars' (dL -> e@(L _ HsApp{})) = freeVars' $ children e -- Application.
  -- freeVars' (dL -> e@(L _ OpApp{})) = freeVars' $ children e -- Operator application.
  -- freeVars' (dL -> e@(L _ NegApp{})) = freeVars' $ children e -- Negation operator.
  -- freeVars' (dL -> e@(L _ HsPar{})) = freeVars' $ children e -- Parenthesized expr.
  -- freeVars' (dL -> e@(L _ SectionL{})) = freeVars' $ children e -- Left section.
  -- freeVars' (dL -> e@(L _ SectionR{})) = freeVars' $ children e -- Right section.
  -- freeVars' (dL -> e@(L _ ExplicitTuple{})) = freeVars' $ children e -- Explicit tuple and sections thereof.
  -- freeVars' (dL -> e@(L _ ExplicitSum{})) = freeVars' $ children e -- Used for unboxed sum types.
  -- freeVars' (dL -> e@(L _ HsIf{})) = freeVars' $ children e -- If.
  -- freeVars' (dL -> e@(L _ ExplicitList{})) = freeVars' $ children e -- Syntactic list e.g. '[a, b, c]'.
  -- freeVars' (dL -> e@(L _ ExprWithTySig{})) = freeVars' $ children e -- Expr with type signature.
  -- freeVars' (dL -> e@(L _ ArithSeq {})) = freeVars' $ children e -- Arithmetic sequence.
  -- freeVars' (dL -> e@(L _ HsSCC{})) = freeVars' $ children e -- Set cost center pragma (expr whose const is to be measured).
  -- freeVars' (dL -> e@(L _ HsCoreAnn{})) = freeVars' $ children e -- Pragma.
  -- freeVars' (dL -> e@(L _ HsBracket{})) = freeVars' $ children e -- Haskell bracket.
  -- freeVars' (dL -> e@(L _ HsSpliceE{})) = freeVars' $ children e -- Template haskell splice expr.
  -- freeVars' (dL -> e@(L _ HsProc{})) = freeVars' $ children e -- Proc notation for arrows.
  -- freeVars' (dL -> e@(L _ HsStatic{})) = freeVars' $ children e -- Static pointers extension.
  -- freeVars' (dL -> e@(L _ HsArrApp{})) = freeVars' $ children e -- Arrow tail or arrow application.
  -- freeVars' (dL -> e@(L _ HsArrForm{})) = freeVars' $ children e -- Come back to it. Arrow tail or arrow application.
  -- freeVars' (dL -> e@(L _ HsTick{})) = freeVars' $ children e -- Haskell program coverage (Hpc) support.
  -- freeVars' (dL -> e@(L _ HsBinTick{})) = freeVars' $ children e -- Haskell program coverage (Hpc) support.
  -- freeVars' (dL -> e@(L _ HsTickPragma{})) = freeVars' $ children e -- Haskell program coverage (Hpc) support.
  -- freeVars' (dL -> e@(L _ EAsPat{})) = freeVars' $ children e -- Expr as pat.
  -- freeVars' (dL -> e@(L _ EViewPat{})) = freeVars' $ children e -- View pattern.
  -- freeVars' (dL -> e@(L _ ELazyPat{})) = freeVars' $ children e -- Lazy pattern.

  freeVars' e = freeVars' $ children e

instance FreeVars' (LHsRecField GhcPs (LHsExpr GhcPs)) where
   freeVars' (dL -> L _ (HsRecField _ x _)) = freeVars' x

instance FreeVars' (LHsRecUpdField GhcPs) where
  freeVars' (dL -> L _ (HsRecField _ x _)) = freeVars' x

instance AllVars' (LPat GhcPs) where
  allVars' (VarPat _ (dL -> L _ x)) = Vars' (Set.singleton $ rdrNameOcc x) Set.empty -- Variable pattern.
  allVars' (AsPat _  n x) = allVars' (VarPat noExt n :: Pat GhcPs) <> allVars' x -- As pattern.
  allVars' (ConPatIn _ (RecCon (HsRecFields flds _))) = allVars' flds
  allVars' (NPlusKPat _ n _ _ _ _) = allVars' (VarPat noExt n :: Pat GhcPs) -- n+k pattern.
  allVars' (ViewPat _ e p) = freeVars_' e <> allVars' p -- View pattern.

  allVars' WildPat{} = mempty -- Wildcard pattern.
  allVars' ConPatOut{} = mempty -- Renamer/typechecker.
  allVars' LitPat{} = mempty -- Literal pattern.
  allVars' NPat{} = mempty -- Natural pattern.

  -- allVars' p@SplicePat{} = allVars' $ children p -- Splice pattern (includes quasi-quotes).
  -- allVars' p@SigPat{} = allVars' $ children p -- Pattern with a type signature.
  -- allVars' p@CoPat{} = allVars' $ children p -- Coercion pattern.
  -- allVars' p@LazyPat{} = allVars' $ children p -- Lazy pattern.
  -- allVars' p@ParPat{} = allVars' $ children p -- Parenthesized pattern.
  -- allVars' p@BangPat{} = allVars' $ children p -- Bang pattern.
  -- allVars' p@ListPat{} = allVars' $ children p -- Syntactic list.
  -- allVars' p@TuplePat{} = allVars' $ children p -- Tuple sub patterns.
  -- allVars' p@SumPat{} = allVars' $ children p -- Anonymous sum pattern.

  allVars' p = allVars' $ children p

instance AllVars' (LHsRecField GhcPs (LPat GhcPs)) where
   allVars' (dL -> L _ (HsRecField _ x _)) = allVars' x

instance AllVars' (LStmt GhcPs (LHsExpr GhcPs)) where
  allVars' (dL -> L _ (LastStmt _ expr _ _)) = freeVars_' expr -- The last stmt of a 'ListComp', 'MonadComp', 'DoExpr','MDoExpr'.
  allVars' (dL -> L _ (BindStmt _ pat expr _ _)) = allVars' pat <> freeVars_' expr -- A generator e.g. 'x <- [1, 2, 3]'.
  allVars' (dL -> L _ (BodyStmt _ expr _ _)) = freeVars_' expr -- A boolean guard e.g. 'even x'.
  allVars' (dL -> L _ (LetStmt _ binds)) = allVars' binds -- A local declaration e.g. 'let y = x + 1'
  allVars' (dL -> L _ (TransStmt _ _ stmts _ using by _ _ fmap_)) = allVars' stmts <> freeVars_' using <> maybe mempty freeVars_' by <> freeVars_' (noLoc fmap_ :: Located (HsExpr GhcPs)) -- Apply a function to a list of statements in order.
  allVars' (dL -> L _ (RecStmt _ stmts _ _ _ _ _)) = allVars' stmts -- A recursive binding for a group of arrows.

  allVars' (dL -> L _ ApplicativeStmt{}) = mempty -- Generated by the renamer.
  allVars' (dL -> L _ ParStmt{}) = mempty -- Parallel list thing. Come back to it.

  allVars' _ = mempty -- New ctor.

instance AllVars' (LHsLocalBinds GhcPs) where
  allVars' (dL -> L _ (HsValBinds _ (ValBinds _ binds _))) = allVars' (bagToList binds) -- Value bindings.
  allVars' (dL -> L _ (HsIPBinds _ (IPBinds _ binds))) = allVars' binds -- Implicit parameter bindings.

  allVars' (dL -> L _ EmptyLocalBinds{}) =  mempty -- The case of no local bindings (signals the empty `let` or `where` clause).

  allVars' _ = mempty -- New ctor.

instance AllVars' (LIPBind GhcPs) where
  allVars' (dL -> L _ (IPBind _ _ e)) = freeVars_' e

  allVars' _ = mempty -- New ctor.

instance AllVars' (LHsBind GhcPs) where
  allVars' (dL -> L _ FunBind{fun_id=n, fun_matches=MG{mg_alts=(dL -> L _ ms)}}) = allVars' (VarPat noExt n :: Pat GhcPs) <> allVars' ms -- Function bindings and simple variable bindings e.g. 'f x = e', 'f !x = 3', 'f = e', '!x = e', 'x `f` y = e'
  allVars' (dL -> L _ PatBind{pat_lhs=n, pat_rhs=grhss}) = allVars' n <> allVars' grhss -- Ctor patterns and some other interesting cases e.g. 'Just x = e', '(x) = e', 'x :: Ty = e'.

  allVars' (dL -> L _ (PatSynBind _ PSB{})) = mempty -- Come back to it.
  allVars' (dL -> L _ VarBind{}) = mempty -- Typechecker.
  allVars' (dL -> L _ AbsBinds{}) = mempty -- Not sure but I think renamer.

  allVars' _ = mempty -- New ctor.

instance AllVars' (MatchGroup GhcPs (LHsExpr GhcPs)) where
  allVars' (MG _ _alts@(dL -> L _ alts) _) = inVars' (foldMap (allVars' . m_pats) ms) (allVars' (map m_grhss ms))
    where ms = map unLoc alts
  allVars' _ = mempty -- New ctor.

instance AllVars' (LMatch GhcPs (LHsExpr GhcPs)) where
  allVars' (dL -> L _ (Match _ FunRhs {mc_fun=name} pats grhss)) = allVars' (VarPat noExt name :: Pat GhcPs) <> allVars' pats <> allVars' grhss -- A pattern matching on an argument of a function binding.
  allVars' (dL -> L _ (Match _ (StmtCtxt ctxt) pats grhss)) = allVars' ctxt <> allVars' pats <> allVars' grhss -- Pattern of a do-stmt, list comprehension, pattern guard etc.
  allVars' (dL -> L _ (Match _ _ pats grhss)) = allVars' pats <> allVars' grhss -- Everything else.

  allVars' _ = mempty -- New ctor.

instance AllVars' (HsStmtContext RdrName) where
  allVars' (PatGuard FunRhs{mc_fun=n}) = allVars' (VarPat noExt n :: Pat GhcPs)
  allVars' ParStmtCtxt{} = mempty -- Come back to it.
  allVars' TransStmtCtxt{}  = mempty -- Come back to it.

  allVars' _ = mempty -- Everything else (correct).

instance AllVars' (GRHSs GhcPs (LHsExpr GhcPs)) where
  allVars' (GRHSs _ grhss binds) = inVars' binds (mconcat (map allVars' grhss))

  allVars' _ = mempty -- New ctor.

instance AllVars' (LGRHS GhcPs (LHsExpr GhcPs)) where
  allVars' (dL -> L _ (GRHS _ guards expr)) =  let gs = allVars' guards in Vars' (bound' gs) (free' gs ^+ (freeVars' expr ^- bound' gs))

  allVars' _ = mempty -- New ctor.

instance AllVars' (LHsDecl GhcPs) where
  allVars' (dL -> L l (ValD _ bind)) = allVars' (cL l bind :: LHsBind GhcPs)

  allVars' _ = mempty  -- We only consider value bindings.

--

vars' :: FreeVars' a => a -> [String]
vars' = Set.toList . Set.map occNameString . freeVars'

varss' :: AllVars' a => a -> [String]
varss' = Set.toList . Set.map occNameString . free' . allVars'

pvars' :: AllVars' a => a -> [String]
pvars' = Set.toList . Set.map occNameString . bound' . allVars'

freeVarSet' :: FreeVars' a => a -> Set String
freeVarSet' = Set.map occNameString . freeVars'

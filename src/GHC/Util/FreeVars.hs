{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Util.FreeVars (
    vars, varss, pvars,
    Vars (..), FreeVars(..) , AllVars (..)
  ) where

import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence
import GHC.Types.Name
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Data.Bag (bagToList)

import Data.Generics.Uniplate.DataOnly
import Data.Monoid
import Data.Semigroup
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude

( ^+ ) :: Set OccName -> Set OccName -> Set OccName
( ^+ ) = Set.union
( ^- ) :: Set OccName -> Set OccName -> Set OccName
( ^- ) = Set.difference

-- See [Note : Space leaks lurking here?] below.
data Vars = Vars{bound :: Set OccName, free :: Set OccName}

-- Useful for debugging.
instance Show Vars where
  show (Vars bs fs) = "bound : " ++
    show (map occNameString (Set.toList bs)) ++
    ", free : " ++ show (map occNameString (Set.toList fs))

instance Semigroup Vars where
    Vars x1 x2 <> Vars y1 y2 = Vars (x1 ^+ y1) (x2 ^+ y2)

instance Monoid Vars where
    mempty = Vars Set.empty Set.empty
    mconcat vs = Vars (Set.unions $ map bound vs) (Set.unions $ map free vs)

-- A type `a` is a model of `AllVars a` if exists a function
-- `allVars` for producing a pair of the bound and free varaiable
-- sets in a value of `a`.
class AllVars a where
    -- | Return the variables, erring on the side of more free
    -- variables.
    allVars :: a -> Vars

-- A type `a` is a model of `FreeVars a` if exists a function
-- `freeVars` for producing a set of free varaiable of a value of
-- `a`.
class FreeVars a where
    -- | Return the variables, erring on the side of more free
    -- variables.
    freeVars :: a -> Set OccName

-- Trivial instances.
instance AllVars Vars  where allVars = id
instance FreeVars (Set OccName) where freeVars = id
-- [Note : Space leaks lurking here?]
-- ==================================
-- We make use of `foldr`. @cocreature suggests we want bangs on `data
-- Vars` and replace usages of `mconcat` with `foldl`.
instance (AllVars a) => AllVars [a] where  allVars = mconcatMap allVars
instance (FreeVars a) => FreeVars [a] where  freeVars = Set.unions . map freeVars

-- Construct a `Vars` value with no bound vars.
freeVars_ :: (FreeVars a) => a -> Vars
freeVars_ = Vars Set.empty . freeVars

-- `inFree a b` is the set of free variables in a together with the
-- free variables in b not bound in a.
inFree :: (AllVars a, FreeVars b) => a -> b -> Set OccName
inFree a b = free aa ^+ (freeVars b ^- bound aa)
    where aa = allVars a

-- `inVars a b` is a value of `Vars_` with bound variables the union
-- of the bound variables of a and b and free variables the union
-- of the free variables of a and the free variables of b not
-- bound by a.
inVars :: (AllVars a, AllVars b) => a -> b -> Vars
inVars a b =
  Vars (bound aa ^+ bound bb) (free aa ^+ (free bb ^- bound aa))
    where aa = allVars a
          bb = allVars b

-- Get an `OccName` out of a reader name.
unqualNames :: LocatedN RdrName -> [OccName]
unqualNames (L _ (Unqual x)) = [x]
unqualNames (L _ (Exact x)) = [nameOccName x]
unqualNames _ = []

instance FreeVars (LocatedA (HsExpr GhcPs)) where
  freeVars (L _ (HsVar _ x)) = Set.fromList $ unqualNames x -- Variable.
  freeVars (L _ (HsUnboundVar _ x)) = Set.fromList [x] -- Unbound variable; also used for "holes".
  freeVars (L _ (HsLam _ mg)) = free (allVars mg) -- Lambda abstraction. Currently always a single match.
  freeVars (L _ (HsLamCase _ MG{mg_alts=(L _ ms)})) = free (allVars ms) -- Lambda case
  freeVars (L _ (HsCase _ of_ MG{mg_alts=(L _ ms)})) = freeVars of_ ^+ free (allVars ms) -- Case expr.
  freeVars (L _ (HsLet _ binds e)) = inFree binds e -- Let (rec).
  freeVars (L _ (HsDo _ ctxt (L _ stmts))) = free (allVars stmts) -- Do block.
  freeVars (L _ (RecordCon _ _ (HsRecFields flds _))) = Set.unions $ map freeVars flds -- Record construction.
  freeVars (L _ (RecordUpd _ e flds)) =
    case flds of
      Left fs -> Set.unions $ freeVars e : map freeVars fs
      Right ps -> Set.unions $ freeVars e : map freeVars ps
  freeVars (L _ (HsMultiIf _ grhss)) = free (allVars grhss) -- Multi-way if.
  freeVars (L _ (HsBracket _ (ExpBr _ e))) = freeVars e
  freeVars (L _ (HsBracket _ (VarBr _ _ v))) = Set.fromList [occName (unLoc v)]

  freeVars (L _ HsConLikeOut{}) = mempty -- After typechecker.
  freeVars (L _ HsRecFld{}) = mempty -- Variable pointing to a record selector.
  freeVars (L _ HsOverLabel{}) = mempty -- Overloaded label. The id of the in-scope fromLabel.
  freeVars (L _ HsIPVar{}) = mempty -- Implicit parameter.
  freeVars (L _ HsOverLit{}) = mempty -- Overloaded literal.
  freeVars (L _ HsLit{}) = mempty -- Simple literal.
  freeVars (L _ HsRnBracketOut{}) = mempty -- Renamer produces these.
  freeVars (L _ HsTcBracketOut{}) = mempty -- Typechecker produces these.

  -- freeVars (e@(L _ HsAppType{})) = freeVars $ children e -- Visible type application e.g. f @ Int x y.
  -- freeVars (e@(L _ HsApp{})) = freeVars $ children e -- Application.
  -- freeVars (e@(L _ OpApp{})) = freeVars $ children e -- Operator application.
  -- freeVars (e@(L _ NegApp{})) = freeVars $ children e -- Negation operator.
  -- freeVars (e@(L _ HsPar{})) = freeVars $ children e -- Parenthesized expr.
  -- freeVars (e@(L _ SectionL{})) = freeVars $ children e -- Left section.
  -- freeVars (e@(L _ SectionR{})) = freeVars $ children e -- Right section.
  -- freeVars (e@(L _ ExplicitTuple{})) = freeVars $ children e -- Explicit tuple and sections thereof.
  -- freeVars (e@(L _ ExplicitSum{})) = freeVars $ children e -- Used for unboxed sum types.
  -- freeVars (e@(L _ HsIf{})) = freeVars $ children e -- If.
  -- freeVars (e@(L _ ExplicitList{})) = freeVars $ children e -- Syntactic list e.g. [a, b, c].
  -- freeVars (e@(L _ ExprWithTySig{})) = freeVars $ children e -- Expr with type signature.
  -- freeVars (e@(L _ ArithSeq {})) = freeVars $ children e -- Arithmetic sequence.
  -- freeVars (e@(L _ HsSCC{})) = freeVars $ children e -- Set cost center pragma (expr whose const is to be measured).
  -- freeVars (e@(L _ HsCoreAnn{})) = freeVars $ children e -- Pragma.
  -- freeVars (e@(L _ HsBracket{})) = freeVars $ children e -- Haskell bracket.
  -- freeVars (e@(L _ HsSpliceE{})) = freeVars $ children e -- Template haskell splice expr.
  -- freeVars (e@(L _ HsProc{})) = freeVars $ children e -- Proc notation for arrows.
  -- freeVars (e@(L _ HsStatic{})) = freeVars $ children e -- Static pointers extension.
  -- freeVars (e@(L _ HsArrApp{})) = freeVars $ children e -- Arrow tail or arrow application.
  -- freeVars (e@(L _ HsArrForm{})) = freeVars $ children e -- Come back to it. Arrow tail or arrow application.
  -- freeVars (e@(L _ HsTick{})) = freeVars $ children e -- Haskell program coverage (Hpc) support.
  -- freeVars (e@(L _ HsBinTick{})) = freeVars $ children e -- Haskell program coverage (Hpc) support.
  -- freeVars (e@(L _ HsTickPragma{})) = freeVars $ children e -- Haskell program coverage (Hpc) support.
  -- freeVars (e@(L _ EAsPat{})) = freeVars $ children e -- Expr as pat.
  -- freeVars (e@(L _ EViewPat{})) = freeVars $ children e -- View pattern.
  -- freeVars (e@(L _ ELazyPat{})) = freeVars $ children e -- Lazy pattern.

  freeVars e = freeVars $ children e

instance FreeVars (HsTupArg GhcPs) where
  freeVars (Present _ args) = freeVars args
  freeVars _ = mempty

instance FreeVars (LocatedA (HsRecField GhcPs (LocatedA (HsExpr GhcPs)))) where
   freeVars o@(L _ (HsRecField _ x _ True)) = Set.singleton $ occName $ unLoc $ rdrNameFieldOcc $ unLoc x -- a pun
   freeVars o@(L _ (HsRecField _ _ x _)) = freeVars x

instance FreeVars (LocatedA (HsRecField' (AmbiguousFieldOcc GhcPs) (LocatedA (HsExpr GhcPs)))) where
  freeVars (L _ (HsRecField _ _ x _)) = freeVars x

instance FreeVars (LocatedA (HsRecField' (FieldLabelStrings GhcPs) (LocatedA (HsExpr GhcPs)))) where
  freeVars (L _ (HsRecField _ _ x _)) = freeVars x

instance AllVars (LocatedA (Pat GhcPs)) where
  allVars (L _ (VarPat _ (L _ x))) = Vars (Set.singleton $ rdrNameOcc x) Set.empty -- Variable pattern.
  allVars (L _ (AsPat _  n x)) = allVars (noLocA $ VarPat noExtField n :: LocatedA (Pat GhcPs)) <> allVars x -- As pattern.
  allVars (L _ (ConPat _ _ (RecCon (HsRecFields flds _)))) = allVars flds
  allVars (L _ (NPlusKPat _ n _ _ _ _)) = allVars (noLocA $ VarPat noExtField n :: LocatedA (Pat GhcPs)) -- n+k pattern.
  allVars (L _ (ViewPat _ e p)) = freeVars_ e <> allVars p -- View pattern.

  allVars (L _ WildPat{}) = mempty -- Wildcard pattern.
  allVars (L _ LitPat{}) = mempty -- Literal pattern.
  allVars (L _ NPat{}) = mempty -- Natural pattern.

  -- allVars p@SplicePat{} = allVars $ children p -- Splice pattern (includes quasi-quotes).
  -- allVars p@SigPat{} = allVars $ children p -- Pattern with a type signature.
  -- allVars p@CoPat{} = allVars $ children p -- Coercion pattern.
  -- allVars p@LazyPat{} = allVars $ children p -- Lazy pattern.
  -- allVars p@ParPat{} = allVars $ children p -- Parenthesized pattern.
  -- allVars p@BangPat{} = allVars $ children p -- Bang pattern.
  -- allVars p@ListPat{} = allVars $ children p -- Syntactic list.
  -- allVars p@TuplePat{} = allVars $ children p -- Tuple sub patterns.
  -- allVars p@SumPat{} = allVars $ children p -- Anonymous sum pattern.

  allVars p = allVars $ children p

instance AllVars (LocatedA (HsRecField GhcPs (LocatedA (Pat GhcPs)))) where
   allVars (L _ (HsRecField _ _ x _)) = allVars x

instance AllVars (LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))) where
  allVars (L _ (LastStmt _ expr _ _)) = freeVars_ expr -- The last stmt of a ListComp, MonadComp, DoExpr,MDoExpr.
  allVars (L _ (BindStmt _ pat expr)) = allVars pat <> freeVars_ expr -- A generator e.g. x <- [1, 2, 3].
  allVars (L _ (BodyStmt _ expr _ _)) = freeVars_ expr -- A boolean guard e.g. even x.
  allVars (L _ (LetStmt _ binds)) = allVars binds -- A local declaration e.g. let y = x + 1
  allVars (L _ (TransStmt _ _ stmts _ using by _ _ fmap_)) = allVars stmts <> freeVars_ using <> maybe mempty freeVars_ by <> freeVars_ (noLocA fmap_ :: LocatedA (HsExpr GhcPs)) -- Apply a function to a list of statements in order.
  allVars (L _ (RecStmt _ stmts _ _ _ _ _)) = allVars (unLoc stmts) -- A recursive binding for a group of arrows.

  allVars (L _ ApplicativeStmt{}) = mempty -- Generated by the renamer.
  allVars (L _ ParStmt{}) = mempty -- Parallel list thing. Come back to it.

instance AllVars (HsLocalBinds GhcPs) where
  allVars (HsValBinds _ (ValBinds _ binds _)) = allVars (bagToList binds) -- Value bindings.
  allVars (HsIPBinds _ (IPBinds _ binds)) = allVars binds -- Implicit parameter bindings.
  allVars EmptyLocalBinds{} =  mempty -- The case of no local bindings (signals the empty `let` or `where` clause).
  allVars _ = mempty -- extension points

instance AllVars (LocatedA (IPBind GhcPs)) where
  allVars (L _ (IPBind _ _ e)) = freeVars_ e

instance AllVars (LocatedA (HsBindLR GhcPs GhcPs)) where
  allVars (L _ FunBind{fun_id=n, fun_matches=MG{mg_alts=(L _ ms)}}) = allVars (noLocA $ VarPat noExtField n :: LocatedA (Pat GhcPs)) <> allVars ms -- Function bindings and simple variable bindings e.g. f x = e, f !x = 3, f = e, !x = e, x `f` y = e
  allVars (L _ PatBind{pat_lhs=n, pat_rhs=grhss}) = allVars n <> allVars grhss -- Ctor patterns and some other interesting cases e.g. Just x = e, (x) = e, x :: Ty = e.

  allVars (L _ (PatSynBind _ PSB{})) = mempty -- Come back to it.
  allVars (L _ VarBind{}) = mempty -- Typechecker.
  allVars (L _ AbsBinds{}) = mempty -- Not sure but I think renamer.

instance AllVars (MatchGroup GhcPs (LocatedA (HsExpr GhcPs))) where
  allVars (MG _ _alts@(L _ alts) _) = inVars (foldMap (allVars . m_pats) ms) (allVars (map m_grhss ms))
    where ms = map unLoc alts

instance AllVars (LocatedA (Match GhcPs (LocatedA (HsExpr GhcPs)))) where
  allVars (L _ (Match _ FunRhs {mc_fun=name} pats grhss)) = allVars (noLocA $ VarPat noExtField name :: LocatedA (Pat GhcPs)) <> allVars pats <> allVars grhss -- A pattern matching on an argument of a function binding.
  allVars (L _ (Match _ (StmtCtxt ctxt) pats grhss)) = allVars ctxt <> allVars pats <> allVars grhss -- Pattern of a do-stmt, list comprehension, pattern guard etc.
  allVars (L _ (Match _ _ pats grhss)) = inVars (allVars pats) (allVars grhss) -- Everything else.

instance AllVars (HsStmtContext GhcPs) where
  allVars (PatGuard FunRhs{mc_fun=n}) = allVars (noLocA $ VarPat noExtField n :: LocatedA (Pat GhcPs))
  allVars ParStmtCtxt{} = mempty -- Come back to it.
  allVars TransStmtCtxt{}  = mempty -- Come back to it.
  allVars _ = mempty

instance AllVars (GRHSs GhcPs (LocatedA (HsExpr GhcPs))) where
  allVars (GRHSs _ grhss binds) = inVars binds (mconcatMap allVars grhss)

instance AllVars (Located (GRHS GhcPs (LocatedA (HsExpr GhcPs)))) where
  allVars (L _ (GRHS _ guards expr)) = Vars (bound gs) (free gs ^+ (freeVars expr ^- bound gs)) where gs = allVars guards

instance AllVars (LocatedA (HsDecl GhcPs)) where
  allVars (L l (ValD _ bind)) = allVars (L l bind :: LocatedA (HsBindLR GhcPs GhcPs))
  allVars _ = mempty


vars :: FreeVars a => a -> [String]
vars = Set.toList . Set.map occNameString . freeVars

varss :: AllVars a => a -> [String]
varss = Set.toList . Set.map occNameString . free . allVars

pvars :: AllVars a => a -> [String]
pvars = Set.toList . Set.map occNameString . bound . allVars

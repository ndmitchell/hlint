{-# LANGUAGE FlexibleInstances #-}

module HSE.FreeVars(FreeVars, freeVars, vars, varss, pvars) where

import Data.Monoid
import HSE.Type as HSE
import qualified Data.Set as Set
import Data.Set(Set)
import Prelude


vars x = Set.toList $ freeVars x

varss x = Set.toList $ free $ allVars x

pvars x = Set.toList $ bound $ allVars x


(^+) = Set.union
(^-) = Set.difference

data Vars = Vars {bound :: Set String, free :: Set String}

instance Monoid Vars where
    mempty = Vars Set.empty Set.empty
    mappend (Vars x1 x2) (Vars y1 y2) = Vars (x1 ^+ y1) (x2 ^+ y2)
    mconcat fvs = Vars (Set.unions $ map bound fvs) (Set.unions $ map free fvs)


class AllVars a where
    -- | Return the variables, erring on the side of more free variables
    allVars :: a -> Vars

class FreeVars a where
    -- | Return the variables, erring on the side of more free variables
    freeVars :: a -> Set String

freeVars_ :: FreeVars a => a -> Vars
freeVars_ = Vars Set.empty . freeVars

inFree :: (AllVars a, FreeVars b) => a -> b -> Set String
inFree a b = free aa ^+ (freeVars b ^- bound aa)
    where aa = allVars a

inVars :: (AllVars a, AllVars b) => a -> b -> Vars
inVars a b = Vars (bound aa ^+ bound bb) (free aa ^+ (free bb ^- bound aa))
    where aa = allVars a
          bb = allVars b


unqualNames :: QName S -> [String]
unqualNames (UnQual _ x) = [prettyPrint x]
unqualNames _ = []

unqualOp :: QOp S -> [String]
unqualOp (QVarOp _ x) = unqualNames x
unqualOp (QConOp _ x) = unqualNames x


instance FreeVars (Set String) where
    freeVars = id

instance AllVars Vars where
    allVars = id

instance FreeVars Exp_ where -- never has any bound variables
    freeVars (Var _ x) = Set.fromList $ unqualNames x
    freeVars (VarQuote l x) = freeVars $ Var l x
    freeVars (SpliceExp _ (IdSplice _ x)) = Set.fromList [x]
    freeVars (InfixApp _ a op b) = freeVars a ^+ Set.fromList (unqualOp op) ^+ freeVars b
    freeVars (LeftSection _ a op) = freeVars a ^+ Set.fromList (unqualOp op)
    freeVars (RightSection _ op b) = Set.fromList (unqualOp op) ^+ freeVars b
    freeVars (Lambda _ p x) = inFree p x
    freeVars (Let _ bind x) = inFree bind x
    freeVars (Case _ x alts) = freeVars x `mappend` freeVars alts
    freeVars (Do _ xs) = free $ allVars xs
    freeVars (MDo l xs) = freeVars $ Do l xs
    freeVars (ParComp _ x xs) = free xfv ^+ (freeVars x ^- bound xfv)
        where xfv = mconcat $ map allVars xs
    freeVars (ListComp l x xs) = freeVars $ ParComp l x [xs]
    freeVars x = freeVars $ children x

instance FreeVars [Exp_] where
    freeVars = Set.unions . map freeVars

instance AllVars Pat_ where
    allVars (PVar _ x) = Vars (Set.singleton $ prettyPrint x) Set.empty
    allVars (PNPlusK l x _) = allVars (PVar l x)
    allVars (PAsPat l n x) = allVars (PVar l n) `mappend` allVars x
    allVars (PWildCard _) = mempty -- explicitly cannot guess what might be bound here
    allVars (PViewPat _ e p) = freeVars_ e `mappend` allVars p
    allVars x = allVars $ children x

instance AllVars [Pat_] where
    allVars = mconcat . map allVars

instance FreeVars (HSE.Alt S) where
    freeVars (HSE.Alt _ pat alt bind) = inFree pat $ inFree bind alt

instance FreeVars [HSE.Alt S] where
    freeVars = mconcat . map freeVars

instance FreeVars (Rhs S) where
    freeVars (UnGuardedRhs _ x) = freeVars x
    freeVars (GuardedRhss _ xs) = mconcat $ map freeVars xs

instance FreeVars (GuardedRhs S) where
    freeVars (GuardedRhs _ stmt exp) = inFree stmt exp

instance AllVars (QualStmt S) where
    allVars (QualStmt _ x) = allVars x
    allVars x = freeVars_ (childrenBi x :: [Exp_])

instance AllVars [QualStmt S] where
    allVars (x:xs) = inVars x xs
    allVars [] = mempty

instance AllVars [Stmt S] where
    allVars (x:xs) = inVars x xs
    allVars [] = mempty

instance AllVars (Stmt S) where
    allVars (Generator _ pat exp) = allVars pat `mappend` freeVars_ exp
    allVars (Qualifier _ exp) = freeVars_ exp
    allVars (LetStmt _ binds) = allVars binds
    allVars (RecStmt _ stmts) = allVars stmts

instance AllVars (Maybe (Binds S)) where
    allVars = maybe mempty allVars

instance AllVars (Binds S) where
    allVars (BDecls _ decls) = allVars decls
    allVars (IPBinds _ binds) = freeVars_ binds

instance AllVars [Decl S] where
    allVars = mconcat . map allVars

instance AllVars (Decl S) where
    allVars (FunBind _ m) = allVars m
    allVars (PatBind _ pat rhs bind) = allVars pat `mappend` freeVars_ (inFree bind rhs)
    allVars _ = mempty

instance AllVars [Match S] where
    allVars = mconcat . map allVars

instance AllVars (Match S) where
    allVars (Match l name pat rhs binds) = allVars (PVar l name) `mappend` freeVars_ (inFree pat (inFree binds rhs))
    allVars (InfixMatch l p1 name p2 rhs binds) = allVars $ Match l name (p1:p2) rhs binds

instance FreeVars [IPBind S] where
    freeVars = mconcat . map freeVars

instance FreeVars (IPBind S) where
    freeVars (IPBind _ _ exp) = freeVars exp

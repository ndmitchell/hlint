{-# LANGUAGE MultiParamTypeClasses , FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-overlapping-patterns #-}

module GHC.Util.Brackets (Brackets(..), isApp,isOpApp,isAnyApp) where

import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Types.SourceText
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Refact.Types

class Brackets a where
  remParen :: a -> Maybe a -- Remove one paren or nothing if there is no paren.
  addParen :: a -> a -- Write out a paren.
  -- | Is this item lexically requiring no bracketing ever i.e. is
  -- totally atomic.
  isAtom :: a -> Bool
  -- | Is the child safe free from brackets in the parent
  -- position. Err on the side of caution, True = don't know.
  needBracket :: Int -> a -> a -> Bool
  findType :: a -> RType

instance Brackets (LocatedA (HsExpr GhcPs)) where
  -- When GHC parses a section in concrete syntax, it will produce an
  -- 'HsPar (Section[L|R])'. There is no concrete syntax that will
  -- result in a "naked" section. Consequently, given an expression,
  -- when stripping brackets (c.f. 'Hint.Brackets), don't remove the
  -- paren's surrounding a section - they are required.
  remParen (L _ (HsPar _ _ (L _ SectionL{}) _)) = Nothing
  remParen (L _ (HsPar _ _ (L _ SectionR{}) _)) = Nothing
  remParen (L _ (HsPar _ _ x _)) = Just x
  remParen _ = Nothing

  addParen = nlHsPar

  isAtom (L _ x) = case x of
      HsVar{} -> True
      HsUnboundVar{} -> True
      -- Technically atomic, but lots of people think it shouldn't be
      HsRecSel{} -> False
      HsOverLabel{} -> True
      HsIPVar{} -> True
      -- Note that sections aren't atoms (but parenthesized sections are).
      HsPar{} -> True
      ExplicitTuple{} -> True
      ExplicitSum{} -> True
      ExplicitList{} -> True
      RecordCon{} -> True
      RecordUpd{} -> True
      ArithSeq{}-> True
      HsTypedBracket{} -> True
      HsUntypedBracket{} -> True
      -- HsSplice might be $foo, where @($foo) would require brackets,
      -- but in that case the $foo is a type, so we can still mark Splice as atomic
      HsSpliceE{} -> True
      HsOverLit _ x | not $ isNegativeOverLit x -> True
      HsLit _ x     | not $ isNegativeLit x     -> True
      _  -> False
      where
        isNegativeLit (HsInt _ i) = il_neg i
        isNegativeLit (HsRat _ f _) = fl_neg f
        isNegativeLit (HsFloatPrim _ f) = fl_neg f
        isNegativeLit (HsDoublePrim _ f) = fl_neg f
        isNegativeLit (HsIntPrim _ x) = x < 0
        isNegativeLit (HsInt64Prim _ x) = x < 0
        isNegativeLit (HsInteger _ x _) = x < 0
        isNegativeLit _ = False
        isNegativeOverLit OverLit {ol_val=HsIntegral i} = il_neg i
        isNegativeOverLit OverLit {ol_val=HsFractional f} = fl_neg f
        isNegativeOverLit _ = False
  isAtom _ = False -- '{-# COMPLETE L #-}'

  needBracket i parent child -- Note: i is the index in children, not in the AST.
     | isAtom child = False
     | isSection parent, L _ HsApp{} <- child = False
     | L _ OpApp{} <- parent, L _ HsApp{} <- child, i /= 0 || isAtomOrApp child = False
     | L _ ExplicitList{} <- parent = False
     | L _ ExplicitTuple{} <- parent = False
     | L _ HsIf{} <- parent, isAnyApp child = False
     | L _ HsApp{} <- parent, i == 0, L _ HsApp{} <- child = False
     | L _ ExprWithTySig{} <- parent, i == 0, isApp child = False
     | L _ RecordCon{} <- parent = False
     | L _ RecordUpd{} <- parent, i /= 0 = False

     -- These all have view patterns embedded within them, or are naturally followed by ->, so we have to watch out for
     -- @(x::y) -> z@ which is valid, as either a type annotation, or a view pattern.
     | L _ HsLet{} <- parent, isApp child = False
     | L _ HsDo{} <- parent, isAnyApp child = False
     | L _ HsLam{} <- parent, isAnyApp child = False
     | L _ HsCase{} <- parent, isAnyApp child = False

     | L _ HsPar{} <- parent = False
     | otherwise = True

  findType _ = Expr

-- | Am I an HsApp such that having me in an infix doesn't require brackets.
--   Before BlockArguments that was _all_ HsApps. Now, imagine:
--
--   (f \x -> x) *> ...
--   (f do x) *> ...
isAtomOrApp :: LocatedA (HsExpr GhcPs) -> Bool
isAtomOrApp x | isAtom x = True
isAtomOrApp (L _ (HsApp _ _ x)) = isAtomOrApp x
isAtomOrApp _ = False

instance Brackets (LocatedA (Pat GhcPs)) where
  remParen (L _ (ParPat _ _ x _)) = Just x
  remParen _ = Nothing
  addParen = nlParPat

  isAtom (L _ x) = case x of
    ParPat{} -> True
    TuplePat{} -> True
    ListPat{} -> True
    -- This is technically atomic, but lots of people think it shouldn't be
    ConPat _ _ RecCon{} -> False
    ConPat _ _ (PrefixCon _ []) -> True
    VarPat{} -> True
    WildPat{} -> True
    SumPat{} -> True
    AsPat{} -> True
    SplicePat{} -> True
    LitPat _ x | not $ isSignedLit x -> True
    _ -> False
    where
      isSignedLit HsInt{} = True
      isSignedLit HsIntPrim{} = True
      isSignedLit HsInt64Prim{} = True
      isSignedLit HsInteger{} = True
      isSignedLit HsRat{} = True
      isSignedLit HsFloatPrim{} = True
      isSignedLit HsDoublePrim{} = True
      isSignedLit _ = False
  isAtom _ = False -- '{-# COMPLETE L #-}'

  needBracket _ parent child
    | isAtom child = False
    | L _ TuplePat{} <- parent = False
    | L _ ListPat{} <- parent = False
    | otherwise = True

  findType _ = Pattern

instance Brackets (LocatedA (HsType GhcPs)) where
  remParen (L _ (HsParTy _ x)) = Just x
  remParen _ = Nothing
  addParen e = noLocA $ HsParTy EpAnnNotUsed e

  isAtom (L _ x) = case x of
      HsParTy{} -> True
      HsTupleTy{} -> True
      HsListTy{} -> True
      HsExplicitTupleTy{} -> True
      HsExplicitListTy{} -> True
      HsTyVar{} -> True
      HsSumTy{} -> True
      HsWildCardTy{} -> True
      -- HsSpliceTy{} is not atomic, because of @($foo)
      _ -> False
  isAtom _ = False -- '{-# COMPLETE L #-}'

  needBracket _ parent child
    | isAtom child = False
-- a -> (b -> c) is not a required bracket, but useful for documentation about arity etc.
--        | TyFun{} <- parent, i == 1, TyFun{} <- child = False
    | L _ HsFunTy{} <- parent, L _ HsAppTy{} <- child = False
    | L _ HsTupleTy{} <- parent = False
    | L _ HsListTy{} <- parent = False
    | L _ HsExplicitTupleTy{} <- parent = False
    | L _ HsListTy{} <- parent = False
    | L _ HsExplicitListTy{} <- parent = False
    | L _ HsOpTy{} <- parent, L _ HsAppTy{} <- child = False
    | L _ HsParTy{} <- parent = False
    | otherwise = True

  findType _ = Type

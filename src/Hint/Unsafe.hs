
{-
    Find things that are unsafe

<TEST>
{-# NOINLINE entries #-}; entries = unsafePerformIO newIO
entries = unsafePerformIO Multimap.newIO -- {-# NOINLINE entries #-} ; entries = unsafePerformIO Multimap.newIO
entries = unsafePerformIO $ f y where foo = 1 -- {-# NOINLINE entries #-} ; entries = unsafePerformIO $ f y where foo = 1
entries v = unsafePerformIO $ Multimap.newIO where foo = 1
entries v = x where x = unsafePerformIO $ Multimap.newIO
entries = x where x = unsafePerformIO $ Multimap.newIO -- {-# NOINLINE entries #-} ; entries = x where x = unsafePerformIO $ Multimap.newIO
entries = unsafePerformIO . bar
entries = unsafePerformIO . baz $ x -- {-# NOINLINE entries #-} ; entries = unsafePerformIO . baz $ x
entries = unsafePerformIO . baz $ x -- {-# NOINLINE entries #-} ; entries = unsafePerformIO . baz $ x
</TEST>
-}


module Hint.Unsafe(unsafeHint) where

import Hint.Type(DeclHint,ModuleEx(..),Severity(..),rawIdea,toSSA)
import Data.List.Extra
import Refact.Types hiding(Match)
import Data.Generics.Uniplate.DataOnly

import GHC.Hs
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Data.FastString
import GHC.Types.Basic
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable

-- The conditions on which to fire this hint are subtle. We are
-- interested exclusively in application constants involving
-- 'unsafePerformIO'. For example,
-- @
--   f = \x -> unsafePerformIO x
-- @
-- is not such a declaration (the right hand side is a lambda, not an
-- application) whereas,
-- @
--   f = g where g = unsafePerformIO Multimap.newIO
-- @
-- is. We advise that such constants should have a @NOINLINE@ pragma.
unsafeHint :: DeclHint
unsafeHint _ (ModuleEx (L _ m)) = \ld@(L loc d) ->
  [rawIdea Hint.Type.Warning "Missing NOINLINE pragma" (locA loc)
         (unsafePrettyPrint d)
         (Just $ trimStart (unsafePrettyPrint $ gen x) ++ "\n" ++ unsafePrettyPrint d)
         [] [InsertComment (toSSA ld) (unsafePrettyPrint $ gen x)]
     -- 'x' does not declare a new function.
     | d@(ValD _
           FunBind {fun_id=L _ (Unqual x)
                      , fun_matches=MG{mg_origin=FromSource,mg_alts=L _ [L _ Match {m_pats=[]}]}}) <- [d]
     -- 'x' is a synonym for an appliciation involing 'unsafePerformIO'
     , isUnsafeDecl d
     -- 'x' is not marked 'NOINLINE'.
     , x `notElem` noinline]
  where
    gen :: OccName -> LHsDecl GhcPs
    gen x = noLocA $
      SigD noExtField (InlineSig EpAnnNotUsed (noLocA (mkRdrUnqual x))
                      (InlinePragma (SourceText "{-# NOINLINE") NoInline Nothing NeverActive FunLike))
    noinline :: [OccName]
    noinline = [q | L _(SigD _ (InlineSig _ (L _ (Unqual q))
                                                (InlinePragma _ NoInline Nothing NeverActive FunLike))
        ) <- hsmodDecls m]

isUnsafeDecl :: HsDecl GhcPs -> Bool
isUnsafeDecl (ValD _ FunBind {fun_matches=MG {mg_origin=FromSource,mg_alts=L _ alts}}) =
  any isUnsafeApp (childrenBi alts) || any isUnsafeDecl (childrenBi alts)
isUnsafeDecl _ = False

-- Am I equivalent to @unsafePerformIO x@?
isUnsafeApp :: HsExpr GhcPs -> Bool
isUnsafeApp (OpApp _ (L _ l) op _ ) | isDol op = isUnsafeFun l
isUnsafeApp (HsApp _ (L _ x) _) = isUnsafeFun x
isUnsafeApp _ = False

-- Am I equivalent to @unsafePerformIO . x@?
isUnsafeFun :: HsExpr GhcPs -> Bool
isUnsafeFun (HsVar _ (L _ x)) | x == mkVarUnqual (fsLit "unsafePerformIO") = True
isUnsafeFun (OpApp _ (L _ l) op _) | isDot op = isUnsafeFun l
isUnsafeFun _ = False

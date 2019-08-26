{-# LANGUAGE PackageImports #-}

{-
    Find things that are unsafe

<TEST>
{-# NOINLINE slaves #-}; slaves = unsafePerformIO newIO
slaves = unsafePerformIO Multimap.newIO -- {-# NOINLINE slaves #-} ; slaves = unsafePerformIO Multimap.newIO
slaves = unsafePerformIO $ f y where foo = 1 -- {-# NOINLINE slaves #-} ; slaves = unsafePerformIO $ f y where foo = 1
slaves v = unsafePerformIO $ Multimap.newIO where foo = 1
slaves v = x where x = unsafePerformIO $ Multimap.newIO
slaves = x where x = unsafePerformIO $ Multimap.newIO -- {-# NOINLINE slaves #-} ; slaves = x where x = unsafePerformIO $ Multimap.newIO
slaves = unsafePerformIO . bar
slaves = unsafePerformIO . baz $ x -- {-# NOINLINE slaves #-} ; slaves = unsafePerformIO . baz $ x
slaves = unsafePerformIO . baz $ x -- {-# NOINLINE slaves #-} ; slaves = unsafePerformIO . baz $ x
</TEST>
-}


module Hint.Unsafe(unsafeHint) where

import Hint.Type
import Data.Char
import Refact.Types

import qualified "ghc-lib-parser" HsSyn as GHC
import "ghc-lib-parser" HsExpr as GHC
import "ghc-lib-parser" HsDecls
import "ghc-lib-parser" HsExtension
import "ghc-lib-parser" OccName
import "ghc-lib-parser" RdrName
import "ghc-lib-parser" FastString
import "ghc-lib-parser" BasicTypes as GHC
import qualified "ghc-lib-parser" SrcLoc as GHC
import qualified GHC.Util as GHC

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
unsafeHint :: DeclHint'
unsafeHint _ (ModuleEx _ _ (GHC.L _ m) _) = \(GHC.L loc d) ->
  [rawIdea Hint.Type.Warning "Missing NOINLINE pragma" (ghcSpanToHSE loc)
         (GHC.unsafePrettyPrint d)
         (Just $ dropWhile isSpace (GHC.unsafePrettyPrint $ gen x) ++ "\n" ++ GHC.unsafePrettyPrint d)
         [] [InsertComment (toSS' (GHC.L loc d)) (GHC.unsafePrettyPrint $ gen x)]
     -- 'x' does not declare a new function.
     | d@(GHC.ValD _
           GHC.FunBind {GHC.fun_id=GHC.L _ (Unqual x)
                      , GHC.fun_matches=MG{GHC.mg_alts=GHC.L _ [GHC.L _ GHC.Match {GHC.m_pats=[]}]}}) <- [d]
     -- 'x' is a synonym for an appliciation involing 'unsafePerformIO'
     , isUnsafeDecl d
     -- 'x' is not marked 'NOINLINE'.
     , x `notElem` noinline]
  where
    gen :: OccName -> LHsDecl GhcPs
    gen x = GHC.noLoc $
      SigD GHC.noExt (GHC.InlineSig noExt (GHC.noLoc (mkRdrUnqual x))
                      (InlinePragma (SourceText "{-# NOINLINE") NoInline Nothing NeverActive FunLike))
    noinline :: [OccName]
    noinline = [q | GHC.L _(SigD _ (GHC.InlineSig _ (GHC.L _ (Unqual q))
                                                (InlinePragma _ NoInline Nothing NeverActive FunLike))
        ) <- GHC.hsmodDecls m]

isUnsafeDecl :: HsDecl GhcPs -> Bool
isUnsafeDecl (ValD _ GHC.FunBind {GHC.fun_matches=MG {mg_alts= GHC.L _ alts}}) =
  any isUnsafeApp (childrenBi alts) || any isUnsafeDecl (childrenBi alts)
isUnsafeDecl _ = False

-- Am I equivalent to @unsafePerformIO x@?
isUnsafeApp :: HsExpr GhcPs -> Bool
isUnsafeApp (OpApp _ (GHC.L _ l) (GHC.L _ op) _ ) | GHC.isDol' op = isUnsafeFun l
isUnsafeApp (HsApp _ (GHC.L _ x) _) = isUnsafeFun x
isUnsafeApp _ = False

-- Am I equivalent to @unsafePerformIO . x@?
isUnsafeFun :: HsExpr GhcPs -> Bool
isUnsafeFun (HsVar _ (GHC.L _ x)) | x == mkVarUnqual (fsLit "unsafePerformIO") = True
isUnsafeFun (OpApp _ (GHC.L _ l) (GHC.L _ op) _) | GHC.isDot' op = isUnsafeFun l
isUnsafeFun _ = False


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


unsafeHint :: ModuHint
unsafeHint _ m =
        [ rawIdea Warning "Missing NOINLINE pragma" (srcInfoSpan $ ann d)
            (prettyPrint d)
            (Just $ dropWhile isSpace (prettyPrint $ gen x) ++ "\n" ++ prettyPrint d)
            [] [InsertComment (toSS d) (prettyPrint $ gen x)]
        | d@(PatBind _ (PVar _ x) _ _) <- moduleDecls m
        , isUnsafeDecl d, x `notElem_` noinline]
    where
        gen x = InlineSig an False Nothing $ UnQual an x
        noinline = [q | InlineSig _ False Nothing (UnQual _ q) <- moduleDecls m]

isUnsafeDecl :: Decl_ -> Bool
isUnsafeDecl (PatBind _ _ rhs bind) =
    any isUnsafeApp (childrenBi rhs) || any isUnsafeDecl (childrenBi bind)
isUnsafeDecl _ = False

-- Am I equivalent to @unsafePerformIO x@
isUnsafeApp :: Exp_ -> Bool
isUnsafeApp (InfixApp _ x d _) | isDol d = isUnsafeFun x
isUnsafeApp (App _ x _) = isUnsafeFun x
isUnsafeApp _ = False

-- Am I equivalent to @unsafePerformIO . x@
isUnsafeFun :: Exp_ -> Bool
isUnsafeFun (Var _ x) | x ~= "unsafePerformIO" = True
isUnsafeFun (InfixApp _ x d _) | isDot d = isUnsafeFun x
isUnsafeFun _ = False

{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-

Raise an error if you are bracketing an atom, or are enclosed by a
list bracket.

<TEST>
-- expression bracket reduction
yes = (f x) x -- @Suggestion f x x
no = f (x x)
yes = (foo) -- foo
yes = (foo bar) -- @Suggestion foo bar
yes = foo (bar) -- @Warning bar
yes = foo ((x x)) -- @Suggestion (x x)
yes = (f x) ||| y -- @Suggestion f x ||| y
yes = if (f x) then y else z -- @Suggestion if f x then y else z
yes = if x then (f y) else z -- @Suggestion if x then f y else z
yes = (a foo) :: Int -- @Suggestion a foo :: Int
yes = [(foo bar)] -- @Suggestion [foo bar]
yes = foo ((x y), z) -- @Suggestion (x y, z)
yes = C { f = (e h) } -- @Suggestion C {f = e h}
yes = \ x -> (x && x) -- @Suggestion \x -> x && x
no = \(x -> y) -> z
yes = (`foo` (bar baz)) -- @Suggestion (`foo` bar baz)
yes = f ((x)) -- @Warning x
main = do f; (print x) -- @Suggestion do f print x
yes = f (x) y -- @Warning x
no = f (+x) y
no = f ($x) y
no = ($x)
yes = (($x))
no = ($1)
yes = (($1)) -- @Warning ($1)
no = (+5)
yes = ((+5)) -- @Warning (+5)

-- type bracket reduction
foo :: (Int -> Int) -> Int
foo :: (Maybe Int) -> a -- @Suggestion Maybe Int -> a
instance Named (DeclHead S)
data Foo = Foo {foo :: (Maybe Foo)} -- @Suggestion foo :: Maybe Foo

-- pattern bracket reduction
foo (x:xs) = 1
foo (True) = 1 -- @Warning True
foo ((True)) = 1 -- @Warning True
foo (A{}) = True -- A{}
f x = case x of (Nothing) -> 1; _ -> 2 -- Nothing

-- dollar reduction tests
no = groupFsts . sortFst $ mr
yes = split "to" $ names -- split "to" names
yes = white $ keysymbol -- white keysymbol
yes = operator foo $ operator -- operator foo operator
no = operator foo $ operator bar
yes = return $ Record{a=b}

-- $/bracket rotation tests
yes = (b $ c d) ++ e -- b (c d) ++ e
yes = (a b $ c d) ++ e -- a b (c d) ++ e
no = (f . g $ a) ++ e
no = quickCheck ((\h -> cySucc h == succ h) :: Hygiene -> Bool)
foo = (case x of y -> z; q -> w) :: Int

-- backup fixity resolution
main = do a += b . c; return $ a . b

-- <$> bracket tests
yes = (foo . bar x) <$> baz q -- foo . bar x <$> baz q
no = foo . bar x <$> baz q

-- annotations
main = 1; {-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
main = 1; {-# ANN module (1 + (2)) #-} -- 2

-- special case from esqueleto, see #224
main = operate <$> (select $ from $ \user -> return $ user ^. UserEmail)
-- unknown fixity, see #426
bad x = x . (x +? x . x)
-- special case people don't like to warn on
special = foo $ f{x=1}
special = foo $ Rec{x=1}
special = foo (f{x=1})
</TEST>
-}


module Hint.Bracket(bracketHint) where

import Hint.Type(DeclHint',Idea(..),rawIdea',warn',suggest',Severity(..),toSS')
import Data.Data
import Data.Generics.Uniplate.Operations
import Refact.Types

import HsSyn
import Outputable
import SrcLoc
import GHC.Util

bracketHint :: DeclHint'
bracketHint _ _ x =
  concatMap (\x -> bracket prettyExpr isPartialAtom True x ++ dollar x) (childrenBi (descendBi annotations x) :: [LHsExpr GhcPs]) ++
  concatMap (bracket unsafePrettyPrint (const False) False) (childrenBi x :: [LHsType GhcPs]) ++
  concatMap (bracket unsafePrettyPrint (const False) False) (childrenBi x :: [Pat GhcPs]) ++
  concatMap fieldDecl (childrenBi x)
   where
     -- Brackets the roots of annotations are fine, so we strip them.
     annotations :: AnnDecl GhcPs -> AnnDecl GhcPs
     annotations= descendBi $ \x -> case (x :: LHsExpr GhcPs) of
       LL l (HsPar _ x) -> x
       x -> x

-- If we find ourselves in the context of a section and we want to
-- issue a warning that a child therein has unneccessary brackets,
-- we'd rather report 'Found : (`Foo` (Bar Baz))' rather than 'Found :
-- `Foo` (Bar Baz)'. If left to 'unsafePrettyPrint' we'd get the
-- latter (in contrast to the HSE pretty printer). This patches things
-- up.
prettyExpr :: LHsExpr GhcPs -> String
prettyExpr s@(LL _ SectionL{}) = unsafePrettyPrint (noLoc (HsPar noExt s) :: LHsExpr GhcPs)
prettyExpr s@(LL _ SectionR{}) = unsafePrettyPrint (noLoc (HsPar noExt s) :: LHsExpr GhcPs)
prettyExpr x = unsafePrettyPrint x

-- Dirty, should add to Brackets type class I think
tyConToRtype :: String -> RType
tyConToRtype "Exp"    = Expr
tyConToRtype "Type"   = Type
tyConToRtype "HsType" = Type
tyConToRtype "Pat"    = Pattern
tyConToRtype _        = Expr

findType :: (Data a) => a -> RType
findType = tyConToRtype . dataTypeName . dataTypeOf

-- 'Just _' if at least one set of parens were removed. 'Nothing' if
-- zero parens were removed.
remParens' :: Brackets' a => a -> Maybe a
remParens' = fmap go . remParen'
  where
    go e = maybe e go (remParen' e)

isPartialAtom :: LHsExpr GhcPs -> Bool
-- Might be '$x', which was really '$ x', but TH enabled misparsed it.
isPartialAtom (LL _ (HsSpliceE _ (HsTypedSplice _ HasDollar _ _) )) = True
isPartialAtom (LL _ (HsSpliceE _ (HsUntypedSplice _ HasDollar _ _) )) = True
isPartialAtom x = isRecConstr' x || isRecUpdate' x

bracket :: forall a . (Data a, Data (SrcSpanLess a), HasSrcSpan a, Outputable a, Brackets' a) => (a -> String) -> (a -> Bool) -> Bool -> a -> [Idea]
bracket pretty isPartialAtom root = f Nothing
  where
    msg = "Redundant bracket"
    -- 'f' is a (generic) function over types in 'Brackets'
    -- (expressions, patterns and types). Arguments are, 'f (Maybe
    -- (index, parent, gen)) child'.
    f :: (HasSrcSpan a, Data a, Outputable a, Brackets' a) => Maybe (Int, a , a -> a) -> a -> [Idea]
    -- No context. Removing parentheses from 'x' succeeds?
    f Nothing o@(remParens' -> Just x)
      -- If at the root, or 'x' is an atom, 'x' parens are redundant.
      | root || isAtom' x
      , not $ isPartialAtom x =
          (if isAtom' x then bracketError else bracketWarning) msg o x : g x
    -- In some context, removing parentheses from 'x' succeeds and 'x'
    -- is atomic?
    f Just{} o@(remParens' -> Just x)
      | isAtom' x
      , not $ isPartialAtom x =
          bracketError msg o x : g x
    -- In some context, removing parentheses from 'x' succeeds. Does
    -- 'x' actually need bracketing in this context?
    f (Just (i, o, gen)) v@(remParens' -> Just x)
      | not $ needBracket' i o x, not $ isPartialAtom x =
          rawIdea' Suggestion msg (getLoc o) (pretty o) (Just (pretty (gen x))) [] [r] : g x
      where
        typ = findType (unLoc v)
        r = Replace typ (toSS' v) [("x", toSS' x)] "x"
    -- Regardless of the context, there are no parentheses to remove
    -- from 'x'.
    f _ x = g x

    g :: (HasSrcSpan a, Data a, Outputable a, Brackets' a) => a -> [Idea]
    -- Enumerate over all the immediate children of 'o' looking for
    -- redundant parentheses in each.
    g o = concat [f (Just (i, o, gen)) x | (i, (x, gen)) <- zip [0..] $ holes o]

bracketWarning :: (HasSrcSpan a, HasSrcSpan b, Data (SrcSpanLess b), Outputable a, Outputable b) => String -> a -> b -> Idea
bracketWarning msg o x =
  suggest' msg o x [Replace (findType (unLoc x)) (toSS' o) [("x", toSS' x)] "x"]

bracketError :: (HasSrcSpan a, HasSrcSpan b, Data (SrcSpanLess b), Outputable a, Outputable b ) => String -> a -> b -> Idea
bracketError msg o x =
  warn' msg o x [Replace (findType (unLoc x)) (toSS' o) [("x", toSS' x)] "x"]

fieldDecl ::  LConDeclField GhcPs -> [Idea]
fieldDecl o@(LL loc f@ConDeclField{cd_fld_type=v@(LL l (HsParTy _ c))}) =
   let r = LL loc (f{cd_fld_type=c}) :: LConDeclField GhcPs in
   [rawIdea' Suggestion "Redundant bracket" loc
    (showSDocUnsafe $ ppr_fld o) -- Note this custom printer!
    (Just (showSDocUnsafe $ ppr_fld r))
    []
    [Replace Type (toSS' v) [("x", toSS' c)] "x"]]
   where
     -- If we call 'unsafePrettyPrint' on a field decl, we won't like
     -- the output (e.g. "[foo, bar] :: T"). Here we use a custom
     -- printer to work around (snarfed from
     -- https://hackage.haskell.org/package/ghc-lib-parser-8.8.1/docs/src/HsTypes.html#pprConDeclFields).
     ppr_fld (LL _ ConDeclField { cd_fld_names = ns, cd_fld_type = ty, cd_fld_doc = doc })
       = ppr_names ns <+> dcolon <+> ppr ty <+> ppr_mbDoc doc
     ppr_fld (LL _ (XConDeclField x)) = ppr x
     ppr_fld _ = undefined -- '{-# COMPLETE LL #-}'

     ppr_names [n] = ppr n
     ppr_names ns = sep (punctuate comma (map ppr ns))
fieldDecl _ = []

-- This function relies heavily on fixities having been applied to the
-- raw parse tree (c.f. 'Util.Refact.Fixings').
dollar :: LHsExpr GhcPs -> [Idea]
dollar = concatMap f . universe
  where
    f x = [ suggest' "Redundant $" x y [r]| o@(LL loc (OpApp _ a d b)) <- [x], isDol' d
            , let y = noLoc (HsApp noExt a b) :: LHsExpr GhcPs
            , not $ needBracket' 0 y a
            , not $ needBracket' 1 y b
            , not $ isPartialAtom b
            , let r = Replace Expr (toSS' x) [("a", toSS' a), ("b", toSS' b)] "a b"]
          ++
          [ suggest' "Move brackets to avoid $" x (t y) [r]
            |(t, e@(LL _ (HsPar _ (LL _ (OpApp _ a1 op1 a2))))) <- splitInfix x
            , isDol' op1
            , isVar' a1 || isApp' a1 || isPar' a1, not $ isAtom' a2
            , varToStr' a1 /= "select" -- special case for esqueleto, see #224
            , let y = noLoc $ HsApp noExt a1 (noLoc (HsPar noExt a2))
            , let r = Replace Expr (toSS' e) [("a", toSS' a1), ("b", toSS' a2)] "a (b)" ]
          ++  -- Special case of (v1 . v2) <$> v3
          [ suggest' "Redundant bracket" x y []
          | LL _ (OpApp _ (LL _ (HsPar _ o1@(LL _ (OpApp _ v1 (isDot' -> True) v2)))) o2 v3) <- [x], varToStr' o2 == "<$>"
          , let y = noLoc (OpApp noExt o1 o2 v3) :: LHsExpr GhcPs]

splitInfix :: LHsExpr GhcPs -> [(LHsExpr GhcPs -> LHsExpr GhcPs, LHsExpr GhcPs)]
splitInfix (LL l (OpApp _ lhs op rhs)) =
  [(LL l . OpApp noExt lhs op, rhs), (\lhs -> LL l (OpApp noExt lhs op rhs), lhs)]
splitInfix _ = []

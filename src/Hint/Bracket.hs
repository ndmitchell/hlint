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
no = f ($ x) y
no = ($ x)
yes = (($ x))  -- @Warning ($ x)
no = ($ 1)
yes = (($ 1)) -- @Warning ($ 1)
no = (+5)
yes = ((+5)) -- @Warning (+5)
issue909 = case 0 of { _ | n <- (0 :: Int) -> n }
issue909 = foo (\((x :: z) -> y) -> 9 + x * 7)
issue909 = foo (\((x : z) -> y) -> 9 + x * 7) -- \(x : z -> y) -> 9 + x * 7
issue909 = let ((x:: y) -> z) = q in q
issue909 = do {((x :: y) -> z) <- e; return 1}
issue970 = (f x +) (g x) -- f x + (g x)
issue969 = (Just \x -> x || x) *> Just True
issue1179 = do(this is a test) -- do this is a test
issue1212 = $(Git.hash)

-- type bracket reduction
foo :: (Int -> Int) -> Int
foo :: (Maybe Int) -> a -- @Suggestion Maybe Int -> a
instance Named (DeclHead S)
data Foo = Foo {foo :: (Maybe Foo)} -- @Suggestion foo :: Maybe Foo

-- pattern bracket reduction
foo (x:xs) = 1
foo (True) = 1 -- @Warning True
foo ((True)) = 1 -- @Warning True
f x = case x of (Nothing) -> 1; _ -> 2 -- Nothing

-- dollar reduction tests
no = groupFsts . sortFst $ mr
yes = split "to" $ names -- split "to" names
yes = white $ keysymbol -- white keysymbol
yes = operator foo $ operator -- operator foo operator
no = operator foo $ operator bar
yes = return $ Record{a=b}
no = f $ [1,2..5] -- f [1,2..5]

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
loadCradleOnlyonce = skipManyTill anyMessage (message @PublishDiagnosticsNotification)
-- These used to require a bracket
$(pure [])
$(x)
-- People aren't a fan of the record constructors being secretly atomic
function (Ctor (Rec { field })) = Ctor (Rec {field = 1})

-- type splices are a bit special
no = f @($x)

-- template haskell is harder
issue1292 = [e| handleForeignCatch $ \ $(varP pylonExPtrVarName) -> $(quoteExp C.block modifiedStr) |]
</TEST>
-}


module Hint.Bracket(bracketHint) where

import Hint.Type(DeclHint,Idea(..),rawIdea,warn,suggest,Severity(..),toRefactSrcSpan,toSSA)
import Data.Data
import Data.List.Extra
import Data.Generics.Uniplate.DataOnly
import Refact.Types

import GHC.Hs
import GHC.Utils.Outputable
import GHC.Types.SrcLoc
import GHC.Util
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Hs.Pat

bracketHint :: DeclHint
bracketHint _ _ x =
  concatMap (\x -> bracket prettyExpr isPartialAtom True x ++ dollar x) (childrenBi (descendBi splices $ descendBi annotations x) :: [LHsExpr GhcPs]) ++
  concatMap (bracket unsafePrettyPrint (\_ _ -> False) False) (childrenBi x :: [LHsType GhcPs]) ++
  concatMap (bracket unsafePrettyPrint (\_ _ -> False) False) (childrenBi x :: [LPat GhcPs]) ++
  concatMap fieldDecl (childrenBi x)
   where
     -- Brackets the roots of annotations are fine, so we strip them.
     annotations :: AnnDecl GhcPs -> AnnDecl GhcPs
     annotations= descendBi $ \x -> case (x :: LHsExpr GhcPs) of
       L _ (HsPar _ _ x _) -> x
       x -> x

     -- Brackets at the root of splices used to be required, but now they aren't
     splices :: HsDecl GhcPs -> HsDecl GhcPs
     splices (SpliceD a x) = SpliceD a $ flip descendBi x $ \x -> case (x :: LHsExpr GhcPs) of
       L _ (HsPar _ _ x _) -> x
       x -> x
     splices x = x

-- If we find ourselves in the context of a section and we want to
-- issue a warning that a child therein has unneccessary brackets,
-- we'd rather report 'Found : (`Foo` (Bar Baz))' rather than 'Found :
-- `Foo` (Bar Baz)'. If left to 'unsafePrettyPrint' we'd get the
-- latter (in contrast to the HSE pretty printer). This patches things
-- up.
prettyExpr :: LHsExpr GhcPs -> String
prettyExpr s@(L _ SectionL{}) = unsafePrettyPrint (nlHsPar s :: LHsExpr GhcPs)
prettyExpr s@(L _ SectionR{}) = unsafePrettyPrint (nlHsPar s :: LHsExpr GhcPs)
prettyExpr x = unsafePrettyPrint x

-- 'Just _' if at least one set of parens were removed. 'Nothing' if
-- zero parens were removed.
remParens' :: Brackets (LocatedA a) => LocatedA a -> Maybe (LocatedA a)
remParens' = fmap go . remParen
  where
    go e = maybe e go (remParen e)

isPartialAtom :: Maybe (LHsExpr GhcPs) -> LHsExpr GhcPs -> Bool
-- Might be '$x', which was really '$ x', but TH enabled misparsed it.
isPartialAtom _ (L _ (HsSpliceE _ (HsTypedSplice _ DollarSplice _ _) )) = True
isPartialAtom _ (L _ (HsSpliceE _ (HsUntypedSplice _ DollarSplice _ _) )) = True
-- Might be '$(x)' where the brackets are required in GHC 8.10 and below
isPartialAtom (Just (L _ HsSpliceE{})) _ = True
isPartialAtom _ x = isRecConstr x || isRecUpdate x

bracket :: forall a . (Data a, Outputable a, Brackets (LocatedA a)) => (LocatedA a -> String) -> (Maybe (LocatedA a) -> LocatedA a -> Bool) -> Bool -> LocatedA a -> [Idea]
bracket pretty isPartialAtom root = f Nothing
  where
    msg = "Redundant bracket"
    -- 'f' is a (generic) function over types in 'Brackets
    -- (expressions, patterns and types). Arguments are, 'f (Maybe
    -- (index, parent, gen)) child'.
    f :: (Data a, Outputable a, Brackets (LocatedA a)) => Maybe (Int, LocatedA a , LocatedA a -> LocatedA a) -> LocatedA a -> [Idea]
    -- No context. Removing parentheses from 'x' succeeds?
    f Nothing o@(remParens' -> Just x)
      -- If at the root, or 'x' is an atom, 'x' parens are redundant.
      | root || isAtom x
      , not $ isPartialAtom Nothing x =
          (if isAtom x then bracketError else bracketWarning) msg o x : g x
    -- In some context, removing parentheses from 'x' succeeds and 'x'
    -- is atomic?
    f (Just (_, p, _)) o@(remParens' -> Just x)
      | isAtom x
      , not $ isPartialAtom (Just p) x =
          bracketError msg o x : g x
    -- In some context, removing parentheses from 'x' succeeds. Does
    -- 'x' actually need bracketing in this context?
    f (Just (i, o, gen)) v@(remParens' -> Just x)
      | not $ needBracket i o x
      , not $ isPartialAtom (Just o) x
      , not $ any isSplicePat $ universeBi o -- over-appoximate ,see #1292
      = rawIdea Suggestion msg (getLocA v) (pretty o) (Just (pretty (gen x))) [] [r] : g x
      where
        typ = findType v
        r = Replace typ (toSSA v) [("x", toSSA x)] "x"
    -- Regardless of the context, there are no parentheses to remove
    -- from 'x'.
    f _ x = g x

    g :: (Data a, Outputable a, Brackets (LocatedA a)) => LocatedA a -> [Idea]
    -- Enumerate over all the immediate children of 'o' looking for
    -- redundant parentheses in each.
    g o = concat [f (Just (i, o, gen)) x | (i, (x, gen)) <- zipFrom 0 $ holes o]

bracketWarning msg o x =
  suggest msg (reLoc o) (reLoc x) [Replace (findType x) (toSSA o) [("x", toSSA x)] "x"]

bracketError :: (Outputable a, Outputable b, Brackets (LocatedA b)) => String -> LocatedA a -> LocatedA b -> Idea
bracketError msg o x =
  warn msg (reLoc o) (reLoc x) [Replace (findType x) (toSSA o) [("x", toSSA x)] "x"]

fieldDecl ::  LConDeclField GhcPs -> [Idea]
fieldDecl o@(L loc f@ConDeclField{cd_fld_type=v@(L l (HsParTy _ c))}) =
   let r = L loc (f{cd_fld_type=c}) :: LConDeclField GhcPs in
   [rawIdea Suggestion "Redundant bracket" (locA l)
    (showSDocUnsafe $ ppr_fld o) -- Note this custom printer!
    (Just (showSDocUnsafe $ ppr_fld r))
    []
    [Replace Type (toSSA v) [("x", toSSA c)] "x"]]
   where
     -- If we call 'unsafePrettyPrint' on a field decl, we won't like
     -- the output (e.g. "[foo, bar] :: T"). Here we use a custom
     -- printer to work around (snarfed from Hs.Types.pprConDeclFields)
     ppr_fld (L _ ConDeclField { cd_fld_names = ns, cd_fld_type = ty, cd_fld_doc = doc })
       = pprMaybeWithDoc doc (ppr_names ns <+> dcolon <+> ppr ty)
     ppr_fld (L _ (XConDeclField x)) = ppr x

     ppr_names [n] = ppr n
     ppr_names ns = sep (punctuate comma (map ppr ns))
fieldDecl _ = []

-- This function relies heavily on fixities having been applied to the
-- raw parse tree.
dollar :: LHsExpr GhcPs -> [Idea]
dollar = concatMap f . universe
  where
    f x = [ (suggest "Redundant $" (reLoc x) (reLoc y) [r]){ideaSpan = locA (getLoc d)} | L _ (OpApp _ a d b) <- [x], isDol d
            , let y = noLocA (HsApp EpAnnNotUsed a b) :: LHsExpr GhcPs
            , not $ needBracket 0 y a
            , not $ needBracket 1 y b
            , not $ isPartialAtom (Just x) b
            , let r = Replace Expr (toSSA x) [("a", toSSA a), ("b", toSSA b)] "a b"]
          ++
          [ suggest "Move brackets to avoid $" (reLoc x) (reLoc (t y)) [r]
            |(t, e@(L _ (HsPar _ _ (L _ (OpApp _ a1 op1 a2)) _))) <- splitInfix x
            , isDol op1
            , isVar a1 || isApp a1 || isPar a1, not $ isAtom a2
            , varToStr a1 /= "select" -- special case for esqueleto, see #224
            , let y = noLocA $ HsApp EpAnnNotUsed a1 (nlHsPar a2)
            , let r = Replace Expr (toSSA e) [("a", toSSA a1), ("b", toSSA a2)] "a (b)" ]
          ++  -- Special case of (v1 . v2) <$> v3
          [ (suggest "Redundant bracket" (reLoc x) (reLoc y) [r]){ideaSpan = locA locPar}
          | L _ (OpApp _ (L locPar (HsPar _ _ o1@(L locNoPar (OpApp _ _ (isDot -> True) _)) _)) o2 v3) <- [x], varToStr o2 == "<$>"
          , let y = noLocA (OpApp EpAnnNotUsed o1 o2 v3) :: LHsExpr GhcPs
          , let r = Replace Expr (toRefactSrcSpan (locA locPar)) [("a", toRefactSrcSpan (locA locNoPar))] "a"]
          ++
          [ suggest "Redundant section" (reLoc x) (reLoc y) [r]
          | L _ (HsApp _ (L _ (HsPar _ _ (L _ (SectionL _ a b)) _)) c) <- [x]
          -- , error $ show (unsafePrettyPrint a, gshow b, unsafePrettyPrint c)
          , let y = noLocA $ OpApp EpAnnNotUsed a b c :: LHsExpr GhcPs
          , let r = Replace Expr (toSSA x) [("x", toSSA a), ("op", toSSA b), ("y", toSSA c)] "x op y"]

splitInfix :: LHsExpr GhcPs -> [(LHsExpr GhcPs -> LHsExpr GhcPs, LHsExpr GhcPs)]
splitInfix (L l (OpApp _ lhs op rhs)) =
  [(L l . OpApp EpAnnNotUsed lhs op, rhs), (\lhs -> L l (OpApp EpAnnNotUsed lhs op rhs), lhs)]
splitInfix _ = []

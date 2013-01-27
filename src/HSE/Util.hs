{-# LANGUAGE FlexibleContexts #-}

module HSE.Util where

import Control.Monad
import Data.List
import Data.Maybe
import System.FilePath
import HSE.Type
import Language.Haskell.Exts.Annotated.Simplify(sQName, sAssoc)


---------------------------------------------------------------------
-- ACCESSOR/TESTER

opExp :: QOp S -> Exp_
opExp (QVarOp s op) = Var s op
opExp (QConOp s op) = Con s op

expOp :: Exp_ -> Maybe (QOp S)
expOp (Var s op) = Just $ QVarOp s op
expOp (Con s op) = Just $ QConOp s op
expOp _ = Nothing

moduleDecls :: Module_ -> [Decl_]
moduleDecls (Module _ _ _ _ xs) = xs

moduleName :: Module_ -> String
moduleName (Module _ Nothing _ _ _) = "Main"
moduleName (Module _ (Just (ModuleHead _ (ModuleName _ x) _ _)) _ _ _) = x

moduleImports :: Module_ -> [ImportDecl S]
moduleImports (Module _ _ _ x _) = x

modulePragmas :: Module_ -> [ModulePragma S]
modulePragmas (Module _ _ x _ _) = x

fromModuleName :: ModuleName S -> String
fromModuleName (ModuleName _ x) = x

isChar :: Exp_ -> Bool
isChar (Lit _ Char{}) = True
isChar _ = False

fromChar :: Exp_ -> Char
fromChar (Lit _ (Char _ x _)) = x

isString :: Exp_ -> Bool
isString (Lit _ String{}) = True
isString _ = False

fromString :: Exp_ -> String
fromString (Lit _ (String _ x _)) = x

isPString (PLit _ String{}) = True; isPString _ = False
fromPString (PLit _ (String _ x _)) = x

fromParen :: Exp_ -> Exp_
fromParen (Paren _ x) = fromParen x
fromParen x = x

fromPParen :: Pat s -> Pat s
fromPParen (PParen _ x) = fromPParen x
fromPParen x = x

fromTyParen :: Type s -> Type s
fromTyParen (TyParen _ x) = fromTyParen x
fromTyParen x = x

-- is* :: Exp_ -> Bool
-- is* :: Decl_ -> Bool
isVar Var{} = True; isVar _ = False
isCon Con{} = True; isCon _ = False
isApp App{} = True; isApp _ = False
isInfixApp InfixApp{} = True; isInfixApp _ = False
isList List{} = True; isList _ = False
isAnyApp x = isApp x || isInfixApp x
isParen Paren{} = True; isParen _ = False
isIf If{} = True; isIf _ = False
isLambda Lambda{} = True; isLambda _ = False
isMDo MDo{} = True; isMDo _ = False
isBoxed Boxed{} = True; isBoxed _ = False
isDerivDecl DerivDecl{} = True; isDerivDecl _ = False
isPBangPat PBangPat{} = True; isPBangPat _ = False
isPExplTypeArg PExplTypeArg{} = True; isPExplTypeArg _ = False
isPFieldPun PFieldPun{} = True; isPFieldPun _ = False
isFieldPun FieldPun{} = True; isFieldPun _ = False
isPWildCard PWildCard{} = True; isPWildCard _ = False
isPFieldWildcard PFieldWildcard{} = True; isPFieldWildcard _ = False
isFieldWildcard FieldWildcard{} = True; isFieldWildcard _ = False
isPViewPat PViewPat{} = True; isPViewPat _ = False
isParComp ParComp{} = True; isParComp _ = False
isPatTypeSig PatTypeSig{} = True; isPatTypeSig _ = False
isQuasiQuote QuasiQuote{} = True; isQuasiQuote _ = False
isSpliceDecl SpliceDecl{} = True; isSpliceDecl _ = False

isSection LeftSection{} = True
isSection RightSection{} = True
isSection _ = False

-- which names are bound by a declaration
declBind :: Decl_ -> [String]
declBind (FunBind _ (Match _ x _ _ _ : _)) = [prettyPrint x]
declBind (PatBind _ x _ _ _) = pvars x
declBind _ = []


allowRightSection x = x `notElem` ["-","#"]
allowLeftSection x = x /= "#"


unqual :: QName S -> QName S
unqual (Qual an _ x) = UnQual an x
unqual x = x

fromQual :: QName S -> Name S
fromQual (Qual _ _ x) = x
fromQual (UnQual _ x) = x

isSpecial :: QName S -> Bool
isSpecial Special{} = True; isSpecial _ = False

isDol :: QOp S -> Bool
isDol (QVarOp _ (UnQual _ (Symbol _ "$"))) = True
isDol _ = False

isDot :: QOp S -> Bool
isDot (QVarOp _ (UnQual _ (Symbol _ "."))) = True
isDot _ = False

isDotApp :: Exp_ -> Bool
isDotApp (InfixApp _ _ dot _) | isDot dot = True
isDotApp _ = False

dotApp :: Exp_ -> Exp_ -> Exp_
dotApp x = InfixApp an x (QVarOp an $ UnQual an $ Symbol an ".")

dotApps :: [Exp_] -> Exp_
dotApps [x] = x
dotApps (x:xs) = dotApp x (dotApps xs)


isLexeme Var{} = True
isLexeme Con{} = True
isLexeme Lit{} = True
isLexeme _ = False


isWHNF :: Exp_ -> Bool
isWHNF Con{} = True
isWHNF Lit{} = True
isWHNF Lambda{} = True
isWHNF Tuple{} = True
isWHNF List{} = True
isWHNF (Paren _ x) = isWHNF x
isWHNF RecConstr{} = True
isWHNF (ExpTypeSig _ x _) = isWHNF x
isWHNF _ = False


---------------------------------------------------------------------
-- HSE FUNCTIONS


getEquations :: Decl s -> [Decl s]
getEquations (FunBind s xs) = map (FunBind s . (:[])) xs
getEquations x@PatBind{} = [toFunBind x]
getEquations x = [x]


toFunBind :: Decl s -> Decl s
toFunBind (PatBind s (PVar _ name) _ bod bind) = FunBind s [Match s name [] bod bind]
toFunBind x = x


fromGuardedAlts :: GuardedAlts s -> Rhs s
fromGuardedAlts (UnGuardedAlt s x) = UnGuardedRhs s x
fromGuardedAlts (GuardedAlts s xs) = GuardedRhss s [GuardedRhs a b c | GuardedAlt a b c <- xs]

toGuardedAlts :: Rhs s -> GuardedAlts s
toGuardedAlts (UnGuardedRhs s x) = UnGuardedAlt s x
toGuardedAlts (GuardedRhss s xs) = GuardedAlts s [GuardedAlt a b c | GuardedRhs a b c <- xs]


-- case and if both have branches, nothing else does
replaceBranches :: Exp s -> ([Exp s], [Exp s] -> Exp s)
replaceBranches (If s a b c) = ([b,c], \[b,c] -> If s a b c)
replaceBranches (Case s a bs) = (concatMap f bs, Case s a . g bs)
    where
        f (Alt _ _ (UnGuardedAlt _ x) _) = [x]
        f (Alt _ _ (GuardedAlts _ xs) _) = [x | GuardedAlt _ _ x <- xs]
        g (Alt s1 a (UnGuardedAlt s2 _) b:rest) (x:xs) = Alt s1 a (UnGuardedAlt s2 x) b : g rest xs
        g (Alt s1 a (GuardedAlts s2 ns) b:rest) xs =
                Alt s1 a (GuardedAlts s2 [GuardedAlt a b x | (GuardedAlt a b _,x) <- zip ns as]) b : g rest bs
            where (as,bs) = splitAt (length ns) xs
        g [] [] = []
replaceBranches x = ([], \[] -> x)


---------------------------------------------------------------------
-- VECTOR APPLICATION


apps :: [Exp_] -> Exp_
apps = foldl1 (App an)


fromApps :: Exp_ -> [Exp_]
fromApps (App _ x y) = fromApps x ++ [y]
fromApps x = [x]


-- Rule for the Uniplate Apps functions
-- Given (f a) b, consider the children to be: children f ++ [a,b]

childrenApps :: Exp_ -> [Exp_]
childrenApps (App _ x@App{} y) = childrenApps x ++ [y]
childrenApps (App _ x y) = children x ++ [y]
childrenApps x = children x


descendApps :: (Exp_ -> Exp_) -> Exp_ -> Exp_
descendApps f (App s x@App{} y) = App s (descendApps f x) (f y)
descendApps f (App s x y) = App s (descend f x) (f y)
descendApps f x = descend f x


descendAppsM :: Monad m => (Exp_ -> m Exp_) -> Exp_ -> m Exp_
descendAppsM f (App s x@App{} y) = liftM2 (App s) (descendAppsM f x) (f y)
descendAppsM f (App s x y) = liftM2 (App s) (descendM f x) (f y)
descendAppsM f x = descendM f x


universeApps :: Exp_ -> [Exp_]
universeApps x = x : concatMap universeApps (childrenApps x)

transformApps :: (Exp_ -> Exp_) -> Exp_ -> Exp_
transformApps f = f . descendApps (transformApps f)

transformAppsM :: (Monad m) => (Exp_ -> m Exp_) -> Exp_ -> m Exp_
transformAppsM f x = f =<< descendAppsM (transformAppsM f) x


---------------------------------------------------------------------
-- UNIPLATE FUNCTIONS

universeS :: Biplate x (f S) => x -> [f S]
universeS = universeBi

childrenS :: Biplate x (f S) => x -> [f S]
childrenS = childrenBi


vars :: Biplate a Exp_ => a -> [String]
vars xs = [prettyPrint x | Var _ (UnQual _ x) <- universeS xs]

pvars :: Biplate a Pat_ => a -> [String]
pvars xs = [prettyPrint x | PVar _ x <- universeS xs]


-- return the parent along with the child
universeParentExp :: Biplate a Exp_ => a -> [(Maybe (Int, Exp_), Exp_)]
universeParentExp xs = concat [(Nothing, x) : f x | x <- childrenBi xs]
    where f p = concat [(Just (i,p), c) : f c | (i,c) <- zip [0..] $ children p]


---------------------------------------------------------------------
-- SRCLOC FUNCTIONS

showSrcLoc :: SrcLoc -> String
showSrcLoc (SrcLoc file line col) = take 1 file ++ f (drop 1 file) ++ ":" ++ show line ++ ":" ++ show col
    where f (x:y:zs) | isPathSeparator x && isPathSeparator y = f $ x:zs
          f (x:xs) = x : f xs
          f [] = []

toSrcLoc :: SrcInfo si => si -> SrcLoc
toSrcLoc = getPointLoc

nullSrcLoc :: SrcLoc
nullSrcLoc = SrcLoc "" 0 0

an :: SrcSpanInfo
an = toSrcInfo nullSrcLoc [] nullSrcLoc

dropAnn :: Functor f => f s -> f ()
dropAnn = fmap (const ())


---------------------------------------------------------------------
-- SRCLOC EQUALITY

-- enforce all being on S, as otherwise easy to =~= on a Just, and get the wrong functor

x /=~= y = not $ x =~= y

elem_, notElem_ :: (Annotated f, Eq (f ())) => f S -> [f S] -> Bool
elem_ x = any (x =~=)
notElem_ x = not . elem_ x

nub_ :: (Annotated f, Eq (f ())) => [f S] -> [f S]
nub_ = nubBy (=~=)

intersect_ :: (Annotated f, Eq (f ())) => [f S] -> [f S] -> [f S]
intersect_ = intersectBy (=~=)

eqList, neqList :: (Annotated f, Eq (f ())) => [f S] -> [f S] -> Bool
neqList x y = not $ eqList x y
eqList (x:xs) (y:ys) = x =~= y && eqList xs ys
eqList [] [] = True
eqList _ _ = False

eqMaybe:: (Annotated f, Eq (f ())) => Maybe (f S) -> Maybe (f S) -> Bool
eqMaybe (Just x) (Just y) = x =~= y
eqMaybe Nothing Nothing = True
eqMaybe _ _ = False


---------------------------------------------------------------------
-- FIXITIES

getFixity :: Decl a -> [Fixity]
getFixity (InfixDecl sl a mp ops) = [Fixity (sAssoc a) (fromMaybe 9 mp) (sQName $ UnQual sl $ f op) | op <- ops]
    where f (VarOp _ x) = x
          f (ConOp _ x) = x
getFixity _ = []

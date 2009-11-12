{-# LANGUAGE PatternGuards #-}

module HSE.Util where

import Control.Monad
import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Language.Haskell.Exts
import Util


---------------------------------------------------------------------
-- ACCESSOR/TESTER

opExp :: QOp -> Exp
opExp (QVarOp op) = Var op
opExp (QConOp op) = Con op

moduleDecls :: Module -> [Decl]
moduleDecls (Module _ _ _ _ _ _ xs) = xs

moduleName :: Module -> String
moduleName (Module _ (ModuleName x) _ _ _ _ _) = x

moduleImports :: Module -> [ImportDecl]
moduleImports (Module _ _ _ _ _ x _) = x

modulePragmas :: Module -> [OptionPragma]
modulePragmas (Module _ _ x _ _ _ _) = x

isChar :: Exp -> Bool
isChar (Lit (Char _)) = True
isChar _ = False

fromChar :: Exp -> Char
fromChar (Lit (Char x)) = x

isString :: Exp -> Bool
isString (Lit (String _)) = True
isString _ = False

fromString :: Exp -> String
fromString (Lit (String x)) = x

isPString (PLit (String _)) = True; isPString _ = False
fromPString (PLit (String x)) = x

fromParen :: Exp -> Exp
fromParen (Paren x) = fromParen x
fromParen x = x

-- is* :: Exp -> Bool
isVar Var{} = True; isVar _ = False
isApp App{} = True; isApp _ = False
isInfixApp InfixApp{} = True; isInfixApp _ = False
isAnyApp x = isApp x || isInfixApp x
isParen Paren{} = True; isParen _ = False
isLambda Lambda{} = True; isLambda _ = False
isMDo MDo{} = True; isMDo _ = False
isBoxed Boxed{} = True; isBoxed _ = False
isDerivDecl DerivDecl{} = True; isDerivDecl _ = False
isFunDep FunDep{} = True; isFunDep _ = False
isPBangPat PBangPat{} = True; isPBangPat _ = False
isPExplTypeArg PExplTypeArg{} = True; isPExplTypeArg _ = False
isPFieldPun PFieldPun{} = True; isPFieldPun _ = False
isPFieldWildcard PFieldWildcard{} = True; isPFieldWildcard _ = False
isPViewPat PViewPat{} = True; isPViewPat _ = False
isParComp ParComp{} = True; isParComp _ = False
isPatTypeSig PatTypeSig{} = True; isPatTypeSig _ = False
isQuasiQuote QuasiQuote{} = True; isQuasiQuote _ = False


-- which names are bound by a declaration
declBind :: Decl -> [Name]
declBind (FunBind (Match _ x _ _ _ _ : _)) = [x]
declBind (PatBind _ x _ _ _) = [x | PVar x <- universe x]
declBind _ = []

---------------------------------------------------------------------
-- HSE FUNCTIONS

-- pick a variable that is not being used
freeVar :: Data a => a -> String
freeVar x = head $ allVars \\ concat [[y, drop 1 y] | Ident y <- universeBi x]
    where allVars = [letter : number | number <- "" : map show [1..], letter <- ['a'..'z']]


getEquations :: Decl -> [Decl]
getEquations (FunBind xs) = map (FunBind . (:[])) xs
getEquations (PatBind src (PVar name) typ bod bind) = [FunBind [Match src name [] typ bod bind]]
getEquations x = [x]


---------------------------------------------------------------------
-- VECTOR APPLICATION


apps :: [Exp] -> Exp
apps = foldl1 App


fromApps :: Exp -> [Exp]
fromApps (App x y) = fromApps x ++ [y]
fromApps x = [x]


-- Rule for the Uniplate Apps functions
-- Given (f a) b, consider the children to be: children f ++ [a,b]

childrenApps :: Exp -> [Exp]
childrenApps (App x@App{} y) = childrenApps x ++ [y]
childrenApps (App x y) = children x ++ [y]
childrenApps x = children x


descendApps :: (Exp -> Exp) -> Exp -> Exp
descendApps f (App x@App{} y) = App (descendApps f x) (f y)
descendApps f (App x y) = App (descend f x) (f y)
descendApps f x = descend f x


descendAppsM :: Monad m => (Exp -> m Exp) -> Exp -> m Exp
descendAppsM f (App x@App{} y) = liftM2 App (descendAppsM f x) (f y)
descendAppsM f (App x y) = liftM2 App (descendM f x) (f y)
descendAppsM f x = descendM f x


universeApps :: Exp -> [Exp]
universeApps x = x : concatMap universeApps (childrenApps x)

transformApps :: (Exp -> Exp) -> Exp -> Exp
transformApps f = f . descendApps (transformApps f)

transformAppsM :: Monad m => (Exp -> m Exp) -> Exp -> m Exp
transformAppsM f x = f =<< descendAppsM (transformAppsM f) x


---------------------------------------------------------------------
-- SRCLOC FUNCTIONS

nullSrcLoc :: SrcLoc
nullSrcLoc = SrcLoc "" 0 0

showSrcLoc :: SrcLoc -> String
showSrcLoc (SrcLoc file line col) = file ++ ":" ++ show line ++ ":" ++ show col ++ ":"

getSrcLoc :: Data a => a -> Maybe SrcLoc
getSrcLoc = headDef Nothing . gmapQ cast


declSrcLoc :: Decl -> SrcLoc
declSrcLoc (FunBind (x:xs)) = fromMaybe nullSrcLoc $ getSrcLoc x
declSrcLoc x = fromMaybe nullSrcLoc $ getSrcLoc x

---------------------------------------------------------------------
-- UNIPLATE STYLE FUNCTIONS

-- children on Exp, but with SrcLoc's
children1Exp :: Data a => SrcLoc -> a -> [(SrcLoc, Exp)]
children1Exp src x = concat $ gmapQ (children0Exp src2) x
    where src2 = fromMaybe src (getSrcLoc x)

children0Exp :: Data a => SrcLoc -> a -> [(SrcLoc, Exp)]
children0Exp src x | Just y <- cast x = [(src, y)]
                   | otherwise = children1Exp src x

universeExp :: Data a => SrcLoc -> a -> [(SrcLoc, Exp)]
universeExp src x = concatMap f (children0Exp src x)
    where f (src,x) = (src,x) : concatMap f (children1Exp src x)


{-# LANGUAGE PatternGuards, ViewPatterns, MultiParamTypeClasses #-}

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

fromParseOk :: ParseResult a -> a
fromParseOk (ParseOk x) = x

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
isApp App{} = True; isApp _ = False
isInfixApp InfixApp{} = True; isInfixApp _ = False
isAnyApp x = isApp x || isInfixApp x
isParen Paren{} = True; isParen _ = False
isListComp ListComp{} = True; isListComp _ = False
isIf If{} = True; isIf _ = False


---------------------------------------------------------------------
-- HSE FUNCTIONS


instance Eq Module where
    Module x1 x2 x3 x4 x5 x6 x7 == Module y1 y2 y3 y4 y5 y6 y7 =
        x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4 && x5 == y5 && x6 == y6 && x7 == y7


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


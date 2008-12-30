{-# LANGUAGE PatternGuards, ViewPatterns, MultiParamTypeClasses #-}

module HSE.Util where

import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Language.Haskell.Exts


---------------------------------------------------------------------
-- GENERAL FUNCTIONS

headDef :: a -> [a] -> a
headDef x [] = x
headDef x (y:ys) = y


limit :: Int -> String -> String
limit n s = if null post then s else pre ++ "..."
    where (pre,post) = splitAt n s


---------------------------------------------------------------------
-- ACCESSOR/TESTER

fromName :: Name -> String
fromName (Ident x) = x
fromName (Symbol x) = x

toName :: String -> Name
toName x = Ident x

toQName :: String -> QName
toQName = UnQual . toName

opExp ::  QOp -> Exp
opExp (QVarOp op) = Var op
opExp (QConOp op) = Con op

moduleDecls :: Module -> [Decl]
moduleDecls (Module _ _ _ _ _ _ xs) = xs

moduleName :: Module -> String
moduleName (Module _ (ModuleName x) _ _ _ _ _) = x

fromParseOk :: ParseResult a -> a
fromParseOk (ParseOk x) = x

fromVar :: Exp -> Maybe String
fromVar (Var (UnQual x)) = Just $ fromName x
fromVar _ = Nothing

fromPVar :: Pat -> Maybe String
fromPVar (PVar x) = Just $ fromName x
fromPVar _ = Nothing

isVar :: Exp -> Bool
isVar = isJust . fromVar

toVar :: String -> Exp
toVar = Var . UnQual . Ident

isChar :: Exp -> Bool
isChar (Lit (Char _)) = True
isChar _ = False

fromChar :: Exp -> Char
fromChar (Lit (Char x)) = x

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

declName :: Decl -> String
declName (TypeDecl _ name _ _) = fromName name
declName (DataDecl _ _ _ name _ _ _) = fromName name
declName (GDataDecl _ _ _ name _ _ _ _) = fromName name
declName (TypeFamDecl _ name _ _) = fromName name
declName (DataFamDecl _ _ name _ _) = fromName name
declName (ClassDecl _ _ name _ _ _) = fromName name
declName (PatBind _ (PVar name) _ _) = fromName name
declName (FunBind (Match _ name _ _ _ : _)) = fromName name
declName (ForImp _ _ _ _ name _) = fromName name
declName (ForExp _ _ _ name _) = fromName name
declName _ = ""


instance Eq Module where
    Module x1 x2 x3 x4 x5 x6 x7 == Module y1 y2 y3 y4 y5 y6 y7 =
        x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4 && x5 == y5 && x6 == y6 && x7 == y7


-- pick a variable that is not being used
freeVar :: Data a => a -> String
freeVar x = head $ allVars \\ concat [[y, drop 1 y] | Ident y <- universeBi x]
    where allVars = [letter : number | number <- "" : map show [1..], letter <- ['a'..'z']]


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


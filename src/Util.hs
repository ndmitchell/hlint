{-# LANGUAGE PatternGuards, ViewPatterns, MultiParamTypeClasses #-}

module Util where

import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Language.Haskell.Exts


headDef :: a -> [a] -> a
headDef x [] = x
headDef x (y:ys) = y


declName :: Decl -> String
declName (PatBind _ (PVar (Ident name)) _ _) = name
declName (FunBind (Match _ (Ident name) _ _ _ : _)) = name
declName x = error $ "declName: " ++ show x


parseHsModule :: FilePath -> IO Module
parseHsModule file = do
    res <- parseFile file
    case res of
        ParseOk x -> return x
        ParseFailed src msg -> do
            putStrLn $ showSrcLoc src ++ " Parse failure, " ++ limit 50 msg
            return $ Module nullSrcLoc (ModuleName "") Nothing [] []


limit :: Int -> String -> String
limit n s = if null post then s else pre ++ "..."
    where (pre,post) = splitAt n s


---------------------------------------------------------------------
-- SRCLOC FUNCTIONS

nullSrcLoc :: SrcLoc
nullSrcLoc = SrcLoc "" 0 0

showSrcLoc :: SrcLoc -> String
showSrcLoc (SrcLoc file line col) = file ++ ":" ++ show line ++ ":" ++ show col ++ ":"

getSrcLoc :: Data a => a -> Maybe SrcLoc
getSrcLoc x = headDef Nothing $ gmapQ cast x


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


---------------------------------------------------------------------
-- VARIABLE MANIPULATION

-- pick a variable that is not being used
freeVar :: Data a => a -> String
freeVar x = head $ allVars \\ concat [[y, drop 1 y] | Ident y <- universeBi x]
    where allVars = [letter : number | number <- "" : map show [1..], letter <- ['a'..'z']]


fromVar :: Exp -> Maybe String
fromVar (Var (UnQual (Ident x))) = Just x
fromVar (Var (UnQual (Symbol x))) = Just x
fromVar _ = Nothing

toVar :: String -> Exp
toVar = Var . UnQual . Ident

isVar :: Exp -> Bool
isVar = isJust . fromVar


isCharExp :: Exp -> Bool
isCharExp (Lit (Char _)) = True
isCharExp _ = False


----------------------------------------------------------------------
-- BRACKETS

addParen, hsParen :: Exp -> Exp
addParen x = if atom x then x else XExpTag x
hsParen x = if atom x then x else Paren x

remParen :: Exp -> Exp
remParen = transform g . transform f
    where
        g (XExpTag x) = Paren x
        g x = x
    
        f (XExpTag x) | atom x = x
        f (InfixApp a b c) = InfixApp (f2 a) b (f2 c)
        f x = x
        
        f2 (XExpTag (App a b)) = App a b
        f2 x = x

isParen :: Exp -> Bool
isParen (Paren _) = True
isParen (XExpTag _) = True
isParen _ = False

fromParen :: Exp -> Exp
fromParen (Paren x) = fromParen x
fromParen (XExpTag x) = fromParen x
fromParen x = x

atom x = case x of
  Paren _ -> True
  Var _ -> True
  Con _ -> True
  Lit _ -> True
  Tuple _ -> True
  List _ -> True
  LeftSection _ _ -> True
  RightSection _ _ -> True
  RecConstr _ _ -> True
  ListComp _ _ -> True
  _ -> False


---------------------------------------------------------------------
-- PATTERN MATCHING

class View a b where
    view :: a -> b


data App2 = NoApp2 | App2 Exp Exp Exp deriving Show

instance View Exp App2 where
    view (InfixApp lhs op rhs) = view $ f op `App` lhs `App` rhs
        where f (QVarOp op) = Var op
              f (QConOp op) = Con op
    view (f `App` x `App` y) = App2 f x y
    view _ = NoApp2


(~=) :: Exp -> String -> Bool
(Con (Special Cons)) ~= ":" = True
(List []) ~= "[]" = True
x ~= y = fromVar x == Just y

{-# LANGUAGE PatternGuards, ViewPatterns, MultiParamTypeClasses #-}

module Hint.Util where

import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Language.Haskell.Exts


headDef :: a -> [a] -> a
headDef x [] = x
headDef x (y:ys) = y

ifNull :: [a] -> [a] -> [a]
ifNull x y = if null x then y else x


declName :: HsDecl -> String
declName (HsPatBind _ (HsPVar (HsIdent name)) _ _) = name
declName (HsFunBind (HsMatch _ (HsIdent name) _ _ _ : _)) = name
declName x = error $ "declName: " ++ show x


parseHsModule :: FilePath -> IO HsModule
parseHsModule file = do
    res <- parseFile file
    case res of
        ParseOk x -> return x
        ParseFailed src msg -> do
            putStrLn $ showSrcLoc src ++ " Parse failure, " ++ msg
            return $ HsModule nullSrcLoc (Module "") Nothing [] []


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
children1Exp :: Data a => SrcLoc -> a -> [(SrcLoc, HsExp)]
children1Exp src x = concat $ gmapQ (children0Exp src2) x
    where src2 = fromMaybe src (getSrcLoc x)

children0Exp :: Data a => SrcLoc -> a -> [(SrcLoc, HsExp)]
children0Exp src x | Just y <- cast x = [(src, y)]
                   | otherwise = children1Exp src x

universeExp :: Data a => SrcLoc -> a -> [(SrcLoc, HsExp)]
universeExp src x = concatMap f (children0Exp src x)
    where f (src,x) = (src,x) : concatMap f (children1Exp src x)


---------------------------------------------------------------------
-- VARIABLE MANIPULATION

-- pick a variable that is not being used
freeVar :: Data a => a -> String
freeVar x = head $ allVars \\ concat [[y, drop 1 y] | HsIdent y <- universeBi x]
    where allVars = [letter : number | number <- "" : map show [1..], letter <- ['a'..'z']]


fromVar :: HsExp -> Maybe String
fromVar (HsVar (UnQual (HsIdent x))) = Just x
fromVar (HsVar (UnQual (HsSymbol x))) = Just x
fromVar _ = Nothing

toVar :: String -> HsExp
toVar = HsVar . UnQual . HsIdent

isVar :: HsExp -> Bool
isVar = isJust . fromVar


isCharExp :: HsExp -> Bool
isCharExp (HsLit (HsChar _)) = True
isCharExp _ = False


isParen :: HsExp -> Bool
isParen (HsParen _) = True
isParen _ = False

----------------------------------------------------------------------
-- BRACKETS

addParen :: HsExp -> HsExp
addParen x = if atom x then x else HsXExpTag x

remParen :: HsExp -> HsExp
remParen = transform g . transform f
    where
        g (HsXExpTag x) = HsParen x
        g x = x
    
        f (HsXExpTag x) | atom x = x
        f (HsInfixApp a b c) = HsInfixApp (f2 a) b (f2 c)
        f x = x
        
        f2 (HsXExpTag (HsApp a b)) = HsApp a b
        f2 x = x


atom x = case x of
  HsParen _ -> True
  HsVar _ -> True
  HsCon _ -> True
  HsLit _ -> True
  HsTuple _ -> True
  HsList _ -> True
  HsLeftSection _ _ -> True
  HsRightSection _ _ -> True
  HsRecConstr _ _ -> True
  HsListComp _ _ -> True
  _ -> False


---------------------------------------------------------------------
-- PATTERN MATCHING

class View a b where
    view :: a -> b


data Nil = NoNil | Nil deriving Show

instance View HsExp Nil where
    view (HsList []) = Nil
    view _ = NoNil


data Cons = NoCons | Cons HsExp HsExp deriving Show

instance View HsExp Cons where
    view (view -> App2 f x y) | f ~= ":" = Cons x y
    view _ = NoCons


data App2 = NoApp2 | App2 HsExp HsExp HsExp deriving Show

instance View HsExp App2 where
    view (HsInfixApp lhs op rhs) = view $ f op `HsApp` lhs `HsApp` rhs
        where f (HsQVarOp op) = HsVar op
              f (HsQConOp op) = HsCon op
    view (f `HsApp` x `HsApp` y) = App2 f x y
    view _ = NoApp2


(~=) :: HsExp -> String -> Bool
(HsCon (Special HsCons)) ~= ":" = True
x ~= y = fromVar x == Just y

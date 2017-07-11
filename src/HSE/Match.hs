{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}

module HSE.Match(
    View(..), Named(..),
    (~=), isSym,
    App2(App2), LamConst1(LamConst1), PVar_(PVar_), Var_(Var_), PApp_(PApp_)
    ) where

import Data.Char
import HSE.Type
import HSE.Util


class View a b where
    view :: a -> b


data App2 = NoApp2 | App2 Exp_ Exp_ Exp_ deriving Show

instance View Exp_ App2 where
    view (fromParen -> InfixApp _ lhs op rhs) = App2 (opExp op) lhs rhs
    view (fromParen -> App _ (fromParen -> App _ f x) y) = App2 f x y
    view _ = NoApp2


data App1 = NoApp1 | App1 Exp_ Exp_ deriving Show

instance View Exp_ App1 where
    view (fromParen -> App _ f x) = App1 f x
    view _ = NoApp1

-- \_ -> body
data LamConst1 = NoLamConst1 | LamConst1 Exp_ deriving Show

instance View Exp_ LamConst1 where
    view (fromParen -> Lambda _ [PWildCard _] x) = LamConst1 x
    view _ = NoLamConst1

data PApp_ = NoPApp_ | PApp_ String [Pat_]

instance View Pat_ PApp_ where
    view (fromPParen -> PApp _ x xs) = PApp_ (fromNamed x) xs
    view (fromPParen -> PInfixApp _ lhs op rhs) = PApp_ (fromNamed op) [lhs, rhs]
    view _ = NoPApp_

data PVar_ = NoPVar_ | PVar_ String

instance View Pat_ PVar_ where
    view (fromPParen -> PVar _ x) = PVar_ $ fromNamed x
    view _ = NoPVar_

data Var_ = NoVar_ | Var_ String deriving Eq

instance View Exp_ Var_ where
    view (fromParen -> Var _ (UnQual _ x)) = Var_ $ fromNamed x
    view _ = NoVar_


(~=) :: Named a => a -> String -> Bool
(~=) = (==) . fromNamed


-- | fromNamed will return \"\" when it cannot be represented
--   toNamed may crash on \"\"
class Named a where
    toNamed :: String -> a
    fromNamed :: a -> String


isCtor (x:_) = isUpper x || x == ':'
isCtor _ = False

isSym (x:_) = not $ isAlpha x || x `elem` "_'"
isSym _ = False


instance Named (Exp S) where
    fromNamed (Var _ x) = fromNamed x
    fromNamed (Con _ x) = fromNamed x
    fromNamed (List _ []) = "[]"
    fromNamed _ = ""

    toNamed "[]" = List an []
    toNamed x | isCtor x = Con an $ toNamed x
              | otherwise = Var an $ toNamed x

instance Named (QName S) where
    fromNamed (Special _ Cons{}) = ":"
    fromNamed (Special _ UnitCon{}) = "()"
    fromNamed (UnQual _ x) = fromNamed x
    fromNamed _ = ""

    toNamed ":" = Special an $ Cons an
    toNamed x = UnQual an $ toNamed x

instance Named (Name S) where
    fromNamed (Ident _ x) = x
    fromNamed (Symbol _ x) = x

    toNamed x | isSym x = Symbol an x
              | otherwise = Ident an x

instance Named (ModuleName S) where
    fromNamed (ModuleName _ x) = x
    toNamed = ModuleName an


instance Named (Pat S) where
    fromNamed (PVar _ x) = fromNamed x
    fromNamed (PApp _ x []) = fromNamed x
    fromNamed (PList _ []) = "[]"
    fromNamed _ = ""

    toNamed x | isCtor x = PApp an (toNamed x) []
              | otherwise = PVar an $ toNamed x


instance Named (TyVarBind S) where
    fromNamed (KindedVar _ x _) = fromNamed x
    fromNamed (UnkindedVar _ x) = fromNamed x
    toNamed x = UnkindedVar an (toNamed x)


instance Named (QOp S) where
    fromNamed (QVarOp _ x) = fromNamed x
    fromNamed (QConOp _ x) = fromNamed x
    toNamed x | isCtor x = QConOp an $ toNamed x
              | otherwise = QVarOp an $ toNamed x

instance Named (Match S) where
    fromNamed (Match _ x _ _ _) = fromNamed x
    fromNamed (InfixMatch _ _ x _ _ _) = fromNamed x
    toNamed = error "No toNamed for Match"

instance Named (DeclHead S) where
    fromNamed (DHead _ x) = fromNamed x
    fromNamed (DHInfix _ _ x) = fromNamed x
    fromNamed (DHParen _ x) = fromNamed x
    fromNamed (DHApp _ x _) = fromNamed x
    toNamed = error "No toNamed for DeclHead"

instance Named (Decl S) where
    fromNamed (TypeDecl _ name _) = fromNamed name
    fromNamed (DataDecl _ _ _ name _ _) = fromNamed name
    fromNamed (GDataDecl _ _ _ name _ _ _) = fromNamed name
    fromNamed (TypeFamDecl _ name _ _) = fromNamed name
    fromNamed (DataFamDecl _ _ name _) = fromNamed name
    fromNamed (ClassDecl _ _ name _ _) = fromNamed name
    fromNamed (PatBind _ (PVar _ name) _ _) = fromNamed name
    fromNamed (FunBind _ (name:_)) = fromNamed name
    fromNamed (ForImp _ _ _ _ name _) = fromNamed name
    fromNamed (ForExp _ _ _ name _) = fromNamed name
    fromNamed (TypeSig _ (name:_) _) = fromNamed name
    fromNamed _ = ""

    toNamed = error "No toNamed for Decl"

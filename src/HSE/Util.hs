{-# LANGUAGE FlexibleContexts, TupleSections #-}

module HSE.Util(
    fromParen, fromPParen, an, moduleImports, fromModuleName, getFixity, moduleDecls, isAssocNone,
    isAssocLeft, showSrcLoc, isLambda, isIf, unqual, childrenS, getEquations, modulePragmas, opExp,
    fromApps, fromString, fromPString, toInfixDecl, isDol, apps, isDot, isAnyApp, extensionImpliedBy,
    extensionImplies, moduleName,  moduleExtensions,
    ) where

import Control.Monad
import Data.Default
import Data.Tuple.Extra
import Data.List.Extra
import Language.Haskell.Exts.Util() -- Default for SrcSpanInfo
import qualified Data.Map as Map
import Data.Maybe
import Data.Data hiding (Fixity)
import System.FilePath
import HSE.Type
import Data.Functor
import Prelude
import qualified Language.Haskell.GhclibParserEx.DynFlags as GhclibParserEx

---------------------------------------------------------------------
-- ACCESSOR/TESTER

opExp :: QOp S -> Exp_
opExp (QVarOp s op) = Var s op
opExp (QConOp s op) = Con s op

moduleDecls :: Module_ -> [Decl_]
moduleDecls (Module _ _ _ _ xs) = xs
moduleDecls _ = [] -- XmlPage/XmlHybrid

moduleName :: Module_ -> String
moduleName (Module _ Nothing _ _ _) = "Main"
moduleName (Module _ (Just (ModuleHead _ (ModuleName _ x) _ _)) _ _ _) = x
moduleName _ = "" -- XmlPage/XmlHybrid

moduleImports :: Module_ -> [ImportDecl S]
moduleImports (Module _ _ _ x _) = x
moduleImports _ = [] -- XmlPage/XmlHybrid

modulePragmas :: Module_ -> [ModulePragma S]
modulePragmas (Module _ _ x _ _) = x
modulePragmas _ = [] -- XmlPage/XmlHybrid

moduleExtensions :: Module_ -> [Name S]
moduleExtensions x = concat [y | LanguagePragma _ y <- modulePragmas x]

fromModuleName :: ModuleName S -> String
fromModuleName (ModuleName _ x) = x

fromString :: Exp_ -> Maybe String
fromString (Lit _ (String _ x _)) = Just x
fromString _ = Nothing

fromPString :: Pat_ -> Maybe String
fromPString (PLit _ _ (String _ x _)) =  Just x
fromPString _ = Nothing

fromParen :: Exp_ -> Exp_
fromParen (Paren _ x) = fromParen x
fromParen x = x

fromPParen :: Pat s -> Pat s
fromPParen (PParen _ x) = fromPParen x
fromPParen x = x

-- is* :: Exp_ -> Bool
-- is* :: Decl_ -> Bool
isApp App{} = True; isApp _ = False
isInfixApp InfixApp{} = True; isInfixApp _ = False
isAnyApp x = isApp x || isInfixApp x
isIf If{} = True; isIf _ = False
isLambda Lambda{} = True; isLambda _ = False


unqual :: QName S -> QName S
unqual (Qual an _ x) = UnQual an x
unqual x = x

fromQual :: QName a -> Maybe (Name a)
fromQual (Qual _ _ x) = Just x
fromQual (UnQual _ x) = Just x
fromQual _ = Nothing

isDol :: QOp S -> Bool
isDol (QVarOp _ (UnQual _ (Symbol _ "$"))) = True
isDol _ = False

isDot :: QOp S -> Bool
isDot (QVarOp _ (UnQual _ (Symbol _ "."))) = True
isDot _ = False

isAssocLeft AssocLeft{} = True; isAssocLeft _ = False
isAssocNone AssocNone{} = True; isAssocNone _ = False


---------------------------------------------------------------------
-- HSE FUNCTIONS

getEquations :: Decl s -> [Decl s]
getEquations (FunBind s xs) = map (FunBind s . (:[])) xs
getEquations x@PatBind{} = [toFunBind x]
getEquations x = [x]


toFunBind :: Decl s -> Decl s
toFunBind (PatBind s (PVar _ name) bod bind) = FunBind s [Match s name [] bod bind]
toFunBind x = x


---------------------------------------------------------------------
-- VECTOR APPLICATION


apps :: [Exp_] -> Exp_
apps = foldl1 (App an)

fromApps :: Exp_ -> [Exp_]
fromApps = map fst . fromAppsWithLoc

fromAppsWithLoc :: Exp_ -> [(Exp_, S)]
fromAppsWithLoc (App l x y) = fromAppsWithLoc x ++ [(y, l)]
fromAppsWithLoc x = [(x, ann x)]



childrenS :: (Data x, Data (f S)) => x -> [f S]
childrenS = childrenBi

---------------------------------------------------------------------
-- SRCLOC FUNCTIONS

showSrcLoc :: SrcLoc -> String
showSrcLoc (SrcLoc file line col) = take 1 file ++ f (drop1 file) ++ ":" ++ show line ++ ":" ++ show col
    where f (x:y:zs) | isPathSeparator x && isPathSeparator y = f $ x:zs
          f (x:xs) = x : f xs
          f [] = []

an :: SrcSpanInfo
an = def

---------------------------------------------------------------------
-- FIXITIES

getFixity :: Decl a -> [Fixity]
getFixity (InfixDecl sl a mp ops) = [Fixity (void a) (fromMaybe 9 mp) (UnQual () $ void $ f op) | op <- ops]
    where f (VarOp _ x) = x
          f (ConOp _ x) = x
getFixity _ = []

toInfixDecl :: Fixity -> Decl ()
toInfixDecl (Fixity a b c) = InfixDecl () a (Just b) $ maybeToList $ VarOp () <$> fromQual c



-- | This extension implies the following extensions
extensionImplies :: Extension -> [Extension]
extensionImplies = \x -> Map.findWithDefault [] x mp
    where mp = Map.fromList extensionImplications

-- | This extension is implied by the following extensions
extensionImpliedBy :: Extension -> [Extension]
extensionImpliedBy = \x -> Map.findWithDefault [] x mp
    where mp = Map.fromListWith (++) [(b, [a]) | (a,bs) <- extensionImplications, b <- bs]

-- | (a, bs) means extension a implies all of bs. Uses GHC source at
-- DynFlags.impliedXFlags
extensionImplications :: [(Extension, [Extension])]
extensionImplications = map toHse GhclibParserEx.extensionImplications
  where
    enable ext = parseExtension (show ext)
    disable ext = parseExtension ("No" ++ show ext)
    toHse (e, (enables, disables)) = (enable e, map enable enables ++ map disable disables)

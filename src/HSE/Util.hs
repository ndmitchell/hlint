
module HSE.Util(
    fromParen, fromPParen, an, getFixity, moduleDecls, isAssocNone,
    isAssocLeft, showSrcLoc,
    toInfixDecl, extensionImpliedBy,
    extensionImplies,
    ) where

import Control.Monad
import Data.List.Extra
import qualified Data.Map as Map
import Data.Maybe
import System.FilePath
import HSE.Type
import Data.Functor
import Prelude
import qualified Language.Haskell.GhclibParserEx.DynFlags as GhclibParserEx

---------------------------------------------------------------------
-- ACCESSOR/TESTER

moduleDecls :: Module_ -> [Decl_]
moduleDecls (Module _ _ _ _ xs) = xs
moduleDecls _ = [] -- XmlPage/XmlHybrid

fromParen :: Exp_ -> Exp_
fromParen (Paren _ x) = fromParen x
fromParen x = x

fromPParen :: Pat s -> Pat s
fromPParen (PParen _ x) = fromPParen x
fromPParen x = x


fromQual :: QName a -> Maybe (Name a)
fromQual (Qual _ _ x) = Just x
fromQual (UnQual _ x) = Just x
fromQual _ = Nothing

isAssocLeft AssocLeft{} = True; isAssocLeft _ = False
isAssocNone AssocNone{} = True; isAssocNone _ = False


---------------------------------------------------------------------
-- SRCLOC FUNCTIONS

showSrcLoc :: SrcLoc -> String
showSrcLoc (SrcLoc file line col) = take 1 file ++ f (drop1 file) ++ ":" ++ show line ++ ":" ++ show col
    where f (x:y:zs) | isPathSeparator x && isPathSeparator y = f $ x:zs
          f (x:xs) = x : f xs
          f [] = []

an :: SrcSpanInfo
an = noSrcSpan

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

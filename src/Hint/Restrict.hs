{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Hint.Restrict(restrictHint) where

{-
-- These tests rely on the .hlint.yaml file in the root
<TEST>
foo = unsafePerformIO --
foo = bar `unsafePerformIO` baz --
module Util where otherFunc = unsafePerformIO $ print 1 --
module Util where exitMessageImpure = unsafePerformIO $ print 1
foo = unsafePerformOI
</TEST>
-}

import Hint.Type(ModuHint,ModuleEx(..),Idea(..),Severity(..),warn',rawIdea')
import Config.Type

import Data.Generics.Uniplate.Operations
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Prelude

import HsSyn
import RdrName
import ApiAnnotation
import Module
import SrcLoc
import OccName
import GHC.Util

-- FIXME: The settings should be partially applied, but that's hard to orchestrate right now
restrictHint :: [Setting] -> ModuHint
restrictHint settings scope m =
    let anns = ghcAnnotations m
        ps   = pragmas anns
        opts = flags ps
        exts = langExts ps in
    checkPragmas modu opts exts restrict ++
    maybe [] (checkImports modu $ hsmodImports (unLoc (ghcModule m))) (Map.lookup RestrictModule restrict) ++
    maybe [] (checkFunctions modu $ hsmodDecls (unLoc (ghcModule m))) (Map.lookup RestrictFunction restrict)
    where
        modu = modName (ghcModule m)
        restrict = restrictions settings

---------------------------------------------------------------------
-- UTILITIES

data RestrictItem = RestrictItem
    {riAs :: [String]
    ,riWithin :: [(String, String)]
    ,riBadIdents :: [String]
    ,riMessage :: Maybe String
    }
instance Semigroup RestrictItem where
    RestrictItem x1 x2 x3 x4 <> RestrictItem y1 y2 y3 y4 = RestrictItem (x1<>y1) (x2<>y2) (x3<>y3) (x4<>y4)
instance Monoid RestrictItem where
    mempty = RestrictItem [] [] [] Nothing
    mappend = (<>)

restrictions :: [Setting] -> Map.Map RestrictType (Bool, Map.Map String RestrictItem)
restrictions settings = Map.map f $ Map.fromListWith (++) [(restrictType x, [x]) | SettingRestrict x <- settings]
    where
        f rs = (all restrictDefault rs
               ,Map.fromListWith (<>) [(s, RestrictItem restrictAs restrictWithin restrictBadIdents restrictMessage) | Restrict{..} <- rs, s <- restrictName])


ideaMessage :: Maybe String -> Idea -> Idea
ideaMessage (Just message) w = w{ideaNote=[Note message]}
ideaMessage Nothing w = w{ideaNote=[noteMayBreak]}

ideaNoTo :: Idea -> Idea
ideaNoTo w = w{ideaTo=Nothing}

noteMayBreak :: Note
noteMayBreak = Note "may break the code"

within :: String -> String -> RestrictItem -> Bool
within modu func RestrictItem{..} = any (\(a,b) -> (a == modu || a == "") && (b == func || b == "")) riWithin

---------------------------------------------------------------------
-- CHECKS

checkPragmas :: String
              -> [(Located AnnotationComment, [String])]
              -> [(Located AnnotationComment, [String])]
              ->  Map.Map RestrictType (Bool, Map.Map String RestrictItem)
              -> [Idea]
checkPragmas modu flags exts mps =
  f RestrictFlag "flags" flags ++ f RestrictExtension "extensions" exts
  where
   f tag name xs =
     [(if null good then ideaNoTo else id) $ notes $ rawIdea' Hint.Type.Warning ("Avoid restricted " ++ name) l c Nothing [] []
     | Just (def, mp) <- [Map.lookup tag mps]
     , (L l (AnnBlockComment c), les) <- xs
     , let (good, bad) = partition (isGood def mp) les
     , let note = maybe noteMayBreak Note . (=<<) riMessage . flip Map.lookup mp
     , let notes w = w {ideaNote=note <$> bad}
     , not $ null bad]
   isGood def mp x = maybe def (within modu "") $ Map.lookup x mp

checkImports :: String -> [LImportDecl GhcPs] -> (Bool, Map.Map String RestrictItem) -> [Idea]
checkImports modu imp (def, mp) =
    [ ideaMessage riMessage
      $ if | not allowImport -> ideaNoTo $ warn' "Avoid restricted module" i i []
           | not allowIdent  -> ideaNoTo $ warn' "Avoid restricted identifiers" i i []
           | not allowQual   -> warn' "Avoid restricted qualification" i (noLoc $ (unLoc i){ ideclAs=noLoc . mkModuleName <$> listToMaybe riAs} :: Located (ImportDecl GhcPs)) []
           | otherwise       -> error "checkImports: unexpected case"
    | i@(L _ ImportDecl {..}) <- imp
    , let ri@RestrictItem{..} = Map.findWithDefault (RestrictItem [] [("","") | def] [] Nothing) (moduleNameString (unLoc ideclName)) mp
    , let allowImport = within modu "" ri
    , let allowIdent = Set.disjoint
                       (Set.fromList riBadIdents)
                       (Set.fromList (maybe [] (\(b, lxs) -> if b then [] else concatMap (importListToIdents . unLoc) (unLoc lxs)) ideclHiding))
    , let allowQual = maybe True (\x -> null riAs || moduleNameString (unLoc x) `elem` riAs) ideclAs
    , not allowImport || not allowQual || not allowIdent
    ]

importListToIdents :: IE GhcPs -> [String]
importListToIdents =
  catMaybes .
  \case (IEVar _ n)              -> [fromName n]
        (IEThingAbs _ n)         -> [fromName n]
        (IEThingAll _ n)         -> [fromName n]
        (IEThingWith _ n _ ns _) -> fromName n : map fromName ns
        _                        -> []
  where
    fromName :: LIEWrappedName (IdP GhcPs) -> Maybe String
    fromName wrapped = case unLoc wrapped of
                         IEName    n -> fromId (unLoc n)
                         IEPattern n -> ("pattern " ++) <$> fromId (unLoc n)
                         IEType    n -> ("type " ++) <$> fromId (unLoc n)

    fromId :: IdP GhcPs -> Maybe String
    fromId (Unqual n) = Just $ occNameString n
    fromId (Qual _ n) = Just $ occNameString n
    fromId (Orig _ n) = Just $ occNameString n
    fromId (Exact _)  = Nothing

checkFunctions :: String -> [LHsDecl GhcPs] -> (Bool, Map.Map String RestrictItem) -> [Idea]
checkFunctions modu decls (def, mp) =
    [ (ideaMessage riMessage $ ideaNoTo $ warn' "Avoid restricted function" x x []){ideaDecl = [dname]}
    | d <- decls
    , let dname = fromMaybe "" (declName d)
    , x <- universeBi d :: [Located RdrName]
    , let ri@RestrictItem{..} = Map.findWithDefault (RestrictItem [] [("","") | def] [] Nothing) (occNameString (rdrNameOcc (unLoc x))) mp
    , not $ within modu dname ri
    ]

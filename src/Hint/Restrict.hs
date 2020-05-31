{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Hint.Restrict(restrictHint) where

{-
-- These tests rely on the .hlint.yaml file in the root
<TEST>
foo = unsafePerformIO --
foo = bar `unsafePerformIO` baz --
module Util where otherFunc = unsafePerformIO $ print 1 --
module Util where exitMessageImpure = System.IO.Unsafe.unsafePerformIO $ print 1
foo = unsafePerformOI
import Data.List.NonEmpty as NE \
foo = NE.nub (NE.fromList [1, 2, 3]) --
import Hypothetical.Module \
foo = nub s
</TEST>
-}

import Hint.Type(ModuHint,ModuleEx(..),Idea(..),Severity(..),warn,rawIdea)
import Config.Type

import Data.Generics.Uniplate.Operations
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Extra
import Data.Maybe
import Data.Semigroup
import Data.Tuple.Extra
import Control.Applicative
import Control.Monad
import Prelude

import GHC.Hs
import RdrName
import ApiAnnotation
import Module
import SrcLoc
import OccName
import Language.Haskell.GhclibParserEx.GHC.Hs
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
import GHC.Util

-- FIXME: The settings should be partially applied, but that's hard to orchestrate right now
restrictHint :: [Setting] -> ModuHint
restrictHint settings scope m =
    let anns = ghcAnnotations m
        ps   = pragmas anns
        opts = flags ps
        exts = languagePragmas ps in
    checkPragmas modu opts exts rOthers ++
    maybe [] (checkImports modu $ hsmodImports (unLoc (ghcModule m))) (Map.lookup RestrictModule rOthers) ++
    checkFunctions scope modu (hsmodDecls (unLoc (ghcModule m))) rFunction
    where
        modu = modName (ghcModule m)
        (rFunction, rOthers) = restrictions settings

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

-- Contains a map from module (Nothing if the rule is unqualified) to (within, message), so that we can
-- distinguish functions with the same name.
-- For example, this allows us to have separate rules for "Data.Map.fromList" and "Data.Set.fromList".
-- Using newtype rather than type because we want to define (<>) as 'Map.unionWith (<>)'.
newtype RestrictFunction = RestrictFun (Map.Map (Maybe String) ([(String, String)], Maybe String))

instance Semigroup RestrictFunction where
    RestrictFun m1 <> RestrictFun m2 = RestrictFun (Map.unionWith (<>) m1 m2)

type RestrictFunctions = (Bool, Map.Map String RestrictFunction)
type OtherRestrictItems = Map.Map RestrictType (Bool, Map.Map String RestrictItem)

restrictions :: [Setting] -> (RestrictFunctions, OtherRestrictItems)
restrictions settings = (rFunction, rOthers)
    where
        (map snd -> rfs, ros) = partition ((== RestrictFunction) . fst) [(restrictType x, x) | SettingRestrict x <- settings]
        rFunction = (all restrictDefault rfs, Map.fromListWith (<>) [mkRf s r | r <- rfs, s <- restrictName r])
        mkRf s Restrict{..} = (name, RestrictFun $ Map.singleton modu (restrictWithin, restrictMessage))
          where
            -- Parse module and name from s. module = Nothing if the rule is unqualified.
            (modu, name) = first (fmap NonEmpty.init . NonEmpty.nonEmpty) (breakEnd (== '.') s)

        rOthers = Map.map f $ Map.fromListWith (++) (map (second pure) ros)
        f rs = (all restrictDefault rs
               ,Map.fromListWith (<>) [(s, RestrictItem restrictAs restrictWithin restrictBadIdents restrictMessage) | Restrict{..} <- rs, s <- restrictName])

ideaMessage :: Maybe String -> Idea -> Idea
ideaMessage (Just message) w = w{ideaNote=[Note message]}
ideaMessage Nothing w = w{ideaNote=[noteMayBreak]}

ideaNoTo :: Idea -> Idea
ideaNoTo w = w{ideaTo=Nothing}

noteMayBreak :: Note
noteMayBreak = Note "may break the code"

within :: String -> String -> [(String, String)] -> Bool
within modu func = any (\(a,b) -> (a == modu || a == "") && (b == func || b == ""))

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
     [(if null good then ideaNoTo else id) $ notes $ rawIdea Hint.Type.Warning ("Avoid restricted " ++ name) l c Nothing [] []
     | Just (def, mp) <- [Map.lookup tag mps]
     , (L l (AnnBlockComment c), les) <- xs
     , let (good, bad) = partition (isGood def mp) les
     , let note = maybe noteMayBreak Note . (=<<) riMessage . flip Map.lookup mp
     , let notes w = w {ideaNote=note <$> bad}
     , not $ null bad]
   isGood def mp x = maybe def (within modu "" . riWithin) $ Map.lookup x mp

checkImports :: String -> [LImportDecl GhcPs] -> (Bool, Map.Map String RestrictItem) -> [Idea]
checkImports modu imp (def, mp) =
    [ ideaMessage riMessage
      $ if | not allowImport -> ideaNoTo $ warn "Avoid restricted module" i i []
           | not allowIdent  -> ideaNoTo $ warn "Avoid restricted identifiers" i i []
           | not allowQual   -> warn "Avoid restricted qualification" i (noLoc $ (unLoc i){ ideclAs=noLoc . mkModuleName <$> listToMaybe riAs} :: Located (ImportDecl GhcPs)) []
           | otherwise       -> error "checkImports: unexpected case"
    | i@(L _ ImportDecl {..}) <- imp
    , let RestrictItem{..} = Map.findWithDefault (RestrictItem [] [("","") | def] [] Nothing) (moduleNameString (unLoc ideclName)) mp
    , let allowImport = within modu "" riWithin
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

checkFunctions :: Scope -> String -> [LHsDecl GhcPs] -> RestrictFunctions -> [Idea]
checkFunctions scope modu decls (def, mp) =
    [ (ideaMessage message $ ideaNoTo $ warn "Avoid restricted function" x x []){ideaDecl = [dname]}
    | d <- decls
    , let dname = fromMaybe "" (declName d)
    , x <- universeBi d :: [Located RdrName]
    , let xMods = possModules scope x
    , let (withins, message) = fromMaybe ([("","") | def], Nothing) (findFunction x xMods)
    , not $ within modu dname withins
    ]
  where
    -- Returns Just iff there are rules for x, which are either unqualified, or qualified with a module that is
    -- one of x's possible modules.
    -- If there are multiple matching rules (e.g., there's both an unqualified version and a qualified version), their
    -- withins and messages are concatenated with (<>).
    findFunction :: Located RdrName -> [ModuleName] -> Maybe ([(String, String)], Maybe String)
    findFunction (rdrNameStr -> x) (map moduleNameString -> possMods)
      | Just (RestrictFun mp) <- Map.lookup x mp =
          fmap sconcat . NonEmpty.nonEmpty . Map.elems $ Map.filterWithKey (const . maybe True (`elem` possMods)) mp
      | otherwise = Nothing

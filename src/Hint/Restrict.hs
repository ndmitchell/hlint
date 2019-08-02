{-# LANGUAGE RecordWildCards #-}

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

import qualified Data.Map as Map
import Config.Type
import Hint.Type
import Data.List
import Data.Maybe
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Prelude


-- FIXME: The settings should be partially applied, but that's hard to orchestrate right now
restrictHint :: [Setting] -> ModuHint
restrictHint settings scope m =
        checkPragmas modu (modulePragmas (hseModule m)) restrict ++
        maybe [] (checkImports modu $ moduleImports (hseModule m)) (Map.lookup RestrictModule restrict) ++
        maybe [] (checkFunctions modu $ moduleDecls (hseModule m)) (Map.lookup RestrictFunction restrict)
    where
        modu = moduleName (hseModule m)
        restrict = restrictions settings

---------------------------------------------------------------------
-- UTILITIES

data RestrictItem = RestrictItem
    {riAs :: [String]
    ,riWithin :: [(String, String)]
    ,riMessage :: Maybe String
    }
instance Semigroup RestrictItem where
    RestrictItem x1 x2 x3 <> RestrictItem y1 y2 y3 = RestrictItem (x1<>y1) (x2<>y2) (x3<>y3)
instance Monoid RestrictItem where
    mempty = RestrictItem [] [] Nothing
    mappend = (<>)

restrictions :: [Setting] -> Map.Map RestrictType (Bool, Map.Map String RestrictItem)
restrictions settings = Map.map f $ Map.fromListWith (++) [(restrictType x, [x]) | SettingRestrict x <- settings]
    where
        f rs = (all restrictDefault rs
               ,Map.fromListWith (<>) [(s, RestrictItem restrictAs restrictWithin restrictMessage) | Restrict{..} <- rs, s <- restrictName])


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

checkPragmas :: String -> [ModulePragma S] -> Map.Map RestrictType (Bool, Map.Map String RestrictItem) -> [Idea]
checkPragmas modu xs mps = f RestrictFlag "flags" onFlags ++ f RestrictExtension "extensions" onExtensions
    where
        f tag name sel =
            [ (if null good then ideaNoTo else id) $ notes $ warn ("Avoid restricted " ++ name) o (regen good) []
            | Just (def, mp) <- [Map.lookup tag mps]
            , o <- xs, Just (xs, regen) <- [sel o]
            , let (good, bad) = partition (isGood def mp) xs
            , let note = maybe noteMayBreak Note . (=<<) riMessage . flip Map.lookup mp
            , let notes w = w{ideaNote=note <$> bad}
            , not $ null bad]

        onFlags (OptionsPragma s t x) = Just (words x, OptionsPragma s t . unwords)
        onFlags _ = Nothing

        onExtensions (LanguagePragma s xs) = Just (map fromNamed xs, LanguagePragma (s :: S) . map toNamed)
        onExtensions _ = Nothing

        isGood def mp x = maybe def (within modu "") $ Map.lookup x mp


checkImports :: String -> [ImportDecl S] -> (Bool, Map.Map String RestrictItem) -> [Idea]
checkImports modu imp (def, mp) =
    [ ideaMessage riMessage $ if not allowImport
      then ideaNoTo $ warn "Avoid restricted module" i i []
      else warn "Avoid restricted qualification" i i{importAs=ModuleName an <$> listToMaybe riAs} []
    | i@ImportDecl{..} <- imp
    , let ri@RestrictItem{..} = Map.findWithDefault (RestrictItem [] [("","") | def] Nothing) (fromModuleName importModule) mp
    , let allowImport = within modu "" ri
    , let allowQual = maybe True (\x -> null riAs || fromModuleName x `elem` riAs) importAs
    , not allowImport || not allowQual
    ]


checkFunctions :: String -> [Decl_] -> (Bool, Map.Map String RestrictItem) -> [Idea]
checkFunctions modu decls (def, mp) =
    [ (ideaMessage riMessage $ ideaNoTo $ warn "Avoid restricted function" x x []){ideaDecl = [dname]}
    | d <- decls
    , let dname = fromNamed d
    , x <- universeBi d :: [QName S]
    , let ri@RestrictItem{..} = Map.findWithDefault (RestrictItem [] [("","") | def] Nothing) (fromNamed x) mp
    , not $ within modu dname ri
    ]

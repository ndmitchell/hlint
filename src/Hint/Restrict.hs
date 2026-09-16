{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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

import Hint.Type(ModuHint,ModuleEx(..),Idea(..),Severity(..),warn,rawIdea,modComments,firstDeclComments)
import Config.Type
import Util

import Data.Generics.Uniplate.DataOnly
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.List.Extra
import Data.List.NonEmpty (nonEmpty)
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Tuple.Extra
import Control.Applicative
import Control.Monad
import Control.Monad.Extra
import Prelude

import GHC.Hs
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.Name.Occurrence
import Language.Haskell.GhclibParserEx.GHC.Hs
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
import GHC.Util

-- FIXME: The settings should be partially applied, but that's hard to orchestrate right now
restrictHint :: [Setting] -> ModuHint
restrictHint settings scope m =
    -- Comments appearing without an empty line before the first
    -- declaration in a module are now associated with the declaration
    -- not the module so to be safe, look also at `firstDeclComments
    -- modu`
    -- (https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9517).
    let annsMod = modComments m
        annsFirstDecl = firstDeclComments m
        ps   = pragmas annsMod ++ pragmas annsFirstDecl
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
    ,riAsRequired :: Alt Maybe Bool
    ,riImportStyle :: Alt Maybe RestrictImportStyle
    ,riQualifiedStyle :: Alt Maybe QualifiedStyle
    ,riWithin :: [(String, String)]
    ,riRestrictIdents :: RestrictIdents
    ,riMessage :: Maybe String
    }

instance Semigroup RestrictItem where
    RestrictItem x1 x2 x3 x4 x5 x6 x7
      <> RestrictItem y1 y2 y3 y4 y5 y6 y7
      = RestrictItem (x1<>y1) (x2<>y2) (x3<>y3) (x4<>y4) (x5<>y5) (x6<>y6) (x7<>y7)

-- Contains a map from module (Nothing if the rule is unqualified) to (within, message), so that we can
-- distinguish functions with the same name.
-- For example, this allows us to have separate rules for "Data.Map.fromList" and "Data.Set.fromList".
-- Using newtype rather than type because we want to define (<>) as 'Map.unionWith (<>)'.
-- The 'Int' is the position of the rule in the settings, so that when several
-- rules restrict the type applications of one name, the last-declared one wins
-- rather than some arbitrary function of the two. Withins and messages still
-- merge, because a name can sensibly be allowed in the union of two places but
-- cannot sensibly both require and forbid a type application.
newtype RestrictFunction = RestrictFun (Map.Map (Maybe String) ([(String, String)], Maybe String, Maybe (Max (Arg Int RestrictTypeApp))))

instance Semigroup RestrictFunction where
    RestrictFun m1 <> RestrictFun m2 = RestrictFun (Map.unionWith (<>) m1 m2)

type RestrictFunctions = (Bool, Map.Map String RestrictFunction)
type OtherRestrictItems = Map.Map RestrictType (Bool, Map.Map String RestrictItem)

restrictions :: [Setting] -> (RestrictFunctions, OtherRestrictItems)
restrictions settings = (rFunction, rOthers)
    where
        (map snd -> rfs, ros) = partition ((== RestrictFunction) . fst) [(restrictType x, x) | SettingRestrict x <- settings]
        rFunction = (all restrictDefault rfs, Map.fromListWith (<>) [mkRf i s r | (i, r) <- zipFrom 0 rfs, s <- restrictName r])
        mkRf i s Restrict{..} = (name, RestrictFun $ Map.singleton modu (restrictWithin, restrictMessage, Max . Arg i <$> restrictTypeApp))
          where
            -- Parse module and name from s. module = Nothing if the rule is unqualified.
            (modu, name) = first (fmap NonEmpty.init . NonEmpty.nonEmpty) (breakEnd (== '.') s)

        rOthers = Map.map f $ Map.fromListWith (++) (map (second pure) ros)
        f rs = (all restrictDefault rs
               ,Map.fromListWith (<>)
                  [(,) s RestrictItem
                    { riAs             = restrictAs
                    , riAsRequired     = restrictAsRequired
                    , riImportStyle    = restrictImportStyle
                    , riQualifiedStyle = restrictQualifiedStyle
                    , riWithin         = restrictWithin
                    , riRestrictIdents = restrictIdents
                    , riMessage        = restrictMessage
                    }
                  | Restrict{..} <- rs, s <- restrictName])

ideaMessage :: Maybe String -> Idea -> Idea
ideaMessage (Just message) w = w{ideaNote=[Note message]}
ideaMessage Nothing w = w{ideaNote=[noteMayBreak]}

ideaNoTo :: Idea -> Idea
ideaNoTo w = w{ideaTo=Nothing}

noteMayBreak :: Note
noteMayBreak = Note "may break the code"

within :: String -> String -> [(String, String)] -> Bool
within modu func = any (\(a,b) -> (a ~= modu || a == "") && (b ~= func || b == ""))
  where (~=) = wildcardMatch

---------------------------------------------------------------------
-- CHECKS

checkPragmas :: String
              -> [(LEpaComment, [String])]
              -> [(LEpaComment, [String])]
              ->  Map.Map RestrictType (Bool, Map.Map String RestrictItem)
              -> [Idea]
checkPragmas modu flags exts mps =
  f RestrictFlag "flags" flags ++ f RestrictExtension "extensions" exts
  where
   f tag name xs =
     [(if null good then ideaNoTo else id) $ notes $ rawIdea Hint.Type.Warning ("Avoid restricted " ++ name) (getAncLoc l) c Nothing [] []
     | Just (def, mp) <- [Map.lookup tag mps]
     , (l@(L _ (EpaComment (EpaBlockComment c) _)), les) <- xs
     , let (good, bad) = partition (isGood def mp) les
     , let note = maybe noteMayBreak Note . (=<<) riMessage . flip Map.lookup mp
     , let notes w = w {ideaNote=note <$> bad}
     , not $ null bad]
   isGood def mp x = maybe def (within modu "" . riWithin) $ Map.lookup x mp


-- | Extension to GHC's 'ImportDeclQualifiedStyle', expressing @qualifiedStyle: unrestricted@,
-- i.e. the preference of "either pre- or post-, but qualified" in a rule.
data QualifiedPostOrPre = QualifiedPostOrPre deriving Eq

checkImports :: String -> [LImportDecl GhcPs] -> (Bool, Map.Map String RestrictItem) -> [Idea]
checkImports modu lImportDecls (def, mp) = mapMaybe getImportHint lImportDecls
  where
    getImportHint :: LImportDecl GhcPs -> Maybe Idea
    getImportHint i@(L _ ImportDecl{..}) = do
      let RestrictItem{..} = getRestrictItem def ideclName mp
      either (Just . ideaMessage riMessage) (const Nothing) $ do
        unless (within modu "" riWithin) $
          Left $ ideaNoTo $ warn "Avoid restricted module" (reLoc i) (reLoc i) []

        let importedIdents = Set.fromList $
              case first (== EverythingBut) <$> ideclImportList of
                Just (False, lxs) -> concatMap (importListToIdents . unLoc) (unLoc lxs)
                _ -> []
            invalidIdents = case riRestrictIdents of
              NoRestrictIdents -> Set.empty
              ForbidIdents badIdents -> importedIdents `Set.intersection` Set.fromList badIdents
              OnlyIdents onlyIdents -> importedIdents `Set.difference` Set.fromList onlyIdents
        unless (Set.null invalidIdents) $
          Left $ ideaNoTo $ warn "Avoid restricted identifiers" (reLoc i) (reLoc i) []

        let qualAllowed = case (riAs, ideclAs) of
              ([], _) -> True
              (_, Nothing) -> maybe True not $ getAlt riAsRequired
              (_, Just (L _ modName)) -> moduleNameString modName `elem` riAs
        unless qualAllowed $ do
          let i' = noLoc $ (unLoc i){ ideclAs = noLocA . mkModuleName <$> listToMaybe riAs }
          Left $ warn "Avoid restricted alias" (reLoc i) i' []

        let (expectedQual, expectedHiding) =
              case fromMaybe ImportStyleUnrestricted $ getAlt riImportStyle of
                ImportStyleUnrestricted
                  | NotQualified <- ideclQualified -> (Nothing, Nothing)
                  | otherwise -> (Just $ second (<> " or unqualified") expectedQualStyle, Nothing)
                ImportStyleQualified -> (Just expectedQualStyle, Nothing)
                ImportStyleExplicitOrQualified
                  | Just (False, _) <- first (== EverythingBut) <$> ideclImportList -> (Nothing, Nothing)
                  | otherwise ->
                      ( Just $ second (<> " or with an explicit import list") expectedQualStyle
                      , Nothing )
                ImportStyleExplicit
                  | Just (False, _) <- first (== EverythingBut) <$> ideclImportList -> (Nothing, Nothing)
                  | otherwise ->
                      ( Just (Right NotQualified, "unqualified")
                      , Just $ Just (Exactly, noLocA []) )
                ImportStyleUnqualified -> (Just (Right NotQualified, "unqualified"), Nothing)
            expectedQualStyle =
              case fromMaybe QualifiedStyleUnrestricted $ getAlt riQualifiedStyle of
                QualifiedStyleUnrestricted -> (Left QualifiedPostOrPre, "qualified")
                QualifiedStylePost -> (Right QualifiedPost, "post-qualified")
                QualifiedStylePre -> (Right QualifiedPre, "pre-qualified")
            -- unless expectedQual is Nothing, it holds the Idea (hint) to ultimately emit,
            -- except in these cases when the rule's requirements are fulfilled in-source:
            qualIdea
              -- the rule demands a particular importStyle, and the decl obeys exactly
              | Just (Right ideclQualified) == (fst <$> expectedQual) = Nothing
              -- the rule demands a QualifiedPostOrPre import, and the decl does either
              | Just (Left QualifiedPostOrPre) == (fst <$> expectedQual)
                && ideclQualified `elem` [QualifiedPost, QualifiedPre] = Nothing
              -- otherwise, expectedQual gets converted into a warning below (or is Nothing)
              | otherwise = expectedQual
        whenJust qualIdea $ \(qual, hint) -> do
          -- convert non-Nothing qualIdea into hlint's refactoring Idea
          let i' = noLoc $ (unLoc i){ ideclQualified = fromRight QualifiedPre qual
                                    , ideclImportList = fromMaybe ideclImportList expectedHiding }
              msg = moduleNameString (unLoc ideclName) <> " should be imported " <> hint
          Left $ warn msg (reLoc i) i' []

getRestrictItem :: Bool -> LocatedA ModuleName -> Map.Map String RestrictItem -> RestrictItem
getRestrictItem def ideclName =
  fromMaybe (RestrictItem mempty mempty mempty mempty [("","") | def] NoRestrictIdents Nothing)
    . lookupRestrictItem ideclName

lookupRestrictItem :: LocatedA ModuleName -> Map.Map String RestrictItem -> Maybe RestrictItem
lookupRestrictItem ideclName mp =
    let moduleName = moduleNameString $ unLoc ideclName
        exact = Map.lookup moduleName mp
        wildcard = nonEmpty
            . fmap snd
            . reverse -- the hope is less specific matches will end up last, but it's not guaranteed
            . filter (liftA2 (&&) (elem '*') (`wildcardMatch` moduleName) . fst)
            $ Map.toList mp
    in exact <> sconcat (sequence wildcard)

importListToIdents :: IE GhcPs -> [String]
importListToIdents =
  catMaybes .
  \case (IEVar _ n _)              -> [fromName n]
        (IEThingAbs _ n _)         -> [fromName n]
        (IEThingAll _ n _)         -> [fromName n]
        (IEThingWith _ n _ ns _)   -> fromName n : map fromName ns
        _                        -> []
  where
    fromName :: LIEWrappedName GhcPs -> Maybe String
    fromName wrapped =
      case unLoc wrapped of
        IEName    _ n -> fromId (unLoc n)
        IEPattern _ n -> ("pattern " ++) <$> fromId (unLoc n)
        IEType    _ n -> ("type " ++) <$> fromId (unLoc n)

    fromId :: IdP GhcPs -> Maybe String
    fromId (Unqual n) = Just $ occNameString n
    fromId (Qual _ n) = Just $ occNameString n
    fromId (Orig _ n) = Just $ occNameString n
    fromId (Exact _)  = Nothing

checkFunctions :: Scope -> String -> [LHsDecl GhcPs] -> RestrictFunctions -> [Idea]
checkFunctions scope modu decls (def, mp) =
    [ (ideaMessage message $ ideaNoTo $ warn hint (reLoc x) (reLoc x) []){ideaDecl = [dname]}
    | d <- decls
    , let dname = fromMaybe "" (declName d)
    , x <- universeBi d :: [LocatedN RdrName]
    , let xMods = possModules scope x
    , let (withins, message, typeApp) = fromMaybe ([("","") | def], Nothing, Nothing) (findFunction mp x xMods)
    , hint <- maybeToList $ restrictFunctionHint modu dname withins typeApp typeAppCounts typeAppSites x
    ]
  where
    typeAppCounts = typeApplicationCounts decls
    typeAppSites = typeApplicationSites decls

-- | How many visible type applications a name carries. A wildcard argument is
-- still a visible type application, but it fixes nothing, so the two numbers
-- differ for @fromIntegral \@_ \@_@ and the two restrictions ask about
-- different ones.
data TypeAppCount = TypeAppCount
    {typeAppWritten :: !Int -- ^ every @\@T@, wildcards included
    ,typeAppFixed :: !Int -- ^ only those that actually fix a type
    }

noTypeApps :: TypeAppCount
noTypeApps = TypeAppCount {typeAppWritten = 0, typeAppFixed = 0}

addTypeAppCounts :: TypeAppCount -> TypeAppCount -> TypeAppCount
addTypeAppCounts c1 c2 = TypeAppCount
    {typeAppWritten = typeAppWritten c1 + typeAppWritten c2
    ,typeAppFixed = typeAppFixed c1 + typeAppFixed c2
    }

-- | The hint to emit for a use of a (possibly) restricted function, or
-- 'Nothing' if the use is allowed. A 'within' violation takes precedence over a
-- visible type application violation.
restrictFunctionHint
    :: String -> String -> [(String, String)] -> Maybe RestrictTypeApp
    -> Map.Map SrcSpanD TypeAppCount -> Set.Set SrcSpanD -> LocatedN RdrName -> Maybe String
restrictFunctionHint modu dname withins typeApp typeAppCounts typeAppSites x
    | not $ within modu dname withins = Just "Avoid restricted function"
    | not $ sp `Set.member` typeAppSites = Nothing
    | otherwise = case typeApp of
        Just (TypeAppRequired n) | typeAppFixed count < n -> Just "Use visible type application"
        Just TypeAppForbidden | typeAppWritten count > 0 -> Just "Avoid visible type application"
        _ -> Nothing
  where
    sp = SrcSpanD (locA (getLoc x))
    count = Map.findWithDefault noTypeApps sp typeAppCounts

-- | Source spans of the names that can carry a visible type application: the
-- head of an expression, and a constructor in a prefix pattern. Every other
-- occurrence of a name -- a type signature, a class method signature, a record
-- field, a binder -- is somewhere no type application can be written, so
-- demanding one there would be advice that cannot be followed.
--
-- An operator in infix position or in a section is excluded for the same
-- reason: @a \`seq\` b@ can only carry one after being restructured into
-- @seq \@T a b@, and a hint asking for that is asking for the wrong thing.
-- Parenthesised, as in @(\<+\>)@, the operator is back in prefix position and
-- does count.
--
-- Note that this does not resolve local binders, so a locally bound name that
-- shadows a restricted one is still treated as a use of it.
typeApplicationSites :: [LHsDecl GhcPs] -> Set.Set SrcSpanD
typeApplicationSites decls = Set.difference sites infixOperators
  where
    sites :: Set.Set SrcSpanD
    sites = Set.fromList $
        [ SrcSpanD (locA (getLoc name))
        | L _ (HsVar _ name) <- universeBi decls :: [LHsExpr GhcPs]
        ] ++
        [ SrcSpanD (locA (getLoc name))
        | L _ (ConPat _ name PrefixCon{}) <- universeBi decls :: [LPat GhcPs]
        ]

    infixOperators :: Set.Set SrcSpanD
    infixOperators = Set.fromList
        [ SrcSpanD (locA (getLoc name))
        | L _ (HsVar _ name) <- concatMap operator (universeBi decls :: [LHsExpr GhcPs])
        ]

    operator :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    operator = \case
        L _ (OpApp _ _ op _) -> [op]
        L _ (SectionL _ _ op) -> [op]
        L _ (SectionR _ op _) -> [op]
        _ -> []

-- | A map from the source span of a name to the visible type applications
-- attached to it. Each @\@T@ is a separate 'HsAppType' node (or an element of a
-- constructor pattern's type-argument list), and every node in an application
-- chain shares the head name's source span, so summing gives the count.
typeApplicationCounts :: [LHsDecl GhcPs] -> Map.Map SrcSpanD TypeAppCount
typeApplicationCounts decls = Map.fromListWith addTypeAppCounts $
    [ (SrcSpanD (locA (getLoc h)), countTypeApps [ty])
    | L _ (HsAppType _ fun (HsWC _ ty)) <- universeBi decls :: [LHsExpr GhcPs]
    , Just h <- [typeAppHead fun]
    ] ++
    [ (SrcSpanD (locA (getLoc name)), countTypeApps [t | HsConPatTyArg _ (HsTP _ t) <- tyArgs])
    | L _ (ConPat _ name (PrefixCon tyArgs _)) <- universeBi decls :: [LPat GhcPs]
    , not $ null tyArgs
    ]

-- | A wildcard argument, as in @fromIntegral \@_ \@_@, leaves the type just as
-- inferred as writing no type argument at all, so it is written but fixes
-- nothing.
countTypeApps :: [LHsType GhcPs] -> TypeAppCount
countTypeApps tys = TypeAppCount
    {typeAppWritten = length tys
    ,typeAppFixed = length $ filter (not . isWildcardTy) tys
    }
  where
    isWildcardTy :: LHsType GhcPs -> Bool
    isWildcardTy = \case
        L _ HsWildCardTy{} -> True
        _ -> False

-- | The head name of an application chain, looking through value and type
-- applications and parentheses. Only walks the function spine, never into
-- arguments, so applications of distinct functions don't interfere.
typeAppHead :: LHsExpr GhcPs -> Maybe (LocatedN RdrName)
typeAppHead = \case
    L _ (HsVar _ name)      -> Just name
    L _ (HsApp _ fun _)     -> typeAppHead fun
    L _ (HsAppType _ fun _) -> typeAppHead fun
    L _ (HsPar _ fun)       -> typeAppHead fun
    _                       -> Nothing

-- Returns Just iff there are rules for x, which are either unqualified, or qualified with a module that is
-- one of x's possible modules.
-- If there are multiple matching rules (e.g., there's both an unqualified version and a qualified version), their
-- withins and messages are concatenated with (<>), and the last-declared type application restriction wins.
findFunction
    :: Map.Map String RestrictFunction
    -> LocatedN RdrName
    -> [ModuleName]
    -> Maybe ([(String, String)], Maybe String, Maybe RestrictTypeApp)
findFunction restrictMap (rdrNameStr -> x) (map moduleNameString -> possMods) = do
    (RestrictFun mp) <- Map.lookup x restrictMap
    n <- NonEmpty.nonEmpty . Map.elems $ Map.filterWithKey (const . maybe True (`elem` possMods)) mp
    let (withins, message, typeApp) = sconcat n
    pure (withins, message, (\(Max (Arg _ restrictTypeApp)) -> restrictTypeApp) <$> typeApp)

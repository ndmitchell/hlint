{-# LANGUAGE PatternGuards, ViewPatterns, ScopedTypeVariables, TupleSections #-}
module Config.Haskell(
    readPragma',
    readComment,
    readSetting,
    readFileConfigHaskell
    ) where

import HSE.All
import Data.Char
import Data.List.Extra
import Text.Read.Extra(readMaybe)
import Data.Tuple.Extra
import Data.Maybe
import Config.Type
import Util
import Prelude

import qualified HsSyn as GHC
import qualified BasicTypes as GHC
import GHC.Util
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances

import SrcLoc as GHC
import HsExtension
import HsDecls hiding (SpliceDecl)
import HsExpr hiding (Match)
import FastString
import HsLit
import ApiAnnotation
import OccName
import Outputable


addInfix :: ParseFlags -> ParseFlags
addInfix = parseFlagsAddFixities $ infix_ (-1) ["==>"]


---------------------------------------------------------------------
-- READ A SETTINGS FILE

readFileConfigHaskell :: FilePath -> Maybe String -> IO [Setting]
readFileConfigHaskell file contents = do
    let flags = addInfix defaultParseFlags
    res <- parseModuleEx flags file contents
    case res of
        Left (ParseError sl msg err) ->
            error $ "Config parse failure at " ++ showSrcLoc sl ++ ": " ++ msg ++ "\n" ++ err
        Right modEx@(ModuleEx m _ _ _) -> return $ readSettings m ++ map SettingClassify (concatMap readComment (ghcComments modEx))


-- | Given a module containing HLint settings information return the 'Classify' rules and the 'HintRule' expressions.
--   Any fixity declarations will be discarded, but any other unrecognised elements will result in an exception.
readSettings :: Module_ -> [Setting]
readSettings m = concatMap readSetting $ concatMap getEquations $
                       [AnnPragma l x | AnnModulePragma l x <- modulePragmas m] ++ moduleDecls m


readSetting :: Decl_ -> [Setting]
readSetting (FunBind _ [Match _ (Ident _ (getSeverity -> Just severity)) pats (UnGuardedRhs _ bod) bind])
    | InfixApp _ lhs op rhs <- bod, opExp op ~= "==>" =
        let (a,b) = readSide $ childrenBi bind in
        let unit = GHC.noLoc $ GHC.ExplicitTuple GHC.noExt [] GHC.Boxed in
        [SettingMatchExp $
         HintRule severity (headDef defaultHintName names) (fromParen lhs) (fromParen rhs) a b
        -- Todo : Replace these with "proper" GHC expressions.
         (extendInstances mempty) (extendInstances unit) (extendInstances unit) Nothing]
    | otherwise = [SettingClassify $ Classify severity n a b | n <- names2, (a,b) <- readFuncs bod]
    where
        names = filter (not . null) $ getNames pats bod
        names2 = ["" | null names] ++ names

readSetting x | "test" `isPrefixOf` lower (fromNamed x) = []
readSetting (AnnPragma _ x) | Just y <- readPragma x = [SettingClassify y]
readSetting (PatBind an (PVar _ name) bod bind) = readSetting $ FunBind an [Match an name [] bod bind]
readSetting (FunBind an xs) | length xs /= 1 = concatMap (readSetting . FunBind an . pure) xs
readSetting (SpliceDecl an (App _ (Var _ x) (Lit _ y))) = readSetting $ FunBind an [Match an (toNamed $ fromNamed x) [PLit an (Signless an) y] (UnGuardedRhs an $ Lit an $ String an "" "") Nothing]
readSetting x@InfixDecl{} = map Infix $ getFixity x
readSetting x = errorOn x "bad hint"


-- | Read an {-# ANN #-} pragma and determine if it is intended for HLint.
--   Return Nothing if it is not an HLint pragma, otherwise what it means.
readPragma :: Annotation S -> Maybe Classify
readPragma o = case o of
    Ann _ name x -> f (fromNamed name) x
    TypeAnn _ name x -> f (fromNamed name) x
    ModuleAnn _ x -> f "" x
    where
        f name (Lit _ (String _ s _)) | "hlint:" `isPrefixOf` lower s =
                case getSeverity a of
                    Nothing -> errorOn o "bad classify pragma"
                    Just severity -> Just $ Classify severity (trimStart b) "" name
            where (a,b) = break isSpace $ trimStart $ drop 6 s
        f name (Paren _ x) = f name x
        f name (ExpTypeSig _ x _) = f name x
        f _ _ = Nothing


-- | Read an {-# ANN #-} pragma and determine if it is intended for HLint.
--   Return Nothing if it is not an HLint pragma, otherwise what it means.
readPragma' :: AnnDecl GhcPs -> Maybe Classify
readPragma' (HsAnnotation _ _ provenance expr) = f expr
    where
        name = case provenance of
            ValueAnnProvenance (L _ x) -> occNameString $ occName x
            TypeAnnProvenance (L _ x) -> occNameString $ occName x
            ModuleAnnProvenance -> ""

        f (L _ (HsLit _ (HsString _ (unpackFS -> s)))) | "hlint:" `isPrefixOf` lower s =
                case getSeverity a of
                    Nothing -> errorOn' expr "bad classify pragma"
                    Just severity -> Just $ Classify severity (trimStart b) "" name
            where (a,b) = break isSpace $ trimStart $ drop 6 s
        f (L _ (HsPar _ x)) = f x
        f (L _ (ExprWithTySig _ x _)) = f x
        f _ = Nothing
readPragma' _ = Nothing

readComment :: GHC.Located AnnotationComment -> [Classify]
readComment c@(L pos AnnBlockComment{})
    | (hash, x) <- maybe (False, x) (True,) $ stripPrefix "#" x
    , x <- trim x
    , (hlint, x) <- word1 x
    , lower hlint == "hlint"
    = f hash x
    where
        x = commentText c
        f hash x
            | Just x <- if hash then stripSuffix "#" x else Just x
            , (sev, x) <- word1 x
            , Just sev <- getSeverity sev
            , (things, x) <- g x
            , Just hint <- if x == "" then Just "" else readMaybe x
            = map (Classify sev hint "") $ ["" | null things] ++ things
        f hash _ = errorOnComment c $ "bad HLINT pragma, expected:\n    {-" ++ h ++ " HLINT <severity> <identifier> \"Hint name\" " ++ h ++ "-}"
            where h = ['#' | hash]

        g x | (s, x) <- word1 x
            , s /= ""
            , not $ "\"" `isPrefixOf` s
            = first ((if s == "module" then "" else s):) $ g x
        g x = ([], x)
readComment _ = []


readSide :: [Decl_] -> (Maybe Exp_, [Note])
readSide = foldl f (Nothing,[])
    where f (Nothing,notes) (PatBind _ PWildCard{} (UnGuardedRhs _ side) Nothing) = (Just side, notes)
          f (Nothing,notes) (PatBind _ (fromNamed -> "side") (UnGuardedRhs _ side) Nothing) = (Just side, notes)
          f (side,[]) (PatBind _ (fromNamed -> "note") (UnGuardedRhs _ note) Nothing) = (side,g note)
          f _ x = errorOn x "bad side condition"

          g (Lit _ (String _ x _)) = [Note x]
          g (List _ xs) = concatMap g xs
          g x = case fromApps x of
              [con -> Just "IncreasesLaziness"] -> [IncreasesLaziness]
              [con -> Just "DecreasesLaziness"] -> [DecreasesLaziness]
              [con -> Just "RemovesError",fromString -> Just a] -> [RemovesError a]
              [con -> Just "ValidInstance",fromString -> Just a,var -> Just b] -> [ValidInstance a b]
              [con -> Just "RequiresExtension",con -> Just a] -> [RequiresExtension a]
              _ -> errorOn x "bad note"

          con :: Exp_ -> Maybe String
          con c@Con{} = Just $ prettyPrint c; con _ = Nothing
          var c@Var{} = Just $ prettyPrint c; var _ = Nothing


-- Note: Foo may be ("","Foo") or ("Foo",""), return both
readFuncs :: Exp_ -> [(String, String)]
readFuncs (App _ x y) = readFuncs x ++ readFuncs y
readFuncs (Lit _ (String _ "" _)) = [("","")]
readFuncs (Var _ (UnQual _ name)) = [("",fromNamed name)]
readFuncs (Var _ (Qual _ (ModuleName _ mod) name)) = [(mod, fromNamed name)]
readFuncs (Con _ (UnQual _ name)) = [(fromNamed name,""),("",fromNamed name)]
readFuncs (Con _ (Qual _ (ModuleName _ mod) name)) = [(mod ++ "." ++ fromNamed name,""),(mod,fromNamed name)]
readFuncs x = errorOn x "bad classification rule"


getNames :: [Pat_] -> Exp_ -> [String]
getNames ps _ | ps /= [], Just ps <- mapM fromPString ps = ps
getNames [] (InfixApp _ lhs op rhs) | opExp op ~= "==>" = map ("Use "++) names
    where
        lnames = map f $ childrenS lhs
        rnames = map f $ childrenS rhs
        names = filter (not . isUnifyVar) $ (rnames \\ lnames) ++ rnames
        f (Ident _ x) = x
        f (Symbol _ x) = x
getNames _ _ = []


errorOn :: (Annotated ast, Pretty (ast S)) => ast S -> String -> b
errorOn val msg = exitMessageImpure $
    showSrcLoc (getPointLoc $ ann val) ++
    ": Error while reading hint file, " ++ msg ++ "\n" ++
    prettyPrint val

errorOn' :: Outputable a => Located a -> String -> b
errorOn' (L pos val) msg = exitMessageImpure $
    showSrcLoc (getPointLoc $ ghcSpanToHSE pos) ++
    ": Error while reading hint file, " ++ msg ++ "\n" ++
    unsafePrettyPrint val

errorOnComment :: GHC.Located AnnotationComment -> String -> b
errorOnComment c@(L s _) msg = exitMessageImpure $
    let isMultiline = isCommentMultiline c in
    showSrcLoc (ghcSrcLocToHSE $ GHC.srcSpanStart s) ++
    ": Error while reading hint file, " ++ msg ++ "\n" ++
    (if isMultiline then "{-" else "--") ++ commentText c ++ (if isMultiline then "-}" else "")

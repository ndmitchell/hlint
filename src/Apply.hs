
module Apply(applyHints, applyHintFile, applyHintFiles) where

import Control.Applicative
import Data.Monoid
import GHC.All
import Hint.All
import GHC.Util
import Data.Generics.Uniplate.DataOnly
import Idea
import Data.Tuple.Extra
import Data.Either
import Data.List.Extra
import Data.Maybe
import Data.Ord
import Config.Type
import Config.Haskell
import GHC.Types.SrcLoc
import GHC.Hs
import Language.Haskell.GhclibParserEx.GHC.Hs
import qualified Data.HashSet as Set
import Prelude
import Util


-- | Apply hints to a single file, you may have the contents of the file.
applyHintFile :: ParseFlags -> [Setting] -> FilePath -> Maybe String -> IO [Idea]
applyHintFile flags s file src = do
    res <- parseModuleApply flags s file src
    pure $ case res of
        Left err -> [err]
        Right m -> executeHints s [m]


-- | Apply hints to multiple files, allowing cross-file hints to fire.
applyHintFiles :: ParseFlags -> [Setting] -> [FilePath] -> IO [Idea]
applyHintFiles flags s files = do
    (err, ms) <- partitionEithers <$> mapM (\file -> parseModuleApply flags s file Nothing) files
    pure $ err ++ executeHints s ms


-- | Given a way of classifying results, and a 'Hint', apply to a set of modules generating a list of 'Idea's.
--   The 'Idea' values will be ordered within a file.
--
--   Given a set of modules, it may be faster to pass each to 'applyHints' in a singleton list.
--   When given multiple modules at once this function attempts to find hints between modules,
--   which is slower and often pointless (by default HLint passes modules singularly, using
--   @--cross@ to pass all modules together).
applyHints {- PUBLIC -} :: [Classify] -> Hint -> [ModuleEx] -> [Idea]
applyHints cs = applyHintsReal $ map SettingClassify cs

applyHintsReal :: [Setting] -> Hint -> [ModuleEx] -> [Idea]
applyHintsReal settings hints_ ms = concat $
    [ map (classify classifiers . removeRequiresExtensionNotes m) $
        order [] (hintModule hints settings nm m) `merge`
        concat [order (maybeToList $ declName d) $ decHints d | d <- hsmodDecls $ unLoc $ ghcModule m]
    | (nm,m) <- mns
    , let classifiers = cls ++ mapMaybe readPragma (universeBi (ghcModule m)) ++ concatMap readComment (ghcComments m)
    , seq (length classifiers) True -- to force any errors from readPragma or readComment
    , let decHints = hintDecl hints settings nm m -- partially apply
    , let order n = map (\i -> i{ideaModule = f $ modName (ghcModule m) : ideaModule i, ideaDecl = f $ n ++ ideaDecl i}) . sortOn (SrcSpanD . ideaSpan)
    , let merge = mergeBy (comparing (SrcSpanD . ideaSpan))] ++
    [map (classify cls) (hintModules hints settings mns)]
    where
        f = nubOrd . filter (/= "")
        cls = [x | SettingClassify x <- settings]
        mns = map (\x -> (scopeCreate (unLoc $ ghcModule x), x)) ms
        hints = (if length ms <= 1 then noModules else id) hints_
        noModules h = h{hintModules = \_ _ -> []} `mappend` mempty{hintModule = \s a b -> hintModules h s [(a,b)]}

-- If the hint has said you RequiresExtension Foo, but Foo is enabled, drop the note
removeRequiresExtensionNotes :: ModuleEx -> Idea -> Idea
removeRequiresExtensionNotes m = \x -> x{ideaNote = filter keep $ ideaNote x}
    where
        exts = Set.fromList $ concatMap snd $ languagePragmas $ pragmas (comments (hsmodAnn (unLoc . ghcModule $ m)))
        keep (RequiresExtension x) = not $ x `Set.member` exts
        keep _ = True

-- | Given a list of settings (a way to classify) and a list of hints, run them over a list of modules.
executeHints :: [Setting] -> [ModuleEx] -> [Idea]
executeHints s = applyHintsReal s (allHints s)


-- | Return either an idea (a parse error) or the module. In IO because might call the C pre processor.
parseModuleApply :: ParseFlags -> [Setting] -> FilePath -> Maybe String -> IO (Either Idea ModuleEx)
parseModuleApply flags s file src = do
    res <- parseModuleEx (parseFlagsAddFixities [x | Infix x <- s] flags) file src
    case res of
      Right r -> pure $ Right r
      Left (ParseError sl msg ctxt) ->
            pure $ Left $ classify [x | SettingClassify x <- s] $ rawIdeaN Error (adjustMessage msg) sl ctxt Nothing []
    where

        -- important the message has "Parse error:" as the prefix so "--ignore=Parse error" works
        -- try and tidy up things like "parse error (mismatched brackets)" to not look silly
        adjustMessage :: String -> String
        adjustMessage x = "Parse error: " ++ dropBrackets (
            case stripInfix "parse error " x of
              Nothing -> x
              Just (prefix, _) ->
                dropPrefix (prefix ++ "parse error ") x
          )

        dropBrackets ('(':xs) | Just (xs,')') <- unsnoc xs = xs
        dropBrackets xs = xs


-- | Find which hints a list of settings implies.
allHints :: [Setting] -> Hint
allHints xs = mconcat $ hintRules [x | SettingMatchExp x <- xs] : map f builtin
    where builtin = nubOrd $ concat [if x == "All" then map fst builtinHints else [x] | Builtin x <- xs]
          f x = fromMaybe (error $ "Unknown builtin hints: HLint.Builtin." ++ x) $ lookup x builtinHints


-- | Given some settings, make sure the severity field of the Idea is correct.
classify :: [Classify] -> Idea -> Idea
classify xs i = let s = foldl' (f i) (ideaSeverity i) xs in s `seq` i{ideaSeverity=s}
    where
        -- figure out if we need to change the severity
        f :: Idea -> Severity -> Classify -> Severity
        f i r c | classifyHint c ~~= ideaHint i && classifyModule c ~= ideaModule i && classifyDecl c ~= ideaDecl i = classifySeverity c
                | otherwise = r
        x ~= y = x == "" || any (wildcardMatch x) y
        x  ~~= y = x == "" || x == y || ((x ++ ":") `isPrefixOf` y)

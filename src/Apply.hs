
module Apply(applyHints, applyHintFile, applyHintFiles) where

import Control.Applicative
import Data.Monoid
import HSE.All
import Hint.All
import Idea
import Data.Tuple.Extra
import Data.Either
import Data.List.Extra
import Data.Maybe
import Data.Ord
import Config.Type
import Config.Haskell
import Prelude


-- | Apply hints to a single file, you may have the contents of the file.
applyHintFile :: ParseFlags -> [Setting] -> FilePath -> Maybe String -> IO [Idea]
applyHintFile flags s file src = do
    res <- parseModuleApply flags s file src
    return $ case res of
        Left err -> [err]
        Right m -> executeHints s [m]


-- | Apply hints to multiple files, allowing cross-file hints to fire.
applyHintFiles :: ParseFlags -> [Setting] -> [FilePath] -> IO [Idea]
applyHintFiles flags s files = do
    (err, ms) <- partitionEithers <$> mapM (\file -> parseModuleApply flags s file Nothing) files
    return $ err ++ executeHints s ms


-- | Given a way of classifying results, and a 'Hint', apply to a set of modules generating a list of 'Idea's.
--   The 'Idea' values will be ordered within a file.
--
--   Given a set of modules, it may be faster pass each to 'applyHints' in a singleton list.
--   When given multiple modules at once this function attempts to find hints between modules,
--   which is slower and often pointless (by default HLint passes modules singularly, using
--   @--cross@ to pass all modules together).
applyHints :: [Classify] -> Hint -> [(Module_, [Comment])] -> [Idea]
applyHints cs = applyHintsReal $ map SettingClassify cs

applyHintsReal :: [Setting] -> Hint -> [(Module_, [Comment])] -> [Idea]
applyHintsReal settings hints_ ms = concat $
    [ map (classify $ cls ++ mapMaybe readPragma (universeBi m)) $
        order "" (hintModule hints settings nm m) `merge`
        concat [order (fromNamed d) $ decHints d | d <- moduleDecls m] `merge`
        concat [order "" $ hintComment hints settings c | c <- cs]
    | (nm,(m,cs)) <- mns
    , let decHints = hintDecl hints settings nm m -- partially apply
    , let order n = map (\i -> i{ideaModule=moduleName m, ideaDecl=n}) . sortBy (comparing ideaSpan)
    , let merge = mergeBy (comparing ideaSpan)] ++
    [map (classify cls) (hintModules hints settings $ map (second fst) mns)]
    where
        cls = [x | SettingClassify x <- settings]
        mns = map (scopeCreate . fst &&& id) ms
        hints = (if length ms <= 1 then noModules else id) hints_
        noModules h = h{hintModules = \_ _ -> []} `mappend` mempty{hintModule = \s a b -> hintModules h s [(a,b)]}


-- | Given a list of settings (a way to classify) and a list of hints, run them over a list of modules.
executeHints :: [Setting] -> [(Module_, [Comment])] -> [Idea]
executeHints s = applyHintsReal s (allHints s)


-- | Return either an idea (a parse error) or the module. In IO because might call the C pre processor.
parseModuleApply :: ParseFlags -> [Setting] -> FilePath -> Maybe String -> IO (Either Idea (Module_, [Comment]))
parseModuleApply flags s file src = do
    res <- parseModuleEx (parseFlagsAddFixities [x | Infix x <- s] flags) file src
    case res of
        Right m -> return $ Right m
        Left (ParseError sl msg ctxt) -> do
            i <- return $ rawIdeaN Error "Parse error" (mkSrcSpan sl sl) ctxt Nothing []
            i <- return $ classify [x | SettingClassify x <- s] i
            return $ Left i


-- | Find which hints a list of settings implies.
allHints :: [Setting] -> Hint
allHints xs = mconcat $ hintRules [x | SettingMatchExp x <- xs] : map f builtin
    where builtin = nub $ concat [if x == "All" then map fst builtinHints else [x] | Builtin x <- xs]
          f x = fromMaybe (error $ "Unknown builtin hints: HLint.Builtin." ++ x) $ lookup x builtinHints


-- | Given some settings, make sure the severity field of the Idea is correct.
classify :: [Classify] -> Idea -> Idea
classify xs i =  let s = foldl' (f i) (ideaSeverity i) xs in s `seq` i{ideaSeverity=s}
    where
        -- figure out if we need to change the severity
        f :: Idea -> Severity -> Classify -> Severity
        f i r c | classifyHint c ~= ideaHint i && classifyModule c ~= ideaModule i && classifyDecl c ~= ideaDecl i = classifySeverity c
                | otherwise = r
        x ~= y = null x || x == y

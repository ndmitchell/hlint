
module Apply(applyHints, applyHintFile, applyHintFiles) where

import HSE.All
import Hint.All
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Settings
import Idea
import Util


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
    (err, ms) <- unzipEither <$> mapM (\file -> parseModuleApply flags s file Nothing) files
    return $ err ++ executeHints s ms


-- | Given a way of classifying results, and a 'Hint', apply to a set of modules generating a list of 'Idea's.
--   The 'Idea' values will be ordered within a file.
applyHints :: [Classify] -> Hint -> [Module SrcSpanInfo] -> [Idea]
applyHints cls hints_ ms = concat $
    [ map (classify $ cls ++ mapMaybe readPragma (universeBi m)) $
        order "" (hintModule hints nm m) ++
        concat [order (fromNamed d) $ decHints d | d <- moduleDecls m]
    | (nm,m) <- mns
    , let decHints = hintDecl hints nm m -- partially apply
    , let order n = map (\i -> i{ideaModule=moduleName m, ideaDecl=n}) . sortBy (comparing ideaSpan)] ++
    [map (classify cls) (hintModules hints mns)]
    where
        mns = map (scopeCreate &&& id) ms
        hints = (if length ms <= 1 then noModules else id) hints_
        noModules h = h{hintModules = \_ -> []} `mappend` mempty{hintModule = \a b -> hintModules h [(a,b)]}


-- | Given a list of settings (a way to classify) and a list of hints, run them over a list of modules.
executeHints :: [Setting] -> [Module_] -> [Idea]
executeHints s = applyHints [x | SettingClassify x <- s] (allHints s)


-- | Return either an idea (a parse error) or the module. In IO because might call the C pre processor.
parseModuleApply :: ParseFlags -> [Setting] -> FilePath -> Maybe String -> IO (Either Idea Module_)
parseModuleApply flags s file src = do
    res <- parseModuleEx (parseFlagsAddFixities [x | Infix x <- s] flags) file src
    case res of
        Right m -> return $ Right m
        Left (ParseError sl msg ctxt) -> do
            i <- return $ rawIdea Warning "Parse error" (mkSrcSpan sl sl) ctxt Nothing []
            i <- return $ classify [x | SettingClassify x <- s] i
            return $ Left i{ideaHint = if "Parse error" `isPrefixOf` msg then msg else "Parse error: " ++ msg}


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

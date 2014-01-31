
module Apply(applyHintFile, applyHintFiles) where

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


-- | Given a list of settings (a way to classify) and a list of hints, run them over a list of modules.
executeHints :: [Setting] -> [Module_] -> [Idea]
executeHints s ms = concat $
    [ map (classify $ s ++ mapMaybe readPragma (universeBi m)) $
        order "" (hintModule hints nm m) ++
        concat [order (fromNamed d) $ decHints d | d <- moduleDecls m]
    | (nm,m) <- mns
    , let decHints = hintDecl hints nm m -- partially apply
    , let order n = map (\i -> i{func = (moduleName m,n)}) . sortBy (comparing loc)] ++
    [map (classify s) (hintModules hints mns)]
    where
        mns = map (scopeCreate &&& id) ms

        hints = (if length ms <= 1 then noModules else id) $ mconcat $ allHints s
        noModules h = h{hintModules = \_ -> []} `mappend` mempty{hintModule = \a b -> hintModules h [(a,b)]}


-- | Return either an idea (a parse error) or the module. In IO because might call the C pre processor.
parseModuleApply :: ParseFlags -> [Setting] -> FilePath -> Maybe String -> IO (Either Idea Module_)
parseModuleApply flags s file src = do
    res <- parseModuleEx (parseFlagsAddFixities [x | Infix x <- s] flags) file src
    case res of
        Right m -> return $ Right m
        Left (ParseError sl msg ctxt) -> return $ Left $ classify s $ ParseFailure Warning "Parse error" sl msg ctxt


-- | Find which hints a list of settings implies.
allHints :: [Setting] -> [Hint]
allHints xs = dynamicHints xs : map f builtin
    where builtin = nub $ concat [if x == "All" then map fst builtinHints else [x] | Builtin x <- xs]
          f x = fromMaybe (error $ "Unknown builtin hints: HLint.Builtin." ++ x) $ lookup x builtinHints


-- | Given some settings, make sure the severity field of the Idea is correct.
classify :: [Setting] -> Idea -> Idea
classify xs i = i{severity = foldl' (f i) (severity i) $ filter isClassify xs}
    where
        -- figure out if we need to change the severity
        f :: Idea -> Severity -> Setting -> Severity
        f i r c | matchHint (hintS c) (hint i) && matchFunc (funcS c) (func_ i) = severityS c
                | otherwise = r

        func_ x = if isParseFailure x then ("","") else func x
        matchHint = (~=)
        matchFunc (x1,x2) (y1,y2) = (x1~=y1) && (x2~=y2)
        x ~= y = null x || x == y

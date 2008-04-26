
module Main where

import Control.Monad
import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Language.Haskell.Exts
import System.Console.GetOpt
import System.Environment
import System.Exit

---------------------------------------------------------------------
-- COMMAND LINE OPTIONS


data Opts = Help | HintFile FilePath | Test
            deriving Eq

opts = [Option "?" ["help"] (NoArg Help) "Display help message"
       ,Option "h" ["hint"] (ReqArg HintFile "file") "Hint file to use"
       ,Option "t" ["test"] (NoArg Test) "Run in test mode"
       ]

main = do
    args <- getArgs
    let (opt,files,err) = getOpt Permute opts args
    when (not $ null err) $
        error $ unlines $ "Unrecognised arguments:" : err

    when (Help `elem` opt || null files) $ do
        putStr $ unlines ["Dr Haskell, (C) Neil Mitchell 2006-2008, University of York"
                         ,usageInfo "" opts
                         ,"Dr Haskell makes hints on how to improve some Haskell code."]
        exitWith ExitSuccess

    let hintFiles = [x | HintFile x <- opt]
    hints <- liftM concat $ mapM readHints $ if null hintFiles then ["Hints.hs"] else hintFiles

    let test = Test `elem` opt
    n <- liftM sum $ mapM (runFile test hints) files
    when test $ putStrLn $ "Tests " ++ if n == 0 then "passed" else "failed"
    if n == 0
        then putStrLn "No relevant suggestions"
        else putStrLn $ "Found " ++ show n ++ " suggestions"


runFile test hints file = do
    src <- parseFile2 file
    if not test then do
        let ideas = findIdeas hints src
        putStr $ unlines $ map showIdea ideas
        return $ length ideas
     else do
        let HsModule _ _ _ _ tests = src
        liftM sum $ mapM f tests
    where
        f o | ("_NO" `isSuffixOf` name) == null ideas = return 0
            | otherwise = do
                putStrLn $ "Test failed in " ++ name ++ concatMap ((++) " | " . showIdea) ideas
                return 1
            where
                ideas = findIdeas hints o
                name = declName o


declName (HsPatBind _ (HsPVar (HsIdent name)) _ _) = name
declName (HsFunBind (HsMatch _ (HsIdent name) _ _ _ : _)) = name
declName x = error $ "declName: " ++ show x


parseFile2 file = do
    res <- parseFile file
    case res of
        ParseOk x -> return x
        _ -> error $ "Failed to parse: " ++ file


---------------------------------------------------------------------
-- HINTS

data Hint = Hint {hintName :: String, hintExp :: HsExp}
            deriving (Show,Eq)


readHints :: FilePath -> IO [Hint]
readHints file = do
    res <- parseFile2 file
    return $ map readHint $ childrenBi res


readHint :: HsDecl -> Hint
readHint (HsFunBind [HsMatch src (HsIdent name) free (HsUnGuardedRhs bod) (HsBDecls [])]) = Hint name (transformBi f bod)
    where
        vars = [x | HsPVar (HsIdent x) <- free]
        f (HsIdent x) | x `elem` vars = HsIdent "?"
        f x = x


---------------------------------------------------------------------
-- IDEAS

data Idea = Idea {hint :: Hint, loc :: SrcLoc}
            deriving (Show,Eq)


showIdea :: Idea -> String
showIdea idea = showSrcLoc (loc idea) ++ " " ++ showHint (hint idea)


showSrcLoc (SrcLoc file line col) = file ++ ":" ++ show line ++ ":" ++ show col ++ ":"


showHint :: Hint -> String
showHint = hintName


findIdeas :: Data a => [Hint] -> a -> [Idea]
findIdeas hints = nub . f (SrcLoc "" 0 0)
    where
        f :: Data a => SrcLoc -> a -> [Idea]
        f pos x = case cast x of
                      Just y -> matchIdeas hints pos y ++ rest
                      Nothing -> rest
            where
                rest = concat $ gmapQ (f pos2) x
                pos2 = fromMaybe pos $ getSrcLoc x


getSrcLoc :: Data a => a -> Maybe SrcLoc
getSrcLoc x = head $ gmapQ cast x ++ [Nothing]


matchIdeas :: [Hint] -> SrcLoc -> HsExp -> [Idea]
matchIdeas hints pos x = [Idea h pos | h <- hints, matchIdea h x]


matchIdea :: Hint -> HsExp -> Bool
matchIdea hint x = hintExp hint ==? x


(==?) :: HsExp -> HsExp -> Bool
(==?) x y | x == free || y == free = True
(==?) x y = any (defEq nx) (reduce ny)
    where (nx, ny) = (norm1 x, norm1 y)

defEq x y = descend (const HsWildCard) x == descend (const HsWildCard) y &&
            and (zipWith (==?) (children x) (children y))



-- normalise them to one level
-- try to make them a bit more equal
norm1 :: HsExp -> HsExp
norm1 (HsInfixApp lhs (HsQVarOp op) rhs) = HsVar op `HsApp` lhs `HsApp` rhs
norm1 (HsParen x) = norm1 x
norm1 x = x


-- try to reduce the thing as much as possible
reduce :: HsExp -> [HsExp]
reduce o = [o] ++
    [x `HsApp` (y `HsApp` free) | HsVar (UnQual (HsSymbol ".")) `HsApp` x `HsApp` y <- [o]]


free = HsVar (UnQual (HsIdent "?"))

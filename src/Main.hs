
module Main where

import Control.Monad
import Data.Generics
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Language.Haskell.Exts

import CmdLine


main = do
    mode <- getMode
    hints <- liftM concat $ mapM readHints $ modeHints mode
    let test = modeTest mode

    n <- liftM sum $ mapM (runFile test hints) (modeFiles mode)
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
        f o | ("_NO" `isSuffixOf` name) == null ideas && length ideas <= 1 = return 0
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
        f x = case fromVar x of
                  Just v | v `elem` vars -> toVar $ '?' : v
                  _ -> x


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
matchIdea hint x = doesUnify $ simplify (hintExp hint) ==? simplify x


data Unify = Unify String HsExp
           | Failure
             deriving (Eq, Show)


doesUnify :: [Unify] -> Bool
doesUnify xs | Failure `elem` xs = False
             | otherwise = f [(x,y) | Unify x y <- xs]
    where
        f :: [(String,HsExp)] -> Bool
        f xs = all g vars
            where
                vars = nub $ map fst xs
                g v = (==) 1 $ length $ nub [b | (a,b) <- xs, a == v]


(==?) :: HsExp -> HsExp -> [Unify]
(==?) x y | not $ null vars = vars
          | descend (const HsWildCard) x == descend (const HsWildCard) y = concat $ zipWith (==?) (children x) (children y)
          | otherwise = [Failure]
    where
        vars = [Unify v y | Just ('?':v) <- [fromVar x]]


simplify :: HsExp -> HsExp
simplify = transform f
    where
        f (HsInfixApp lhs (HsQVarOp op) rhs) = simplify $ HsVar op `HsApp` lhs `HsApp` rhs
        f (HsParen x) = simplify x
        f (HsVar (UnQual (HsSymbol ".")) `HsApp` x `HsApp` y) = simplify $ x `HsApp` (y `HsApp` var)
            where var = toVar $ '?' : freeVar (HsApp x y)
        f (HsVar (UnQual (HsSymbol "$")) `HsApp` x `HsApp` y) = simplify $ x `HsApp` y
        f x = x


-- pick a variable that is not being used
freeVar :: Data a => a -> String
freeVar x = head $ allVars \\ concat [[y, drop 1 y] | HsIdent y <- universeBi x]
    where allVars = [letter : number | number <- "" : map show [1..], letter <- ['a'..'z']]


fromVar (HsVar (UnQual (HsIdent x))) = Just x
fromVar _ = Nothing

toVar = HsVar . UnQual . HsIdent

isVar = isJust . fromVar

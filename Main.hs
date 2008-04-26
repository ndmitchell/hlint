
module Main where

import Language.Haskell.Exts
import System.Environment
import Data.Generics.PlateData
import Data.Generics
import Data.Maybe
import Data.List

---------------------------------------------------------------------
-- COMMAND LINE OPTIONS


main = do
    hints <- readHints "Hints.hs"
    src <- parseFile2 "Sample.hs"
    let ideas = findIdeas hints src
    putStr $ unlines $ map showIdea ideas
    if null ideas
        then putStrLn "No relevant suggestions"
        else putStrLn $ "Found " ++ show (length ideas) ++ " suggestions"

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


findIdeas :: [Hint] -> HsModule -> [Idea]
findIdeas hints src@(HsModule pos _ _ _ _) = nub $ f pos src
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

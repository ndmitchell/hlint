{-# LANGUAGE ViewPatterns #-}

module Ignore(ignore) where

import qualified Data.Map as Map
import Data.Char
import Data.List
import Type


type Ignore = Map.Map String [String]


ignore :: [FilePath] -> [String] -> IO (Idea -> Bool)
ignore files extra = do
    src <- mapM readFile files
    let src2 = filter (\x -> not $ null x || "#" `isPrefixOf` x) $ map (dropWhile isSpace) $ concatMap lines src
    let mp = foldl' addIgnore Map.empty $ src2 ++ extra
    let always = Map.findWithDefault [] "" mp
    return $ \i ->
        let check = any (`isPrefixOf` dropMain (key i)) 
        in check always ||
            case Map.lookup (text i) mp of
                Nothing -> False
                Just v -> null v || check v


addIgnore :: Ignore -> String -> Ignore
addIgnore mp s
    | null opts && invert = Map.delete key mp
    | null opts           = Map.insert key [] mp
    | invert    = Map.adjust (\\ opts) key mp
    | otherwise = Map.insertWith (++) key opts mp
    where (invert,key,opts) = parseIgnore s


parseIgnore :: String -> (Bool,String,[String])
parseIgnore ('!':xs) = let (_,a,b) = parseIgnore (dropWhile isSpace xs) in (True,a,b)
parseIgnore ('{':(break (=='}') -> (keys, '}':text))) = (False, dropWhile isSpace text, map dropMain $ words keys)
parseIgnore x = (False, x, [])


dropMain x = if "Main." `isPrefixOf` x then drop 5 x else x

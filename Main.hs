
module Main where

import Core.CoreType
import System
import Char

type Hints = [(String, CoreExpr)]


main = do x <- getArgs
          system $ "yhc -corep Hints.hs"
          hints <- readFile "Hints.ycr"
          let hint = getHints $ readCore hints
          mapM_ (f hint) x
    where
        f hint x = do
            system $ "yhc -corep " ++ x
            src <- readFile $ takeWhile (/= '.') x ++ ".ycr"
            mapM_ putStrLn $ doChecks hint (readCore src)


readCore :: String -> Core
readCore = simplify . read


getHints :: Core -> Hints
getHints (Core _ _ x) = [(name, noPos expr) | CoreFunc (CoreApp (CoreVar name) _) expr <- x]


doChecks :: Hints -> Core -> [String]
doChecks hints (Core _ _ cr) =
    ["I can apply " ++ hname ++ " in " ++ fname |
         (CoreFunc (CoreApp (CoreVar fname) _) fexpr) <- cr,
         (hname, hexpr) <- hints,
         any (doesMatch hexpr) (allCore fexpr)]


doesMatch :: CoreExpr -> CoreExpr -> Bool
doesMatch (CoreVar name) x | isLower (head name) = True
doesMatch (CoreVar a) (CoreVar b) = a == b
doesMatch (CoreApp a1 b1) (CoreApp a2 b2) = doesMatch a1 a2 && doesMatchList b1 b2
doesMatch _ _ = False



doesMatchList [] [] = True
doesMatchList (x:xs) (y:ys) = doesMatch x y && doesMatchList xs ys
doesMatchList _ _ = False




simplify :: Core -> Core
simplify x = mapCore f x
    where
        f (CoreApp x []) = x
        f (CoreApp (CoreVar "Prelude..") [x,CoreApp y xs]) = CoreApp x [CoreApp y (ensure2 xs)]
        f x = x
        
        ensure2 [] = [CoreVar "x",CoreVar "x"]
        ensure2 [x] = [x, CoreVar "x"]
        ensure2 [x,y] = [x,y]


noPos x = mapCore f x
    where
        f (CorePos x y) = y
        f x = x



class PlayCore a where
    mapCore :: (CoreExpr -> CoreExpr) -> a -> a
    allCore :: a -> [CoreExpr]


instance PlayCore CoreExpr where
    mapCore f x = f $ case x of
                          CoreApp x xs -> CoreApp (mapCore f x) (mapCore f xs)
                          CoreCase x xs -> CoreCase (mapCore f x) [(mapCore f a, mapCore f b) | (a,b) <- xs]
                          CoreLet x xs -> CoreLet (mapCore f x) (mapCore f xs)
                          CorePos x xs -> CorePos x (mapCore f xs)
                          _ -> x

    allCore x = x : concatMap allCore (case x of
                          CoreApp x xs -> x:xs
                          CoreCase x xs -> x: concat [[a,b] | (a,b) <- xs]
                          CoreLet x xs -> allCore x ++ [xs]
                          CorePos x xs -> [xs]
                          _ -> [])

instance PlayCore Core where
    mapCore f (Core a x xs) = Core a x (mapCore f xs)
    allCore (Core a x xs) = allCore xs

instance PlayCore CoreItem where
    mapCore f (CoreFunc x y) = CoreFunc (mapCore f x) (mapCore f y)
    mapCore f x = x
    
    allCore (CoreFunc x y) = allCore x ++ allCore y
    allCore x = []

instance PlayCore a => PlayCore [a] where
    mapCore f xs = map (mapCore f) xs
    allCore xs = concatMap allCore xs


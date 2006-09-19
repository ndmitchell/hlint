
module Main where

import Core.CoreType
import System.Cmd
import System.Environment
import Data.Char
import Data.List

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
            let res = doChecks hint (readCore src)
            if null res
                then putStrLn "No hints for this program"
                else mapM_ putStrLn res


readCore :: String -> Core
readCore = simplify . read


getHints :: Core -> Hints
getHints (Core _ _ x) = [(name, noPos expr) | CoreFunc (CoreApp (CoreVar name) _) expr <- x]


doChecks :: Hints -> Core -> [String]
doChecks hints (Core modu _ cr) =
    ["I can apply " ++ getName hname ++ " in " ++ getName fname ++ getPos fexpr |
         (CoreFunc (CoreApp (CoreVar fname) _) fexpr) <- reverse cr,
         (hname, hexpr) <- hints,
         any (doesMatch hexpr) (allCore fexpr)]
    where
        getName x = concat $ intersperse "." [as | as@(a:_) <- splitMods x, isLower a]

        splitMods x = if null b then [a] else a : splitMods (tail b)
            where (a,b) = break (== '.') x
            
        getPos (CorePos msg x) = " (" ++ msg ++ ")"
        getPos _ = ""


doesMatch :: CoreExpr -> CoreExpr -> Bool
doesMatch (CoreVar name) x | isLower (head name) = True
doesMatch (CoreApp a1 b1) (CoreApp a2 b2) = doesMatch a1 a2 && doesMatchList b1 b2
doesMatch a b = a == b



doesMatchList [] [] = True
doesMatchList (x:xs) (y:ys) = doesMatch x y && doesMatchList xs ys
doesMatchList _ _ = False




simplify :: Core -> Core
simplify (Core a b cs) = Core a b (map g cs)
    where
        g (CoreFunc x y) = CoreFunc x (mapTop f y)
        g x = x
    
        f (CoreApp x []) = x
        f (CoreApp (CoreApp x y) z) = CoreApp x (y++z)
        f (CoreApp (CoreVar "Prelude..") [x,y,z]) = f $ CoreApp x [f $ CoreApp y [z]]
        f (CoreApp (CoreVar "Prelude..") [x,y]) = f $ CoreApp (CoreVar "Prelude..") [x,y,CoreVar "?"]
        f x = x


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

-- map top to bottom
mapTop :: (CoreExpr -> CoreExpr) -> CoreExpr -> CoreExpr
mapTop f x = case f x of
                 CoreApp x xs -> CoreApp (g x) (gs xs)
                 CoreCase x xs -> CoreCase (g x) [(g a, g b) | (a,b) <- xs]
                 CoreLet x xs -> CoreLet x (g xs)
                 CorePos x xs -> CorePos x (g xs)
                 x -> x
    where
        g = mapTop f
        gs = map g


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


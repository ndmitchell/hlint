
module Main where

import Yhc.Core
import System.Cmd
import System.Environment
import Control.Monad
import Data.Char
import Data.List

type Hints = [(String, CoreExpr)]


main = do x <- getArgs
          system $ "yhc -corep Hints.hs"
          hints <- liftM simplify $ loadCore "Hints.ycr"
          let hint = getHints hints
          mapM_ (f hint) x
    where
        f hint x = do
            system $ "yhc -corep " ++ x
            src <- loadCore $ takeWhile (/= '.') x ++ ".ycr"
            let res = doChecks hint (simplify src)
            if null res
                then putStrLn "No hints for this program"
                else mapM_ putStrLn res


getHints :: Core -> Hints
getHints (Core _ _ x) = [(name, noPos expr) | CoreFunc name _ expr <- x]


doChecks :: Hints -> Core -> [String]
doChecks hints (Core modu _ cr) =
    ["I can apply " ++ getName hname ++ " in " ++ getName fname ++ getPos fexpr |
         (CoreFunc fname _ fexpr) <- reverse cr,
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
simplify x = mapOverCore f x
    where
        f (CoreApp x []) = x
        f (CoreApp (CoreApp x y) z) = CoreApp x (y++z)
        f (CoreApp (CoreVar "Prelude..") [x,y,z]) = f $ CoreApp x [f $ CoreApp y [z]]
        f (CoreApp (CoreVar "Prelude..") [x,y]) = f $ CoreApp (CoreVar "Prelude..") [x,y,CoreVar "?"]
        f (CoreApp (CoreVar "Prelude.$") [x,y]) = f $ CoreApp x [y]
        f x = x


noPos x = mapUnderCore f x
    where
        f (CorePos x y) = y
        f x = x

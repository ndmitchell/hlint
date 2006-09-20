
module Main where

import Yhc.Core
import System.Cmd
import System.Environment
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List

type Hints = [(String, CoreExpr)]


main = do x <- getArgs
          system $ "yhc -corep Hints.hs"
          hints <- liftM simplify $ loadCore "Hints.ycr"
          let hint = getHints hints
          mapM_ (f hints hint) x
    where
        f core hint x = do
            system $ "yhc -corep " ++ x
            src <- loadCore $ takeWhile (/= '.') x ++ ".ycr"
            let res = doChecks (core,hint) (simplify src)
            if null res
                then putStrLn "No hints for this program"
                else mapM_ putStrLn res


getHints :: Core -> Hints
getHints core = [(hname, noPos expr) | CoreFunc name _ expr <- coreFuncs core,
                                       let hname = getName name, not $ null hname]


doChecks :: (Core,Hints) -> Core -> [String]
doChecks (c1,hints) core =
    ["I can apply " ++ getName hname ++ " in " ++ getName fname ++ getPos fexpr |
         (CoreFunc fname _ fexpr) <- reverse $ coreFuncs core,
         (hname, hexpr) <- hints,
         any (\x -> doesMatch (c1,hexpr) (core,x)) (allCore fexpr)]
    where
        getPos (CorePos msg x) = " (" ++ msg ++ ")"
        getPos _ = ""


getName x = concat $ intersperse "." [as | as@(a:_) <- splitMods x, isLower a]
    where
        splitMods x = if null b then [a] else a : splitMods (tail b)
            where (a,b) = break (== '.') x


doesMatch :: (Core, CoreExpr) -> (Core, CoreExpr) -> Bool
doesMatch (_,CoreVar name) x | isLower (head name) = True
doesMatch (c1,CoreApp a1 b1) (c2,CoreApp a2 b2) = doesMatchList c1 c2 (a1:b1) (a2:b2)
doesMatch (c1, CoreVar a) (c2, CoreVar b) | isLambda a && isLambda b = doesEqual (coreFunc c1 a) (coreFunc c2 b)
doesMatch (_,a) (_,b) = a == b


doesMatchList c1 c2 [] [] = True
doesMatchList c1 c2 (x:xs) (y:ys) = doesMatch (c1,x) (c2,y) && doesMatchList c1 c2 xs ys
doesMatchList _ _ _ _ = False


-- are two functions the same (ish)
doesEqual :: CoreFunc -> CoreFunc -> Bool
doesEqual (CoreFunc _ a1 a2) (CoreFunc _ b1 b2) = a2 == mapUnderCore f b2
    where
        f (CoreVar x) | x `elem` a1 = CoreVar $ fromJust $ lookup x $ zip b1 a1
        f x = x


-- is it a lambda introduced by Yhc (i.e. not a top level func)
isLambda :: String -> Bool
isLambda = any (isPrefixOf "._LAMBDA") . tails


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

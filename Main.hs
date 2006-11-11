
module Main where

import System.Cmd
import System.Environment
import System.Exit
import System.Directory
import System.FilePath.Version_0_10
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List

import Yhc.Core

---------------------------------------------------------------------
-- COMMAND LINE OPTIONS


helpOpts = ["version","v","h","help","?"]
testOpts = ["test","t"]

optChar = "-/"
isOpt (x:xs) = x `elem` optChar
fromOpt = map toLower . dropWhile (`elem` optChar)
hasOpt opts query = any (`elem` query) opts


helpMsg =
    ["Dr Haskell, (C) Neil Mitchell 2006, University of York"
    ,""
    ,"   drhaskell [options] files"
    ,""
    ,"-help  - display this help message"
    ,"-test  - test the program (for development only)"
    ,""
    ,"Dr Haskell spots common patterns in beginner code that map"
    ,"naturally to a given standard function."
    ]


---------------------------------------------------------------------
-- MAIN DRIVER

main = do
    args <- getArgs
    let (opt,files) = partition isOpt args
        opts = map fromOpt opt
        deadOpts = opts \\ (helpOpts ++ testOpts)
    
    case () of
        _ | not $ null deadOpts -> error $ "Unrecognised options: " ++ concat (intersperse ", " deadOpts)
        _ | hasOpt opts helpOpts || null files -> putStr $ unlines helpMsg
        _ -> do
            hints <- loadHints
            mapM_ (mainFile hints (hasOpt opts testOpts)) files

mainFile :: Hints -> Bool -> FilePath -> IO ()
mainFile hints testMode file = do
    core <- loadHaskellCore file
    let res = doChecks hints core
    if null res
        then putStrLn "No hints for this program"
        else mapM_ putStrLn res


-- load a Haskell file to a Core file
loadHaskellCore :: FilePath -> IO Core
loadHaskellCore file = do
    let files = if hasExtension file then [file]
                else [replaceExtension file "hs", replaceExtension file "lhs"]
    exists <- mapM doesFileExist files
    let fil = [a | (a,b) <- zip files exists, b]
    when (null fil) $ error $ "Could not find file, " ++ file
    let fileHs = head fil
        fileCr = replaceExtension fileHs "ycr"
    
    -- fileHs and fileCr are now the Haskell and the Core file
    fileCrExist <- doesFileExist fileCr
    
    rebuild <- if not fileCrExist then return True else do
        fileHsTime <- getModificationTime fileHs
        fileCrTime <- getModificationTime fileCr
        return $ fileHsTime > fileCrTime 
    
    when rebuild $ do
        response <- system $ "yhc -core " ++ fileHs
        when (response /= ExitSuccess) $ error $ "Failed to build file using Yhc: " ++ fileHs
        exist <- doesFileExist fileCr
        when (not exist) $ error $ "Build with Yhc, but core file not created: " ++ fileHs
    
    liftM simplify $ loadCore fileCr


---------------------------------------------------------------------
-- HINT LOADING

type Hints = [Hint]

data Hint = HintExpr Core String CoreExpr
          | HintFunc Core String CoreExpr

hintName (HintExpr _ x _) = x
hintName (HintFunc _ x _) = x

instance Show Hint where
    show (HintExpr _ name expr) = "EXPR: " ++ name ++ " = " ++ show expr
    show (HintFunc _ name expr) = "FUNC: " ++ name ++ " = " ++ show expr

    showList x = showString $ "\n" ++ unlines (map show x)


loadHints :: IO Hints
loadHints = liftM getHints $ loadHaskellCore "Hints.hs"


getHints :: Core -> Hints
getHints core = concatMap f (coreFuncs core)
    where
        name = coreName core ++ "."
    
        f func | name `isPrefixOf` fname && not (isLambda nam) && not ('.' `elem` nam) 
               = g body
            where
                body = noPos $ coreFuncBody func
                fname = coreFuncName func
                nam = drop (length name) fname

                g (CoreApp (CoreFun call) args)
                    | name `isPrefixOf` call = [HintFunc core nam $ getHintFunc call args]
                g (CoreFun call) = g (CoreApp (CoreFun call) [])
                g x = [HintExpr core nam x]

        f func = []
        
        
        getHintFunc call args = mapUnderCore g $ coreFuncBody $ coreFunc core call
            where
                g (CoreApp (CoreFun c) as) | c == call
                    = CoreApp (CoreFun "RECURSIVE") (drop (length args) as)
                g x = x


---------------------------------------------------------------------
-- CORE MATCHING

doChecks :: Hints -> Core -> [String]
doChecks hints core =
    ["I can apply " ++ hintName hint ++ " in " ++ getName fname ++ getPos fexpr |
         func@(CoreFunc fname _ fexpr) <- {- reverse $ -} coreFuncs core,
         hint <- hints,
         canApply hint core func]
    where
        getPos (CorePos msg x) = " (" ++ msg ++ ")"
        getPos _ = ""


-- given a hint, and a core expr (within a given core)
canApply :: Hint -> Core -> CoreFunc -> Bool
canApply (HintExpr hcore _ hexpr) core expr =
    any (\x -> doesMatch (hcore,hexpr) (core,x)) (allCore $ coreFuncBody expr)

canApply (HintFunc hcore _ hexpr) core (CoreFunc fname _ fbody)
        = doesMatch (hcore,hexpr) (core,body)
    where
        body = mapUnderCore f fbody
        
        f (CoreFun x) | x == fname = CoreFun "RECURSIVE"
        f x = x


-- get a shortened name
getName :: String -> String
getName x = if null res then x else res
    where
        res = concat $ intersperse "." [as | as@(a:_) <- splitMods x, isLower a]
    
        splitMods x = if null b then [a] else a : splitMods (tail b)
            where (a,b) = break (== '.') x


doesMatch :: (Core, CoreExpr) -> (Core, CoreExpr) -> Bool
doesMatch (c1, a1) (c2, a2) =
        all (isLowerCore.fst) res && length (nub $ map fst res) == length res
    where
        isLowerCore (CoreVar x) = isLower (head x)
        isLowerCore _ = False
    
        res = nub $ filter (uncurry (/=)) $ f a1 a2
        
        -- try and simplify where possible
        f :: CoreExpr -> CoreExpr -> [(CoreExpr,CoreExpr)]
        f (CoreApp a1 b1) (CoreApp a2 b2) = fs (a1:b1) (a2:b2)
        f (CoreFun a) (CoreFun b) | isLambda a && isLambda b = 
            if doesEqual (coreFunc c1 a) (coreFunc c2 b) then [] else [false]
        
        f (CoreCase a1 b1) (CoreCase a2 b2) = f a1 a2 ++ g b1 b2
            where
                g [] [] = []
                g (x:xs) (y:ys) = g2 x y ++ g xs ys
                g _ _ = [false]
                
                g2 (a1,b1) (a2,b2) | a1 == a2 = f b1 b2
                g2 (CoreApp (CoreCon x1) x2,x3) (CoreApp (CoreCon y1) y2,y3)
                    | x1 == y1 = f x3 (replaceFree (zip (map fromCoreVar y2) x2) y3)
                g2 _ _ = [false]
            
        f a b = [(a,b)]
        
        fs a b = concat $ zipWith f a b
        
        false = (CoreCon "0", CoreCon "1")


-- are two functions the same (ish)
doesEqual :: CoreFunc -> CoreFunc -> Bool
doesEqual (CoreFunc _ a1 a2) (CoreFunc _ b1 b2) = noPos a2 == noPos (mapUnderCore f b2)
    where
        f (CoreVar x) | x `elem` b1 = CoreVar $ fromJust $ lookup x $ zip b1 a1
        f x = x



---------------------------------------------------------------------
-- MANIPULATE CORE AND UTILITIES

-- is it a lambda introduced by Yhc (i.e. not a top level func)
isLambda :: String -> Bool
isLambda = any (isPrefixOf "_LAMBDA") . tails


dataTypes = [
                [("Prelude.True",0)
                ,("Prelude.False",0)]
            ,
                [("Prelude.:",2)
                ,("Prelude.[]",0)]
            ]


-- simplify and normalise a Core data structure
-- in particular:
-- * remove (.) and ($) if possible
-- * expand out case statements such as (:) then _, with [] after
-- * put case statements in a given order (sorted)
simplify :: Core -> Core
simplify x = mapOverCore f x
    where
        f (CoreApp x []) = x
        f (CoreApp (CoreApp x y) z) = f $ CoreApp x (y++z)
        f (CoreApp (CoreFun "Prelude..") [x,y,z]) = f $ CoreApp x [f $ CoreApp y [z]]
        f (CoreApp (CoreFun "Prelude..") [x,y]) = f $ CoreApp (CoreFun "Prelude..") [x,y,CoreVar "?"]
        f (CoreApp (CoreFun "Prelude.$") [x,y]) = f $ CoreApp x [y]
        f (CoreCase on alts) = CoreCase on (sortBy (\x y -> cmp (fst x) (fst y)) (g alts))
        f x = x
        
        cmp (CoreApp x _) y = cmp x y
        cmp x (CoreApp y _) = cmp x y
        cmp (CoreVar _) (CoreVar _) = EQ
        cmp (CoreVar _) _ = LT
        cmp _ (CoreVar _) = GT
        cmp (CoreCon x) (CoreCon y) = compare x y
        cmp _ _ = EQ

        g orig@[a@(CoreApp (CoreCon a1) a2,a3),(CoreVar "_",b1)] =
            case [(c,i) | dats <- dataTypes, a1 `elem` map fst dats, (c,i) <- dats, c /= a1] of
                [(c,i)] -> [a, ((if null args then id else (`CoreApp` args)) (CoreCon c), b1)]
                    where args = [CoreVar ("n" ++ show j) | j <- [1..i]]
                _ -> orig
        g x = x


-- remove all position information from a Core expression
noPos :: PlayCore a => a -> a
noPos x = mapUnderCore f x
    where
        f (CorePos x y) = y
        f x = x

-- replace all free variables in the replacement list
replaceFree :: [(String, CoreExpr)] -> CoreExpr -> CoreExpr
replaceFree reps x = mapUnderCore f x
    where
        f (CoreVar x) = case lookup x reps of
                            Just y -> y
                            Nothing -> CoreVar x
        f x = x

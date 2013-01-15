{-# LANGUAGE RecordWildCards #-}

module Proof(proof) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import System.FilePath
import Settings
import HSE.All


data Theorem = Theorem
    {original :: Maybe Setting
    ,location :: String
    ,lemma :: String
    }

instance Eq Theorem where
    t1 == t2 = lemma t1 == lemma t2

instance Show Theorem where
    show Theorem{..} = location ++ ":\n" ++ maybe "" f original ++ lemma ++ "\n"
        where f MatchExp{..} = "(* " ++ prettyPrint lhs ++ " ==> " ++ prettyPrint rhs ++ " *)\n"

proof :: [FilePath] -> [Setting] -> FilePath -> IO ()
proof reports hints thy = do
    got <- fmap (isabelleTheorems (takeFileName thy)) $ readFile thy
    let want = nub $ hintTheorems hints
    let unused = got \\ want
    let missing = want \\ got
    let reasons = map (\x -> (fst $ head x, map snd x)) $ groupBy ((==) `on` fst) $
                  sortBy (compare `on` fst) $ map (classifyMissing &&& id) missing
    putStr $ table $ let (*) = (,) in
        ["HLint hints" * want
        ,"HOL proofs" * got
        ,"Useful proofs" * (got `intersect` want)
        ,"Unused proofs" * unused
        ,"Unproved hints" * missing] ++
        [("  " ++ name) * ps | (name,ps) <- reasons]
    forM_ reports $ \report -> do
        let out = ("Unused proofs",unused) : map (first ("Unproved hints - " ++)) reasons
        writeFile report $ unlines $ concat
            [("== " ++ a ++ " ==") : "" : map show b | (a,b) <- out]
        putStrLn $ "Report written to " ++ report
    where
        table xs = unlines [a ++ replicate (n + 6 - length a - length bb) ' ' ++ bb | (a,b) <- xs, let bb = show $ length b]
            where n = maximum $ map (length . fst) xs


missingFuncs = let a*b = [(b,a) | b <- words b] in concat
    ["IO" * "putChar putStr print putStrLn getLine getChar getContents hReady hPrint stdin"
    ,"Exit" * "exitSuccess"
    ,"Ord" * "(>) (<=) (>=) (<) compare minimum maximum sort sortBy"
    ,"Show" * "show shows showIntAtBase"
    ,"Read" * "reads read"
    ,"String" * "lines unlines words unwords"
    ,"Monad" * "mapM mapM_ sequence sequence_ msum mplus mzero liftM when unless return join void (<=<) (>=>) forever ap"
    ,"Functor" * "fmap"
    ,"Numeric" * "(+) (*) fromInteger fromIntegral negate log (/) (-) subtract sqrt even odd"
    ,"Char" * "isControl isPrint isUpper isLower isAlpha isDigit"
    ,"Arrow" * "second first (***) (&&&)"
    ,"Applicative+" * "traverse for traverse_ for_ pure (<|>) (<**>)"
    ,"Strictness" * "$! seq evaluate"
    ,"Exception" * "catch handle catchJust bracket error toException"
    ,"WeakPtr" * "mkWeak"
    ]


-- | Guess why a theorem is missing
classifyMissing :: Theorem -> String
classifyMissing Theorem{original = Just MatchExp{..}}
    | _:_ <- [v :: Exp_ | v@Case{} <- universeBi (lhs,rhs)] = "case"
    | _:_ <- [v :: Exp_ | v@ListComp{} <- universeBi (lhs,rhs)] = "list-comp"
    | v:_ <- mapMaybe (`lookup` missingFuncs) [prettyPrint (v :: Name SrcSpanInfo) | v <- universeBi (lhs,rhs)] = v
classifyMissing _ = "?unknown"


-- Extract theorems out of Isabelle code (HLint.thy)
isabelleTheorems :: FilePath -> String -> [Theorem]
isabelleTheorems file = find . lexer 1
    where
        find ((i,"lemma"):(_,'\"':lemma):rest) = Theorem Nothing (file ++ ":" ++ show i) lemma : find rest
        find ((i,"lemma"):(_,name):(_,":"):(_,'\"':lemma):rest) = Theorem Nothing (file ++ ":" ++ show i) lemma : find rest
        find ((i,"lemma"):(_,"assumes"):(_,'\"':assumes):(_,"shows"):(_,'\"':lemma):rest) =
            Theorem Nothing (file ++ ":" ++ show i) (assumes ++ " \\<Longrightarrow> " ++ lemma) : find rest

        find ((i,"lemma"):rest) = Theorem Nothing (file ++ ":" ++ show i) "Unsupported lemma format" : find rest
        find (x:xs) = find xs
        find [] = []

        lexer i x
            | i `seq` False = []
            | Just x <- stripPrefix "(*" x, (a,b) <- breaks "*)" x = lexer (add a i) b
            | Just x <- stripPrefix "\"" x, (a,b) <- breaks "\"" x = (i,'\"':a) : lexer (add a i) b -- NOTE: drop the final "
            | x:xs <- x, isSpace x = lexer (add [x] i) xs
            | (a@(_:_),b) <- span (\y -> y == '_' || isAlpha y) x = (i,a) : lexer (add a i) b
        lexer i (x:xs) = (i,[x]) : lexer (add [x] i) xs
        lexer i [] = []

        add s i = length (filter (== '\n') s) + i

        breaks s x | Just x <- stripPrefix s x = ("",x)
        breaks s (x:xs) = let (a,b) = breaks s xs in (x:a,b)
        breaks s [] = ([],[])


reparen :: Setting -> Setting
reparen m@MatchExp{..} = m{lhs = f False lhs, rhs = f True rhs}
    where f right x = if isLambda x || isIf x || badInfix x then Paren (ann x) x else x
          badInfix (InfixApp _ _ op _) = prettyPrint op `elem` words "|| && ."
          badInfix _ = False
reparen x = x


-- Extract theorems out of the hints
hintTheorems :: [Setting] -> [Theorem]
hintTheorems xs =
    [ Theorem (Just m) (loc $ ann lhs) $ relationship notes (exp $ typeclasses notes lhs) (exp rhs)
    | m@MatchExp{..} <- map reparen xs]
    where
        loc (SrcSpanInfo (SrcSpan file ln _ _ _) _) = takeFileName file ++ ":" ++ show ln

        subs xs = flip lookup [(reverse b, reverse a) | x <- words xs, let (a,'=':b) = break (== '=') $ reverse x]
        funs = subs "id=ID not=neg or=the_or and=the_and (||)=tror (&&)=trand (++)=append"
        ops = subs "||=orelse &&=andalso .=oo ===eq /==neq"
        pre = flip elem $ words "eq neq"
        cons = subs "True=TT False=FF"

        typeclasses notes x = foldr f x notes
            where
                f (ValidInstance cls var) x = evalState (transformM g x) True
                    where g v@Var{} | v ~= var = do
                                b <- get; put False
                                return $ if b then Paren an $ toNamed $ prettyPrint v ++ "::'a::" ++ cls ++ "_sym" else v
                          g v = return v :: State Bool Exp_
                f _  x = x

        relationship notes a b | any lazier notes = a ++ " \\<sqsubseteq> " ++ b
                               | DecreasesLaziness `elem` notes = b ++ " \\<sqsubseteq> " ++ b
                               | otherwise = a ++ " = " ++ b
            where lazier IncreasesLaziness = True
                  lazier RemovesError{} = True
                  lazier _ = False

        exp (App _ a b) = exp a ++ "\\<cdot>" ++ exp b
        exp (Paren _ x) = "(" ++ exp x ++ ")"
        exp (Var _ x) | Just x <- funs $ prettyPrint x = x
        exp (Con _ (Special _ (TupleCon _ _ i))) = "\\<langle>" ++ replicate (i-1) ',' ++ "\\<rangle>"
        exp (Con _ x) | Just x <- cons $ prettyPrint x = x
        exp (Tuple _ xs) = "\\<langle>" ++ intercalate ", " (map exp xs) ++ "\\<rangle>"
        exp (If _ a b c) = "If " ++ exp a ++ " then " ++ exp b ++ " else " ++ exp c
        exp (Lambda _ xs y) = "\\<Lambda> " ++ unwords (map pat xs) ++ ". " ++ exp y
        exp (InfixApp l a (QVarOp _ (UnQual _ (Ident _ b))) c) = exp $ App l (App l (Var l (UnQual l (Ident l b))) a) c
        exp (InfixApp _ x op y) | Just op <- ops $ prettyPrint op =
            if pre op then op ++ "\\<cdot>" ++ exp (paren x) ++ "\\<cdot>" ++ exp (paren y) else exp x ++ " " ++ op ++ " " ++ exp y
        exp x = prettyPrint x

        pat (PTuple _ xs) = "\\<langle>" ++ intercalate ", " (map pat xs) ++ "\\<rangle>"
        pat x = prettyPrint x

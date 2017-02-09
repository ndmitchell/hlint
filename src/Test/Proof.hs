{-# LANGUAGE RecordWildCards, PatternGuards, FlexibleContexts #-}

-- | Check the coverage of the hints given a list of Isabelle theorems
module Test.Proof(proof) where

import Data.Tuple.Extra
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import System.FilePath
import Config.Type
import HSE.All
import Prelude


data Theorem = Theorem
    {original :: Maybe HintRule
    ,location :: String
    ,lemma :: String
    }

instance Eq Theorem where
    t1 == t2 = lemma t1 == lemma t2

instance Show Theorem where
    show Theorem{..} = location ++ ":\n" ++ maybe "" f original ++ lemma ++ "\n"
        where f HintRule{..} = "(* " ++ prettyPrint hintRuleLHS ++ " ==> " ++ prettyPrint hintRuleRHS ++ " *)\n"

proof :: [FilePath] -> [Setting] -> FilePath -> IO ()
proof reports hints thy = do
    got <- isabelleTheorems (takeFileName thy) <$> readFile thy
    let want = nub $ hintTheorems hints
    let unused = got \\ want
    let missing = want \\ got
    let reasons = map (\x -> (fst $ head x, map snd x)) $ groupBy ((==) `on` fst) $
                  sortBy (compare `on` fst) $ map (classifyMissing &&& id) missing
    let summary = table $ let (*) = (,) in
            ["HLint hints" * want
            ,"HOL proofs" * got
            ,"Useful proofs" * (got `intersect` want)
            ,"Unused proofs" * unused
            ,"Unproved hints" * missing] ++
            [("  " ++ name) * ps | (name,ps) <- reasons]
    putStr $ unlines summary
    forM_ reports $ \report -> do
        let out = ("Unused proofs",unused) : map (first ("Unproved hints - " ++)) reasons
        writeFile report $ unlines $ summary ++ "" : concat
            [("== " ++ a ++ " ==") : "" : map show b | (a,b) <- out]
        putStrLn $ "Report written to " ++ report
    where
        table xs = [a ++ replicate (n + 6 - length a - length bb) ' ' ++ bb | (a,b) <- xs, let bb = show $ length b]
            where n = maximum $ map (length . fst) xs


missingFuncs = let a*b = [(b,a) | b <- words b] in concat
    ["IO" * "putChar putStr print putStrLn getLine getChar getContents hReady hPrint stdin"
    ,"Exit" * "exitSuccess"
    ,"Ord" * "(>) (<=) (>=) (<) compare minimum maximum sort sortBy"
    ,"Show" * "show shows showIntAtBase"
    ,"Read" * "reads read"
    ,"String" * "lines unlines words unwords"
    ,"Monad" * "mapM mapM_ sequence sequence_ msum mplus mzero liftM when unless return evaluate join void (>>=) (<=<) (>=>) forever ap"
    ,"Functor" * "fmap"
    ,"Numeric" * "(+) (*) fromInteger fromIntegral negate log (/) (-) (*) (^^) (^) subtract sqrt even odd"
    ,"Char" * "isControl isPrint isUpper isLower isAlpha isDigit"
    ,"Arrow" * "second first (***) (&&&)"
    ,"Applicative+" * "traverse for traverse_ for_ pure (<|>) (<**>)"
    ,"Exception" * "catch handle catchJust bracket error toException"
    ,"WeakPtr" * "mkWeak"
    ]


-- | Guess why a theorem is missing
classifyMissing :: Theorem -> String
classifyMissing Theorem{original = Just HintRule{..}}
    | _:_ <- [v :: Exp_ | v@Case{} <- universeBi (hintRuleLHS,hintRuleRHS)] = "case"
    | _:_ <- [v :: Exp_ | v@ListComp{} <- universeBi (hintRuleLHS,hintRuleRHS)] = "list-comp"
    | v:_ <- mapMaybe (`lookup` missingFuncs) [prettyPrint (v :: Name S) | v <- universeBi (hintRuleLHS,hintRuleRHS)] = v
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
reparen (SettingMatchExp m@HintRule{..}) = SettingMatchExp m{hintRuleLHS = f False hintRuleLHS, hintRuleRHS = f True hintRuleRHS}
    where f right x = if isLambda x || isIf x || badInfix x then Paren (ann x) x else x
          badInfix (InfixApp _ _ op _) = prettyPrint op `elem` words "|| && ."
          badInfix _ = False
reparen x = x


-- Extract theorems out of the hints
hintTheorems :: [Setting] -> [Theorem]
hintTheorems xs =
    [ Theorem (Just m) (loc $ ann hintRuleLHS) $ maybe "" assumes hintRuleSide ++ relationship hintRuleNotes a b
    | SettingMatchExp m@HintRule{..} <- map reparen xs, let a = exp1 $ typeclasses hintRuleNotes hintRuleLHS, let b = exp1 hintRuleRHS, a /= b]
    where
        loc (SrcSpanInfo (SrcSpan file ln _ _ _) _) = takeFileName file ++ ":" ++ show ln

        subs xs = flip lookup [(reverse b, reverse a) | x <- words xs, let (a,'=':b) = break (== '=') $ reverse x]
        funs = subs "id=ID not=neg or=the_or and=the_and (||)=tror (&&)=trand (++)=append (==)=eq (/=)=neq ($)=dollar"
        ops = subs "||=orelse &&=andalso .=oo ===eq /==neq ++=++ !!=!! $=dollar $!=dollarBang"
        pre = flip elem $ words "eq neq dollar dollarBang"
        cons = subs "True=TT False=FF"

        typeclasses hintRuleNotes x = foldr f x hintRuleNotes
            where
                f (ValidInstance cls var) x = evalState (transformM g x) True
                    where g v@Var{} | v ~= var = do
                                b <- get; put False
                                return $ if b then Paren an $ toNamed $ prettyPrint v ++ "::'a::" ++ cls ++ "_sym" else v
                          g v = return v :: State Bool Exp_
                f _  x = x

        relationship hintRuleNotes a b | any lazier hintRuleNotes = a ++ " \\<sqsubseteq> " ++ b
                               | DecreasesLaziness `elem` hintRuleNotes = b ++ " \\<sqsubseteq> " ++ a
                               | otherwise = a ++ " = " ++ b
            where lazier IncreasesLaziness = True
                  lazier RemovesError{} = True
                  lazier _ = False

        assumes (App _ op var)
            | op ~= "isNat" = "le\\<cdot>0\\<cdot>" ++ prettyPrint var ++ " \\<noteq> FF \\<Longrightarrow> "
            | op ~= "isNegZero" = "gt\\<cdot>0\\<cdot>" ++ prettyPrint var ++ " \\<noteq> FF \\<Longrightarrow> "
        assumes (App _ op var) | op ~= "isWHNF" = prettyPrint var ++ " \\<noteq> \\<bottom> \\<Longrightarrow> "
        assumes _ = ""

        exp1 = exp . transformBi unqual

        -- Syntax translations
        exp (App _ a b) = exp a ++ "\\<cdot>" ++ exp b
        exp (Paren _ x) = "(" ++ exp x ++ ")"
        exp (Var _ x) | Just x <- funs $ prettyPrint x = x
        exp (Con _ (Special _ (TupleCon _ _ i))) = "\\<langle>" ++ replicate (i-1) ',' ++ "\\<rangle>"
        exp (Con _ x) | Just x <- cons $ prettyPrint x = x
        exp (Tuple _ _ xs) = "\\<langle>" ++ intercalate ", " (map exp xs) ++ "\\<rangle>"
        exp (If _ a b c) = "If " ++ exp a ++ " then " ++ exp b ++ " else " ++ exp c
        exp (Lambda _ xs y) = "\\<Lambda> " ++ unwords (map pat xs) ++ ". " ++ exp y
        exp (InfixApp _ x op y) | Just op <- ops $ prettyPrint op =
            if pre op then op ++ "\\<cdot>" ++ exp (paren x) ++ "\\<cdot>" ++ exp (paren y) else exp x ++ " " ++ op ++ " " ++ exp y

        -- Translations from the Haskell 2010 report
        exp (InfixApp l a (QVarOp _ b) c) = exp $ App l (App l (Var l b) a) c -- S3.4
        exp x@(LeftSection l e op) = let v = fresh x in exp $ Paren l $ Lambda l [toNamed v] $ InfixApp l e op (toNamed v) -- S3.5
        exp x@(RightSection l op e) = let v = fresh x in exp $ Paren l $ Lambda l [toNamed v] $ InfixApp l (toNamed v) op e -- S3.5
        exp x = prettyPrint x

        pat (PTuple _ _ xs) = "\\<langle>" ++ intercalate ", " (map pat xs) ++ "\\<rangle>"
        pat x = prettyPrint x

        fresh x = head $ ("z":["v" ++ show i | i <- [1..]]) \\ vars x

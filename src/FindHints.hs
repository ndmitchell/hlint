{-# LANGUAGE PatternGuards, ViewPatterns #-}

module FindHints(findHints) where

import Control.Monad
import HSE.All


-- find definitions in a source file, and write them to std out
findHints :: ParseFlags -> FilePath -> IO ()
findHints flags file = do
    x <- parseFile_ flags file
    let xs = concatMap (findHint $ UnQual an) $ moduleDecls x
    putStrLn $ "-- hints found in " ++ file
    when (null xs) $ putStrLn "-- no hints found"
    putStrLn $ unlines xs


findHint :: (Name S -> QName S) -> Decl_ -> [String]
findHint qual (InstDecl _ _ _ (Just xs)) = concatMap (findHint qual) [x | InsDecl _ x <- xs]
findHint qual (PatBind _ (PVar _ name) Nothing (UnGuardedRhs _ bod) Nothing) = findExp (qual name) [] bod
findHint qual (FunBind _ [InfixMatch _ p1 name ps rhs bind]) = findHint qual $ FunBind an [Match an name (p1:ps) rhs bind]
findHint qual (FunBind _ [Match _ name ps (UnGuardedRhs _ bod) Nothing]) = findExp (qual name) [] $ Lambda an ps bod
findHint _ _ = []


-- given a result function name, a list of variables, a body expression, give some hints
findExp :: QName S -> [String] -> Exp_ -> [String]
findExp name vs (Lambda _ ps bod) | length ps2 == length ps = findExp name (vs++ps2) bod
                                  | otherwise = []
    where ps2 = [x | PVar_ x <- map view ps]
findExp name vs Var{} = []
findExp name vs (InfixApp _ x dot y) | isDot dot = findExp name (vs++["_hlint"]) $ App an x $ Paren an $ App an y (toNamed "_hlint")

findExp name vs bod = ["warn = " ++ prettyPrint lhs ++ " ==> " ++ prettyPrint rhs]
    where
        lhs = hintParen $ transform f bod
        rhs = apps $ Var an name : map snd rep

        rep = zip vs $ map (toNamed . return) ['a'..]
        f (view -> Var_ x) | Just y <- lookup x rep = y
        f (InfixApp _ x dol y) | isDol dol = App an x (paren y)
        f x = x


hintParen o@(InfixApp _ _ _ x) | isAnyApp x || isAtom x = o
hintParen o@App{} = o
hintParen o = paren o

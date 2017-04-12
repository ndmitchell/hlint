{-# LANGUAGE PatternGuards, ScopedTypeVariables, RecordWildCards #-}
{-
    Reduce the number of import declarations.
    Two import declarations can be combined if:
      (note, A[] is A with whatever import list, or none)

    import A[]; import A[] = import A[]
    import A(B); import A(C) = import A(B,C)
    import A; import A(C) = import A
    import A; import A hiding (C) = import A
    import A[]; import A[] as Y = import A[] as Y

<TEST>
import A; import A -- import A
import A; import A; import A -- import A
import A(Foo) ; import A -- import A
import A ;import A(Foo) -- import A
import A(Bar(..)); import {-# SOURCE #-} A
import A; import B
import A(B) ; import A(C) -- import A(B,C)
import A; import A hiding (C) -- import A
import A; import A as Y -- import A as Y
import A; import qualified A as Y
import A as B; import A as C
import A as A -- import A
import qualified A as A -- import qualified A
import A; import B; import A -- import A
import qualified A; import A
import B; import A; import A -- import A
import A hiding(Foo); import A hiding(Bar)
import List -- import Data.List
import qualified List -- import qualified Data.List as List
import Char(foo) -- import Data.Char(foo)
import IO(foo)
import IO as X -- import System.IO as X; import System.IO.Error as X; import Control.Exception  as X (bracket,bracket_)
</TEST>
-}


module Hint.Import(importHint) where

import Control.Applicative
import Data.Tuple.Extra
import Hint.Type
import Refact.Types hiding (ModuleName)
import qualified Refact.Types as R
import Data.List.Extra
import Data.Maybe
import Prelude


importHint :: ModuHint
importHint _ x = concatMap (wrap . snd) (groupSort
                 [((fromNamed $ importModule i,importPkg i),i) | i <- universeBi x, not $ importSrc i]) ++
                 concatMap (\x -> hierarchy x ++ combine1 x) (universeBi x)


wrap :: [ImportDecl S] -> [Idea]
wrap o = [ rawIdea Warning "Use fewer imports" (srcInfoSpan $ ann $ head o) (f o) (Just $ f x) [] rs
         | Just (x, rs) <- [simplify o]]
    where f = unlines . map prettyPrint


simplify :: [ImportDecl S] -> Maybe ([ImportDecl S], [Refactoring R.SrcSpan])
simplify [] = Nothing
simplify (x:xs) = case simplifyHead x xs of
    Nothing -> first (x:) <$> simplify xs
    Just (xs, rs) -> Just $ fromMaybe (xs, rs) (second (++ rs) <$> simplify xs)


simplifyHead :: ImportDecl S -> [ImportDecl S] -> Maybe ([ImportDecl S], [Refactoring R.SrcSpan])
simplifyHead x [] = Nothing
simplifyHead x (y:ys) = case combine x y of
    Nothing -> first (y:) <$> simplifyHead x ys
    Just (xy, rs) -> Just (xy : ys, rs)


combine :: ImportDecl S -> ImportDecl S -> Maybe (ImportDecl S, [Refactoring R.SrcSpan])
combine x y | qual, as, specs = Just (x, [Delete Import (toSS y)])
           | qual, as, Just (ImportSpecList _ False xs) <- importSpecs x, Just (ImportSpecList _ False ys) <- importSpecs y = let newImp = x{importSpecs = Just $ ImportSpecList an False $ nub_ $ xs ++ ys}
            in Just (newImp, [ Replace Import (toSS x)  [] (prettyPrint newImp)
                             , Delete Import (toSS y) ] )

           | qual, as, isNothing (importSpecs x) || isNothing (importSpecs y) =
             let (newImp, toDelete) = if isNothing (importSpecs x) then (x, y) else (y, x)
             in Just (newImp, [Delete Import (toSS toDelete)])
           | not (importQualified x), qual, specs, length ass == 1 =
             let (newImp, toDelete) = if isJust (importAs x) then (x, y) else (y, x)
             in Just (newImp, [Delete Import (toSS toDelete)])

    where
        qual = importQualified x == importQualified y
        as = importAs x `eqMaybe` importAs y
        ass = mapMaybe importAs [x,y]
        specs = importSpecs x `eqMaybe` importSpecs y

combine _ _ = Nothing


combine1 :: ImportDecl S -> [Idea]
combine1 i@ImportDecl{..}
    | Just (dropAnn importModule) == fmap dropAnn importAs
    = [suggest "Redundant as" i i{importAs=Nothing} [RemoveAsKeyword (toSS i)]]
combine1 _ = []


newNames = let (*) = flip (,) in
    ["Control" * "Monad"
    ,"Data" * "Char"
    ,"Data" * "List"
    ,"Data" * "Maybe"
    ,"Data" * "Ratio"
    ,"System" * "Directory"

    -- Special, see bug #393
    -- ,"System" * "IO"

    -- Do not encourage use of old-locale/old-time over haskell98
    -- ,"System" * "Locale"
    -- ,"System" * "Time"
    ]


hierarchy :: ImportDecl S -> [Idea]
hierarchy i@ImportDecl{importModule=m@(ModuleName _ x),importPkg=Nothing} | Just y <- lookup x newNames
    =
    let newModuleName = y ++ "." ++ x
        r = [Replace R.ModuleName (toSS m) [] newModuleName] in
    [suggest "Use hierarchical imports" i (desugarQual i){importModule=ModuleName an newModuleName} r]

-- import IO is equivalent to
-- import System.IO, import System.IO.Error, import Control.Exception(bracket, bracket_)
hierarchy i@ImportDecl{importModule=ModuleName _ "IO", importSpecs=Nothing,importPkg=Nothing}
    = [rawIdeaN Suggestion "Use hierarchical imports" (srcInfoSpan $ ann i) (trimStart $ prettyPrint i) (
          Just $ unlines $ map (trimStart . prettyPrint)
          [f "System.IO" Nothing, f "System.IO.Error" Nothing
          ,f "Control.Exception" $ Just $ ImportSpecList an False [IVar an $ toNamed x | x <- ["bracket","bracket_"]]]) []]
    where f a b = (desugarQual i){importModule=ModuleName an a, importSpecs=b}

hierarchy _ = []


-- import qualified X ==> import qualified X as X
desugarQual :: ImportDecl S -> ImportDecl S
desugarQual x | importQualified x && isNothing (importAs x) = x{importAs=Just (importModule x)}
              | otherwise = x

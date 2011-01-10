{-# LANGUAGE PatternGuards #-}
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
import A(Bar(..)); import {-# SOURCE #-} A
import A; import B
import A(B) ; import A(C) -- import A(B,C)
import A; import A hiding (C) -- import A
import A; import A as Y -- import A as Y
import A; import qualified A as Y
import A as B; import A as C
import A; import B; import A -- import A
import qualified A; import A
import B; import A; import A -- import A
import A hiding(Foo); import A hiding(Bar)
import List -- import Data.List
import Char(foo) -- import Data.Char(foo)
import IO(foo)
import IO as X -- import System.IO as X; import System.IO.Error as X; import Control.Exception  as X (bracket,bracket_)
</TEST>
-}


module Hint.Import where

import Hint.Type
import Util
import Data.Maybe


importHint :: ModuHint
importHint _ x = concatMap (wrap . snd) (groupSortFst
                 [((fromNamed $ importModule i,importPkg i),i) | i <- universeBi x, not $ importSrc i]) ++
                 concatMap hierarchy (universeBi x)


wrap :: [ImportDecl S] -> [Idea]
wrap o = [ rawIdea Error "Use fewer imports" (toSrcLoc $ ann $ head o) (f o) (f x)
         | Just x <- [simplify o]]
    where f = unlines . map prettyPrint


simplify :: [ImportDecl S] -> Maybe [ImportDecl S]
simplify [] = Nothing
simplify (x:xs) = case simplifyHead x xs of
    Nothing -> fmap (x:) $ simplify xs
    Just xs -> Just $ fromMaybe xs $ simplify xs


simplifyHead :: ImportDecl S -> [ImportDecl S] -> Maybe [ImportDecl S]
simplifyHead x [] = Nothing
simplifyHead x (y:ys) = case reduce x y of
    Nothing -> fmap (y:) $ simplifyHead x ys
    Just xy -> Just $ xy : ys


-- Useful fields in import are:
-- importModule :: ModuleName [same]
-- importPkg :: Maybe String [same]
-- importQualified :: Bool
-- importSrc :: Bool [False]
-- importAs :: Maybe ModuleName
-- importSpecs :: Maybe (Bool, [ImportSpec])

reduce :: ImportDecl S -> ImportDecl S -> Maybe (ImportDecl S)
reduce x y | qual, as, specs = Just x
           | qual, as, Just (ImportSpecList _ False xs) <- importSpecs x, Just (ImportSpecList _ False ys) <- importSpecs y =
                Just x{importSpecs = Just $ ImportSpecList an False $ nub_ $ xs ++ ys}
           | qual, as, isNothing (importSpecs x) || isNothing (importSpecs y) = Just x{importSpecs=Nothing}
           | not (importQualified x), qual, specs, length ass == 1 = Just x{importAs=Just $ head ass}
    where
        qual = importQualified x == importQualified y
        as = importAs x `eqMaybe` importAs y
        ass = mapMaybe importAs [x,y]
        specs = importSpecs x `eqMaybe` importSpecs y

reduce _ _ = Nothing



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
hierarchy i@ImportDecl{importModule=ModuleName _ x,importPkg=Nothing} | Just y <- lookup x newNames
    = [warn "Use hierarchical imports" i i{importModule=ModuleName an $ y ++ "." ++ x}]

-- import IO is equivalent to
-- import System.IO, import System.IO.Error, import Control.Exception(bracket, bracket_)
hierarchy i@ImportDecl{importModule=ModuleName _ "IO", importSpecs=Nothing}
    = [rawIdea Warning "Use hierarchical imports" (toSrcLoc $ ann i) (ltrim $ prettyPrint i) $
          unlines $ map (ltrim . prettyPrint)
          [f "System.IO" Nothing, f "System.IO.Error" Nothing
          ,f "Control.Exception" $ Just $ ImportSpecList an False [IVar an $ toNamed x | x <- ["bracket","bracket_"]]]]
    where f a b = i{importModule=ModuleName an a, importSpecs=b}

hierarchy _ = []

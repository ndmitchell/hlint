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
module Foo(module A, baz, module B, module C) where; import A; import D; import B(map,filter); import C \
    -- module Foo(baz, module X) where; import A as X; import B as X(map, filter); import C as X
module Foo(module A, baz, module B, module X) where; import A; import B; import X \
    -- module Foo(baz, module Y) where; import A as Y; import B as Y; import X as Y
</TEST>
-}


module Hint.Import where

import Hint.Type
import Util
import Data.List
import Data.Maybe


importHint :: ModuHint
importHint _ x = concatMap (wrap . snd) (groupSortFst
                 [((fromNamed $ importModule i,importPkg i),i) | i <- universeBi x, not $ importSrc i]) ++
                 concatMap (\x -> hierarchy x ++ reduce1 x) (universeBi x) ++
                 multiExport x


wrap :: [ImportDecl S] -> [Idea]
wrap o = [ rawIdea Error "Use fewer imports" (toSrcLoc $ ann $ head o) (f o) (f x) []
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


reduce1 :: ImportDecl S -> [Idea]
reduce1 i@ImportDecl{..}
    | Just (dropAnn importModule) == fmap dropAnn importAs
    = [warn "Redundant as" i i{importAs=Nothing}]
reduce1 _ = []


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
    = [warn "Use hierarchical imports" i (desugarQual i){importModule=ModuleName an $ y ++ "." ++ x}]

-- import IO is equivalent to
-- import System.IO, import System.IO.Error, import Control.Exception(bracket, bracket_)
hierarchy i@ImportDecl{importModule=ModuleName _ "IO", importSpecs=Nothing,importPkg=Nothing}
    = [rawIdea Warning "Use hierarchical imports" (toSrcLoc $ ann i) (ltrim $ prettyPrint i) (
          unlines $ map (ltrim . prettyPrint)
          [f "System.IO" Nothing, f "System.IO.Error" Nothing
          ,f "Control.Exception" $ Just $ ImportSpecList an False [IVar an $ toNamed x | x <- ["bracket","bracket_"]]]) []]
    where f a b = (desugarQual i){importModule=ModuleName an a, importSpecs=b}

hierarchy _ = []


-- import qualified X ==> import qualified X as X
desugarQual :: ImportDecl S -> ImportDecl S
desugarQual x | importQualified x && isNothing (importAs x) = x{importAs=Just (importModule x)}
              | otherwise = x


multiExport :: Module S -> [Idea]
multiExport x =
    [ rawIdea Warning "Use import/export shortcut" (toSrcLoc $ ann hd)
        (unlines $ prettyPrint hd : map prettyPrint imps)
        (unlines $ prettyPrint newhd : map prettyPrint newimps)
        []
    | Module l (Just hd) _ imp _ <- [x]
    , let asNames = mapMaybe importAs imp
    , let expNames = [x | EModuleContents _ x <- childrenBi hd]
    , let imps = [i | i@ImportDecl{importAs=Nothing,importQualified=False,importModule=name} <- imp
                 ,name `notElem_` asNames, name `elem_` expNames]
    , length imps >= 3
    , let newname = ModuleName an $ head $ map return ("XYZ" ++ ['A'..]) \\
                                           [x | ModuleName (_ :: S) x <- universeBi hd ++ universeBi imp]
    , let reexport (EModuleContents _ x) = x `notElem_` map importModule imps
          reexport x = True
    , let newhd = descendBi (\xs -> filter reexport xs ++ [EModuleContents an newname]) hd
    , let newimps = [i{importAs=Just newname} | i <- imps]
    ]

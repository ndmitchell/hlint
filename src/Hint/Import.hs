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
import A hiding (a) -- import A
import A hiding (a, b); foo = a -- import A hiding (a)
import A hiding (a, b); foo = A.a -- import A hiding (a)
import A as B hiding (a) -- import A as B
import A as B hiding (a, b); foo = a -- import A as B hiding (a)
import A as B hiding (a, b); foo = B.a -- import A as B hiding (a)
import qualified A hiding (a) -- import qualified A
import qualified A hiding (a, b); foo = A.a -- import qualified A hiding (a)
import qualified A as B hiding (a, b); foo = B.a -- import qualified A as B hiding (a)
import A hiding ((+)) -- import A
import A hiding ((+), (*)); foo = (+) -- import A hiding ((+))
import A hiding ((+), (*)); foo = (+x) -- import A hiding ((+))
import A hiding ((+), (*)); foo = (x+) -- import A hiding ((+))
import A hiding ((+), (*)); foo = x+y -- import A hiding ((+))
import A hiding ((+), (*)); foo = (A.+) -- import A hiding ((+))
import A hiding ((+), (*)); foo = (A.+ x) -- import A hiding ((+))
import A hiding ((+), (*)); foo = (x A.+) -- import A hiding ((+))
import A hiding ((+), (*)); foo = x A.+ y -- import A hiding ((+))
import A as B hiding ((+)) -- import A as B
import A as B hiding ((+), (*)); foo = (+) -- import A as B hiding ((+))
import A as B hiding ((+), (*)); foo = (x+) -- import A as B hiding ((+))
import A as B hiding ((+), (*)); foo = (+x) -- import A as B hiding ((+))
import A as B hiding ((+), (*)); foo = x+y -- import A as B hiding ((+))
import A as B hiding ((+), (*)); foo = (B.+) -- import A as B hiding ((+))
import A as B hiding ((+), (*)); foo = (x B.+) -- import A as B hiding ((+))
import A as B hiding ((+), (*)); foo = (B.+ x) -- import A as B hiding ((+))
import A as B hiding ((+), (*)); foo = x B.+ y -- import A as B hiding ((+))
import qualified A hiding ((+)) -- import qualified A
import qualified A hiding ((+), (*)); foo = (A.+) -- import qualified A hiding ((+))
import qualified A hiding ((+), (*)); foo = (x A.+) -- import qualified A hiding ((+))
import qualified A hiding ((+), (*)); foo = (A.+ x) -- import qualified A hiding ((+))
import qualified A hiding ((+), (*)); foo = x A.+ y -- import qualified A hiding ((+))
import qualified A as B hiding ((+), (*)); foo = (B.+) -- import qualified A as B hiding ((+))
import qualified A as B hiding ((+), (*)); foo = (x B.+) -- import qualified A as B hiding ((+))
import qualified A as B hiding ((+), (*)); foo = (B.+ x) -- import qualified A as B hiding ((+))
import qualified A as B hiding ((+), (*)); foo = x B.+ y -- import qualified A as B hiding ((+))
module Foo (a) where; import A hiding (a)
module Foo (a) where; import A hiding (a, b) -- import A hiding (a)
module Foo (A.a) where; import A hiding (a, b) -- import A hiding (a)
module Foo (a) where; import A as B hiding (a)
module Foo (a) where; import A as B hiding (a, b) -- import A as B hiding (a)
module Foo (B.a) where; import A as B hiding (a, b) -- import A as B hiding (a)
module Foo (a) where; import qualified A hiding (a) -- import qualified A
module Foo (A.a) where; import qualified A hiding (a, b) -- import qualified A hiding (a)
module Foo (B.a) where; import qualified A as B hiding (a, b) -- import qualified A as B hiding (a)
module Foo (module A) where; import A hiding (a, b, c)
module Foo (module B) where; import A as B hiding (a, b, c)
module Foo (module A) where; import qualified A hiding (a, b, c) -- import qualified A
module Foo (module B) where; import qualified A as B hiding (a, b, c) -- import qualified A as B
module Foo ((+)) where; import A hiding ((+))
module Foo ((+)) where; import A hiding ((+), (*)) -- import A hiding ((+))
module Foo ((A.+)) where; import A hiding ((+), (*)) -- import A hiding ((+))
module Foo ((+)) where; import A as B hiding ((+))
module Foo ((+)) where; import A as B hiding ((+), (*)) -- import A as B hiding ((+))
module Foo ((B.+)) where; import A as B hiding ((+), (*)) -- import A as B hiding ((+))
module Foo ((+)) where; import qualified A hiding ((+)) -- import qualified A
module Foo ((A.+)) where; import qualified A hiding ((+), (*)) -- import qualified A hiding ((+))
module Foo ((B.+)) where; import qualified A as B hiding ((+), (*)) -- import qualified A as B hiding ((+))
module Foo (module A) where; import A hiding ((+), (*), (/))
module Foo (module B) where; import A as B hiding ((+), (*), (/))
module Foo (module A) where; import qualified A hiding ((+), (*), (/)) -- import qualified A
module Foo (module B) where; import qualified A as B hiding ((+), (*), (/)) -- import qualified A as B
{-# LANGUAGE QuasiQuotes #-}; import A hiding (a); [a||]
{-# LANGUAGE QuasiQuotes #-}; import A hiding (a); [A.a||]
{-# LANGUAGE QuasiQuotes #-}; import A as B hiding (a); [B.a||]
{-# LANGUAGE QuasiQuotes #-}; import qualified A hiding (a); [A.a||]
{-# LANGUAGE QuasiQuotes #-}; import qualified A as B hiding (a); [B.a||]
</TEST>
-}


module Hint.Import(importHint) where

import Control.Applicative
import Data.Tuple.Extra
import Hint.Type
import Refact.Types hiding (ModuleName)
import qualified Refact.Types as R
import Data.List.Extra
import Data.Either (partitionEithers)
import Data.Maybe
import Prelude
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


importHint :: ModuHint
importHint _ x = concatMap (wrap . snd) (groupSort
                 [((fromNamed $ importModule i,importPkg i),i) | i <- universeBi x, not $ importSrc i]) ++
                 concatMap (\x -> hierarchy x ++ combine1 x ++ hidden reexported unqual quals x) (universeBi x)
    where
        -- Names of all re-exported modules
        reexported :: Set String
        reexported = Set.fromList [ fromModuleName n | EModuleContents _ n <- universeBi x ]

        -- Unqualified expressions and exported expressions
        unqual :: Set String
        unqual = Set.fromList (mapMaybe f qnames) `Set.union` Set.fromList qqUnqual
            where f :: QName S -> Maybe String
                  f (UnQual _ n) = Just (fromNamed n)
                  f _ = Nothing

        -- Qualified expressions and exported expressions
        quals :: Map String (Set String)
        quals = Map.fromListWith Set.union (map (second Set.singleton) qqQuals ++ mapMaybe f qnames)
            where f (Qual _ m n) = Just (fromModuleName m, Set.singleton (fromNamed n))
                  f _ = Nothing

        -- Unqualified quasi-quoters like [foo|...|]
        qqUnqual :: [String]
        -- Qualified quasi-quoters like [Foo.bar|...|]
        qqQuals :: [(String, String)]
        (qqUnqual, qqQuals) = partitionEithers [ f n | QuasiQuote (_ :: S) n _ <- universeBi x ]
            where f :: String -> Either String (String, String)
                  f n = maybe (Left n) Right (stripInfixEnd "." n)

        qnames :: [QName S]
        qnames = concat
            [ [ n | Var      (_ :: S) n <- universeBi x ]
            , [ n | VarQuote (_ :: S) n <- universeBi x ]
            , [ n | QVarOp   (_ :: S) n <- universeBi x ]
            , [ n | EVar     (_ :: S) n <- universeBi x ]
            ]

wrap :: [ImportDecl S] -> [Idea]
wrap o = [ rawIdea Warning "Use fewer imports" (srcInfoSpan $ ann $ head o) (f o) (Just $ f x) [] rs
         | Just (x, rs) <- [simplify o]]
    where f = unlines . map prettyPrint


simplify :: [ImportDecl S] -> Maybe ([ImportDecl S], [Refactoring R.SrcSpan])
simplify [] = Nothing
simplify (x:xs) = case simplifyHead x xs of
    Nothing -> first (x:) <$> simplify xs
    Just (xs, rs) -> Just $ maybe (xs, rs) (second (++ rs)) $ simplify xs


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

-- Suggest removing unnecessary "hiding" clauses in imports. Currently this only
-- works for expressions.
hidden :: Set String -> Set String -> Map String (Set String) -> ImportDecl S -> [Idea]
hidden reexported unqual quals i@ImportDecl{importSpecs = Just (ImportSpecList loc True xs)}
    -- If the module is re-exported and not imported qualified, we can't prune
    -- any identifiers from the hiding clause
    | not (importQualified i) && as `Set.member` reexported = []
    | otherwise =
        case partition isUsed xs of
            (_, []) -> []
            ([], _) -> [suggest "Unnecessary hiding" i i{importSpecs = Nothing} [Delete Import (toSS i)]]
            (xs, _) ->
                let newImp = i{importSpecs = Just (ImportSpecList loc True xs)}
                in [suggest "Unnecessary hiding" i newImp [Replace Import (toSS i) [] (prettyPrint newImp)]]
    where
        isUsed :: ImportSpec S -> Bool
        isUsed (IVar _ n) = Set.member (fromNamed n) vars
        isUsed _ = True

        vars :: Set String
        vars =
          if importQualified i
            then qual
            else qual `Set.union` unqual

        qual :: Set String
        qual = fromMaybe Set.empty (Map.lookup as quals)

        as :: String
        as = fromModuleName (fromMaybe (importModule i) (importAs i))

hidden _ _ _ _ = []

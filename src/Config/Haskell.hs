{-# LANGUAGE PatternGuards, ViewPatterns, TupleSections #-}
module Config.Haskell(
    readPragma,
    readComment
    ) where

import Data.Char
import Data.List.Extra
import Text.Read.Extra(readMaybe)
import Data.Tuple.Extra
import Data.Maybe
import Config.Type
import Util
import Prelude

import GHC.Util

import SrcLoc as GHC
import HsExtension
import HsDecls hiding (SpliceDecl)
import HsExpr hiding (Match)
import FastString
import HsLit
import ApiAnnotation
import OccName
import Outputable


-- | Read an {-# ANN #-} pragma and determine if it is intended for HLint.
--   Return Nothing if it is not an HLint pragma, otherwise what it means.
readPragma :: AnnDecl GhcPs -> Maybe Classify
readPragma (HsAnnotation _ _ provenance expr) = f expr
    where
        name = case provenance of
            ValueAnnProvenance (L _ x) -> occNameString $ occName x
            TypeAnnProvenance (L _ x) -> occNameString $ occName x
            ModuleAnnProvenance -> ""

        f (L _ (HsLit _ (HsString _ (unpackFS -> s)))) | "hlint:" `isPrefixOf` lower s =
                case getSeverity a of
                    Nothing -> errorOn expr "bad classify pragma"
                    Just severity -> Just $ Classify severity (trimStart b) "" name
            where (a,b) = break isSpace $ trimStart $ drop 6 s
        f (L _ (HsPar _ x)) = f x
        f (L _ (ExprWithTySig _ x _)) = f x
        f _ = Nothing
readPragma _ = Nothing

readComment :: GHC.Located AnnotationComment -> [Classify]
readComment c@(L pos AnnBlockComment{})
    | (hash, x) <- maybe (False, x) (True,) $ stripPrefix "#" x
    , x <- trim x
    , (hlint, x) <- word1 x
    , lower hlint == "hlint"
    = f hash x
    where
        x = commentText c
        f hash x
            | Just x <- if hash then stripSuffix "#" x else Just x
            , (sev, x) <- word1 x
            , Just sev <- getSeverity sev
            , (things, x) <- g x
            , Just hint <- if x == "" then Just "" else readMaybe x
            = map (Classify sev hint "") $ ["" | null things] ++ things
        f hash _ = errorOnComment c $ "bad HLINT pragma, expected:\n    {-" ++ h ++ " HLINT <severity> <identifier> \"Hint name\" " ++ h ++ "-}"
            where h = ['#' | hash]

        g x | (s, x) <- word1 x
            , s /= ""
            , not $ "\"" `isPrefixOf` s
            = first ((if s == "module" then "" else s):) $ g x
        g x = ([], x)
readComment _ = []


errorOn :: Outputable a => Located a -> String -> b
errorOn (L pos val) msg = exitMessageImpure $
    showSrcSpan' pos ++
    ": Error while reading hint file, " ++ msg ++ "\n" ++
    unsafePrettyPrint val

errorOnComment :: GHC.Located AnnotationComment -> String -> b
errorOnComment c@(L s _) msg = exitMessageImpure $
    let isMultiline = isCommentMultiline c in
    showSrcSpan' s ++
    ": Error while reading hint file, " ++ msg ++ "\n" ++
    (if isMultiline then "{-" else "--") ++ commentText c ++ (if isMultiline then "-}" else "")

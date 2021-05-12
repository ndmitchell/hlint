{-# LANGUAGE PatternGuards, ViewPatterns, TupleSections #-}
module Config.Haskell(
    readPragma,
    readComment
    ) where

import Data.Char
import Data.List.Extra
import Text.Read
import Data.Tuple.Extra
import Data.Maybe
import Config.Type
import Util
import Prelude

import GHC.Util

import GHC.Types.SrcLoc
import GHC.Hs.Extension
import GHC.Hs.Decls hiding (SpliceDecl)
import GHC.Hs.Expr hiding (Match)
import GHC.Hs.Lit
import GHC.Data.FastString
import GHC.Parser.Annotation
import GHC.Utils.Outputable

import Language.Haskell.GhclibParserEx.GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader

-- | Read an {-# ANN #-} pragma and determine if it is intended for HLint.
--   Return Nothing if it is not an HLint pragma, otherwise what it means.
readPragma :: AnnDecl GhcPs -> Maybe Classify
readPragma (HsAnnotation _ _ provenance expr) = f expr
    where
        name = case provenance of
            ValueAnnProvenance (L _ x) -> occNameStr x
            TypeAnnProvenance (L _ x) -> occNameStr x
            ModuleAnnProvenance -> ""

        f :: LocatedA (HsExpr GhcPs) -> Maybe Classify
        f (L _ (HsLit _ (HsString _ (unpackFS -> s)))) | "hlint:" `isPrefixOf` lower s =
                case getSeverity a of
                    Nothing -> errorOn expr "bad classify pragma"
                    Just severity -> Just $ Classify severity (trimStart b) "" name
            where (a,b) = break isSpace $ trimStart $ drop 6 s
        f (L _ (HsPar _ x)) = f x
        f (L _ (ExprWithTySig _ x _)) = f x
        f _ = Nothing

readComment :: LEpaComment -> [Classify]
readComment c@(L pos (EpaComment EpaBlockComment{} _))
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


errorOn :: Outputable a => LocatedA a -> String -> b
errorOn (L pos val) msg = exitMessageImpure $
    showSrcSpan (locA pos) ++
    ": Error while reading hint file, " ++ msg ++ "\n" ++
    unsafePrettyPrint val

errorOnComment :: LEpaComment -> String -> b
errorOnComment c@(L s _) msg = exitMessageImpure $
    let isMultiline = isCommentMultiline c in
    showSrcSpan (RealSrcSpan (anchor s) Nothing) ++
    ": Error while reading hint file, " ++ msg ++ "\n" ++
    (if isMultiline then "{-" else "--") ++ commentText c ++ (if isMultiline then "-}" else "")

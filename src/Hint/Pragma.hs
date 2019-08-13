{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

{-
    Suggest better pragmas
    OPTIONS_GHC -cpp => LANGUAGE CPP
    OPTIONS_GHC -fglasgow-exts => LANGUAGE ... (in HSE)
    OPTIONS_GHC -XFoo => LANGUAGE Foo
    LANGUAGE A, A => LANGUAGE A
    -- do not do LANGUAGE A, LANGUAGE B to combine

<TEST>
{-# OPTIONS_GHC -cpp #-} -- {-# LANGUAGE CPP #-}
{-# OPTIONS     -cpp #-} -- {-# LANGUAGE CPP #-}
{-# OPTIONS_YHC -cpp #-}
{-# OPTIONS_GHC -XFoo #-} -- {-# LANGUAGE Foo #-}
{-# OPTIONS_GHC -fglasgow-exts #-} -- ???
{-# LANGUAGE RebindableSyntax, EmptyCase, DuplicateRecordFields, RebindableSyntax #-} -- {-# LANGUAGE RebindableSyntax, EmptyCase, DuplicateRecordFields #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -cpp -foo #-} -- {-# LANGUAGE CPP #-} {-# OPTIONS_GHC -foo #-}
{-# OPTIONS_GHC -cpp #-} \
{-# LANGUAGE CPP, Text #-} --
{-# LANGUAGE RebindableSyntax #-} \
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RebindableSyntax #-} \
{-# LANGUAGE EmptyCase, RebindableSyntax #-} -- {-# LANGUAGE RebindableSyntax, EmptyCase #-}
</TEST>
-}


module Hint.Pragma(pragmaHint) where

import Hint.Type
import Data.List.Extra
import Data.Maybe
import Refact.Types
import qualified Refact.Types as R

import "ghc-lib-parser" ApiAnnotation
import qualified "ghc-lib-parser" SrcLoc as GHC
import GHC.Util

pragmaHint :: ModuHint
pragmaHint _ modu =
  let ps = pragmas (ghcAnnotations modu)
      opts = flags ps
      lang = langExts ps in
    languageDupes lang ++ optToPragma opts lang

optToPragma :: [(Located AnnotationComment, [String])]
             -> [(Located AnnotationComment, [String])]
             -> [Idea]
optToPragma flags langExts =
  [pragmaIdea (OptionsToComment (map fst old) ys rs) | old /= []]
  where
      (old, new, ns, rs) =
        unzip4 [(old, new, ns, r)
               | old <- flags, Just (new, ns) <- [optToLanguage old ls]
               , let r = mkRefact old new ns]

      ls = concatMap snd langExts
      ns2 = nubOrd (concat ns) \\ ls

      ys = [mkLangExts GHC.noSrcSpan ns2 | ns2 /= []] ++ catMaybes new
      mkRefact :: (Located AnnotationComment, [String])
               -> Maybe (Located AnnotationComment)
               -> [String]
               -> Refactoring R.SrcSpan
      mkRefact old (maybe "" comment -> new) ns =
        let ns' = map (\n -> comment (mkLangExts GHC.noSrcSpan [n])) ns
        in ModifyComment (toSS' (fst old)) (intercalate "\n" (filter (not . null) (new : ns')))

data PragmaIdea = SingleComment (Located AnnotationComment) (Located AnnotationComment)
                 | MultiComment (Located AnnotationComment) (Located AnnotationComment) (Located AnnotationComment)
                 | OptionsToComment [Located AnnotationComment] [Located AnnotationComment] [Refactoring R.SrcSpan]

pragmaIdea :: PragmaIdea -> Idea
pragmaIdea pidea =
  case pidea of
    SingleComment old new ->
      mkFewer (getloc old) (comment old) (Just $ comment new) []
      [ModifyComment (toSS' old) (comment new)]
    MultiComment repl delete new ->
      mkFewer (getloc repl)
        (f [repl, delete]) (Just $ comment new) []
        [ ModifyComment (toSS' repl) (comment new)
        , ModifyComment (toSS' delete) ""]
    OptionsToComment old new r ->
      mkLanguage (getloc . head $ old)
        (f old) (Just $ f new) []
        r
    where
          f = unlines . map comment
          mkFewer = rawIdea' Hint.Type.Warning "Use fewer LANGUAGE pragmas"
          mkLanguage = rawIdea' Hint.Type.Warning "Use LANGUAGE pragmas"

languageDupes :: [(Located AnnotationComment, [String])] -> [Idea]
languageDupes ( (a@(GHC.L l _), les) : cs ) =
  (if nubOrd les /= les
       then [pragmaIdea (SingleComment a (mkLangExts l $ nubOrd les))]
       else [pragmaIdea (MultiComment a b (mkLangExts l (nubOrd $ les ++ les'))) | ( b@(GHC.L _ _), les' ) <- cs, not $ null $ intersect les les']
  ) ++ languageDupes cs
languageDupes _ = []

-- Given a pragma, can you extract some language features out?
strToLanguage :: String -> Maybe [String]
strToLanguage "-cpp" = Just ["CPP"]
strToLanguage x | "-X" `isPrefixOf` x = Just [drop 2 x]
strToLanguage "-fglasgow-exts" = Just $ map prettyExtension glasgowExts
strToLanguage _ = Nothing

-- In 'optToLanguage p langexts', 'p' is an 'OPTIONS_GHC' pragma,
-- 'langexts' a list of all language extensions in the module enabled
-- by 'LANGUAGE' pragmas.
--
--  If ALL of the flags in the pragma enable language extensions,
-- 'return Nothing'.
--
-- If some (or all) of the flags enable options that are not language
-- extensions, compute a new options pragma with only non-language
-- extension enabling flags. Return that together with a list of any
-- language extensions enabled by this pragma that are not otherwise
-- enabled by LANGUAGE pragmas in the module.
optToLanguage :: (Located AnnotationComment, [String])
               -> [String]
               -> Maybe (Maybe (Located AnnotationComment), [String])
optToLanguage (GHC.L loc _, flags) langExts
  | any isJust vs =
      -- 'ls' is a list of language features enabled by this
      -- OPTIONS_GHC pragma that are not enabled by LANGUAGE pragmas
      -- in this module.
      let ls = filter (not . (`elem` langExts)) (concat $ catMaybes vs) in
      Just (res, ls)
  where
    -- Try reinterpreting each flag as a list of language features
    -- (e.g. via '-X'..., '-fglasgow-exts').
    vs = map strToLanguage flags -- e.g. '[Nothing, Just ["ScopedTypeVariables"], Nothing, ...]'
    -- Keep any flag that does not enable language extensions.
    keep = concat $ zipWith (\v f -> [f | isNothing v]) vs flags
    -- If there are flags to keep, 'res' is a new pragma setting just those flags.
    res = if null keep then Nothing else Just (mkFlags loc keep)
optToLanguage _ _ = Nothing

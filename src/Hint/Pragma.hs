{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

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
{-# OPTIONS_GHC -fglasgow-exts #-} -- ??? @NoRefactor: refactor output has one LANGUAGE pragma per extension, while hlint suggestion has a single LANGUAGE pragma
{-# LANGUAGE RebindableSyntax, EmptyCase, RebindableSyntax #-} -- {-# LANGUAGE RebindableSyntax, EmptyCase #-}
{-# LANGUAGE RebindableSyntax, EmptyCase, DuplicateRecordFields, RebindableSyntax #-} -- {-# LANGUAGE RebindableSyntax, EmptyCase, DuplicateRecordFields #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -cpp -foo #-} -- {-# LANGUAGE CPP #-} {-# OPTIONS_GHC -foo #-} @NoRefactor -foo is not a valid flag
{-# OPTIONS_GHC -cpp -w #-} -- {-# LANGUAGE CPP #-} {-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -cpp #-} \
{-# LANGUAGE CPP, Text #-} --
{-# LANGUAGE RebindableSyntax #-} \
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RebindableSyntax #-} \
{-# LANGUAGE EmptyCase, RebindableSyntax #-} -- {-# LANGUAGE EmptyCase, RebindableSyntax #-}
</TEST>
-}


module Hint.Pragma(pragmaHint) where

import Hint.Type(ModuHint,Idea(..),Severity(..),toSSAnc,rawIdea,modComments)
import Data.List.Extra
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Refact.Types
import qualified Refact.Types as R

import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Data.FastString

import GHC.Util
import GHC.Driver.Session

pragmaHint :: ModuHint
pragmaHint _ modu =
  let ps = pragmas (modComments modu)
      opts = flags ps
      lang = languagePragmas ps in
    languageDupes lang ++ optToPragma opts lang

optToPragma :: [(LEpaComment, [String])]
             -> [(LEpaComment, [String])]
             -> [Idea]
optToPragma flags languagePragmas =
  [pragmaIdea (OptionsToComment (fst <$> old2) ys rs) | Just old2 <- [NE.nonEmpty old]]
  where
      (old, new, ns, rs) =
        unzip4 [(old, new, ns, r)
               | old <- flags, Just (new, ns) <- [optToLanguage old ls]
               , let r = mkRefact old new ns]

      ls = concatMap snd languagePragmas
      ns2 = nubOrd (concat ns) \\ ls

      dummyLoc = mkRealSrcLoc (fsLit "dummy") 1 1
      dummySpan = mkRealSrcSpan dummyLoc dummyLoc
      dummyAnchor = realSpanAsAnchor dummySpan

      ys = [mkLanguagePragmas dummyAnchor ns2 | ns2 /= []] ++ catMaybes new
      mkRefact :: (LEpaComment, [String])
               -> Maybe LEpaComment
               -> [String]
               -> Refactoring R.SrcSpan
      mkRefact old (maybe "" comment_ -> new) ns =
        let ns' = map (\n -> comment_ (mkLanguagePragmas dummyAnchor [n])) ns
        in ModifyComment (toSSAnc (fst old)) (intercalate "\n" (filter (not . null) (ns' `snoc` new)))

data PragmaIdea = SingleComment LEpaComment LEpaComment
                 | MultiComment LEpaComment LEpaComment LEpaComment
                 | OptionsToComment (NE.NonEmpty LEpaComment) [LEpaComment] [Refactoring R.SrcSpan]

pragmaIdea :: PragmaIdea -> Idea
pragmaIdea pidea =
  case pidea of
    SingleComment old new ->
      mkFewer (getAncLoc old) (comment_ old) (Just $ comment_ new) []
      [ModifyComment (toSSAnc old) (comment_ new)]
    MultiComment repl delete new ->
      mkFewer (getAncLoc repl)
        (f [repl, delete]) (Just $ comment_ new) []
        [ ModifyComment (toSSAnc repl) (comment_ new)
        , ModifyComment (toSSAnc delete) ""]
    OptionsToComment old new r ->
      mkLanguage (getAncLoc . NE.head $ old)
        (f $ NE.toList old) (Just $ f new) []
        r
    where
          f = unlines . map comment_
          mkFewer = rawIdea Hint.Type.Warning "Use fewer LANGUAGE pragmas"
          mkLanguage = rawIdea Hint.Type.Warning "Use LANGUAGE pragmas"

languageDupes :: [(LEpaComment, [String])] -> [Idea]
languageDupes ( (a@(L l _), les) : cs ) =
  (if nubOrd les /= les
       then [pragmaIdea (SingleComment a (mkLanguagePragmas l $ nubOrd les))]
       else [pragmaIdea (MultiComment a b (mkLanguagePragmas l (nubOrd $ les ++ les'))) | ( b@(L _ _), les' ) <- cs, not $ disjoint les les']
  ) ++ languageDupes cs
languageDupes _ = []

-- Given a pragma, can you extract some language features out?
strToLanguage :: String -> Maybe [String]
strToLanguage "-cpp" = Just ["CPP"]
strToLanguage x | "-X" `isPrefixOf` x = Just [drop 2 x]
strToLanguage "-fglasgow-exts" = Just $ map show glasgowExtsFlags
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
optToLanguage :: (LEpaComment, [String])
               -> [String]
               -> Maybe (Maybe LEpaComment, [String])
optToLanguage (L loc _, flags) languagePragmas
  | any isJust vs =
      -- 'ls' is a list of language features enabled by this
      -- OPTIONS_GHC pragma that are not enabled by LANGUAGE pragmas
      -- in this module.
      let ls = filter (not . (`elem` languagePragmas)) (concat $ catMaybes vs) in
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

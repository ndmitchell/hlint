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
{-# OPTIONS_GHC -fglasgow-exts #-} -- ???
{-# LANGUAGE A, B, C, A #-} -- {-# LANGUAGE A, B, C #-}
{-# LANGUAGE A #-}
{-# OPTIONS_GHC -cpp -foo #-} -- {-# LANGUAGE CPP #-} {-# OPTIONS_GHC -foo #-}
{-# OPTIONS_GHC -cpp #-} \
{-# LANGUAGE CPP, Text #-} --
{-# LANGUAGE A #-} \
{-# LANGUAGE B #-}
{-# LANGUAGE A #-} \
{-# LANGUAGE B, A #-} -- {-# LANGUAGE A, B #-}
</TEST>
-}


module Hint.Pragma(pragmaHint) where

import Hint.Type
import Data.List
import Data.Maybe
import Refact.Types
import qualified Refact.Types as R


pragmaHint :: ModuHint
pragmaHint _ x = languageDupes lang ++ optToPragma x lang
    where
        lang = [x | x@LanguagePragma{} <- modulePragmas x]

optToPragma :: Module_ -> [ModulePragma S] -> [Idea]
optToPragma x lang =
  [pragmaIdea (OptionsToComment old ys rs) | old /= []]
  where
        (old,new,ns, rs) =
          unzip4 [(old,new,ns, r)
                 | old <- modulePragmas x, Just (new,ns) <- [optToLanguage old ls]
                 , let r = mkRefact old new ns]

        ls = concat [map fromNamed n | LanguagePragma _ n <- lang]
        ns2 = nub (concat ns) \\ ls

        ys = [LanguagePragma an (map toNamed ns2) | ns2 /= []] ++ catMaybes new
        mkRefact :: ModulePragma S -> Maybe (ModulePragma S) -> [String] -> Refactoring R.SrcSpan
        mkRefact old (maybe "" prettyPrint -> new) ns =
          let ns' = map (\n -> prettyPrint $ LanguagePragma an [toNamed n]) ns
          in
          ModifyComment (toSS old) (intercalate "\n" (filter (not . null) (new: ns')))

data PragmaIdea = SingleComment (ModulePragma S) (ModulePragma S)
                | MultiComment (ModulePragma S) (ModulePragma S) (ModulePragma S)
                | OptionsToComment [ModulePragma S] [ModulePragma S] [Refactoring R.SrcSpan]


pragmaIdea :: PragmaIdea -> Idea
pragmaIdea pidea =
  case pidea of
    SingleComment old new ->
      mkIdea (srcInfoSpan . ann $ old)
        (prettyPrint old) (Just $ prettyPrint new) []
        [ModifyComment (toSS old) (prettyPrint new)]
    MultiComment repl delete new ->
      mkIdea (srcInfoSpan . ann $ repl)
        (f [repl, delete]) (Just $ prettyPrint new) []
        [ ModifyComment (toSS repl) (prettyPrint new)
        , ModifyComment (toSS delete) ""]
    OptionsToComment old new r ->
      mkIdea (srcInfoSpan . ann . head $ old)
        (f old) (Just $ f new) []
        r
    where
          f = unlines . map prettyPrint
          mkIdea = rawIdea Warning "Use better pragmas"


languageDupes :: [ModulePragma S] -> [Idea]
languageDupes (a@(LanguagePragma _ x):xs) =
    (if nub_ x `neqList` x
        then [pragmaIdea (SingleComment a (LanguagePragma (ann a) $ nub_ x))]
        else [pragmaIdea (MultiComment a b (LanguagePragma (ann a) (nub_ $ x ++ y))) | b@(LanguagePragma _ y) <- xs, not $ null $ intersect_ x y]) ++
    languageDupes xs
languageDupes _ = []


-- Given a pragma, can you extract some language features out
strToLanguage :: String -> Maybe [String]
strToLanguage "-cpp" = Just ["CPP"]
strToLanguage x | "-X" `isPrefixOf` x = Just [drop 2 x]
strToLanguage "-fglasgow-exts" = Just $ map prettyExtension glasgowExts
strToLanguage _ = Nothing


optToLanguage :: ModulePragma S -> [String] -> Maybe (Maybe (ModulePragma S), [String])
optToLanguage (OptionsPragma sl tool val) ls
    | maybe True (== GHC) tool && any isJust vs =
      Just (res, filter (not . (`elem` ls)) (concat $ catMaybes vs))
    where
        strs = words val
        vs = map strToLanguage strs
        keep = concat $ zipWith (\v s -> [s | isNothing v]) vs strs
        res = if null keep then Nothing else Just $ OptionsPragma sl tool (unwords keep)
optToLanguage _ _ = Nothing

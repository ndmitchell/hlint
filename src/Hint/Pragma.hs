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


module Hint.Pragma where

import Hint.Type
import Data.List
import Data.Maybe
import Util


pragmaHint :: ModuHint
pragmaHint _ x = languageDupes lang ++ [pragmaIdea old $ [LanguagePragma an (map toNamed ns2) | ns2 /= []] ++ catMaybes new | old /= []]
    where
        lang = [x | x@LanguagePragma{} <- modulePragmas x]
        (old,new,ns) = unzip3 [(old,new,ns) | old <- modulePragmas x, Just (new,ns) <- [optToLanguage old]]
        ns2 = nub (concat ns) \\ concat [map fromNamed n | LanguagePragma _ n <- lang]


pragmaIdea :: [ModulePragma S] -> [ModulePragma S] -> Idea
pragmaIdea xs ys = rawIdea Error "Use better pragmas" (toSrcLoc $ ann $ head xs) (f xs) (f ys) []
    where f = unlines . map prettyPrint


languageDupes :: [ModulePragma S] -> [Idea]
languageDupes [] = []
languageDupes (a@(LanguagePragma _ x):xs) =
    (if nub_ x `neqList` x
        then [pragmaIdea [a] [LanguagePragma an $ nub_ x]]
        else [pragmaIdea [a,b] [LanguagePragma an (nub_ $ x ++ y)] | b@(LanguagePragma _ y) <- xs, notNull $ intersect_ x y]) ++
    languageDupes xs


-- Given a pragma, can you extract some language features out
strToLanguage :: String -> Maybe [String]
strToLanguage "-cpp" = Just ["CPP"]
strToLanguage x | "-X" `isPrefixOf` x = Just [drop 2 x]
strToLanguage "-fglasgow-exts" = Just $ map show glasgowExts
strToLanguage _ = Nothing


optToLanguage :: ModulePragma S -> Maybe (Maybe (ModulePragma S), [String])
optToLanguage (OptionsPragma sl tool val)
    | maybe True (== GHC) tool && any isJust vs = Just (res, concat $ catMaybes vs)
    where
        strs = words val
        vs = map strToLanguage strs
        keep = concat $ zipWith (\v s -> [s | isNothing v]) vs strs
        res = if null keep then Nothing else Just $ OptionsPragma sl tool (unwords keep)
optToLanguage _ = Nothing

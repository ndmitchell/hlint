{-
    Suggest better pragmas
    OPTIONS_GHC -cpp => LANGUAGE CPP
    OPTIONS_GHC -fglasgow-exts => LANGUAGE ... (in HSE)
    OPTIONS_GHC -XFoo => LANGUAGE Foo
    LANGUAGE A, A => LANGUAGE A
    -- do not do LANGUAGE A, LANGUAGE B to combine
<TEST>
</TEST>
-}


module Hint.Pragma where

import HSE.All
import Type
import Data.List
import Data.Maybe
import Data.Function


pragmaHint :: ModuHint
pragmaHint _ x = languageDupes lang ++ [pragmaIdea old $ [LanguagePragma nullSrcLoc ns2 | ns2 /= []] ++ catMaybes new | old /= []]
    where
        lang = [x | x@LanguagePragma{} <- modulePragmas x]
        (old,new,ns) = unzip3 [(old,new,ns) | old <- modulePragmas x, Just (new,ns) <- [optToLanguage old]]
        ns2 = nub (concat ns) \\ concat [n | LanguagePragma _ n <- lang]


pragmaIdea :: [OptionPragma] -> [OptionPragma] -> Idea
pragmaIdea xs ys = rawIdea Error "Use better pragmas" (fromJust $ getSrcLoc $ head xs) (f xs) (f ys)
    where f = unlines . map prettyPrint


languageDupes :: [OptionPragma] -> [Idea]
languageDupes [] = []
languageDupes (a@(LanguagePragma sl x):xs) =
    (if nub x /= x
        then [pragmaIdea [a] [LanguagePragma sl $ nub x]]
        else [pragmaIdea [a,b] [LanguagePragma sl (nub $ x ++ y)] | b@(LanguagePragma _ y) <- xs, not $ null $ intersect x y]) ++
    languageDupes xs


-- Given a pragma, can you extract some language features out
strToLanguage :: String -> Maybe [Name]
strToLanguage "-cpp" = Just [Ident "CPP"]
strToLanguage x | "-X" `isPrefixOf` x = Just [Ident $ drop 2 x]
strToLanguage "-fglasgow-exts" = Just $ map (Ident . show) glasgowExts
strToLanguage _ = Nothing


optToLanguage :: OptionPragma -> Maybe (Maybe OptionPragma, [Name])
optToLanguage (OptionsPragma sl tool val)
    | maybe True (== GHC) tool && any isJust vs = Just (res, concat $ catMaybes vs)
    where
        strs = words val
        vs = map strToLanguage strs
        keep = concat $ zipWith (\v s -> [s | isNothing v]) vs strs
        res = if null keep then Nothing else Just $ OptionsPragma sl tool (unwords keep)
optToLanguage _ = Nothing

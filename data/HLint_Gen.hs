
import Data.List
import Data.Char
import System.Cmd
import System.IO
import System.Directory

main = do
    hseSyntax <- readFile "../haskell-src-exts/src/Language/Haskell/Exts/Annotated/Syntax.hs"
    hseSrcLoc <- readFile "../haskell-src-exts/src/Language/Haskell/Exts/SrcLoc.hs"
    commands  <- readFile "src/HSE/Type.hs"
    writeFile "Derive.tmp" $ unlines $ ["{-"] ++ f hseSyntax ++ f hseSrcLoc ++ f commands
    system "derive Derive.tmp > Derive2.tmp"
    src <- readFile "Derive2.tmp"
    withBinaryFile "src/HSE/Uniplate.hs" WriteMode $ flip hPutStr $ unlines prefix ++ filter (/= '\r') src
    system "runhaskell -isrc;. src/Main.hs -v 2> Derive3.tmp"
    src <- readFile "Derive3.tmp"
    putStrLn $ unlines $ concatMap g $ lines src
    removeFile "Derive.tmp"
    removeFile "Derive2.tmp"
    removeFile "Derive3.tmp"
    where
        f = filter (\x -> not $ any (`isPrefixOf` dropWhile isSpace x) bad) .
            dropWhile (\x -> not $ any (`isPrefixOf` x) ["data","type"]) . lines
        bad = ["#","deriving ("]

        g x | "    No instance for (Biplate " `isPrefixOf` x = h $ drop 29 x
            | "    No instance for (Uniplate " `isPrefixOf` x = h $ drop 30 x
            | otherwise = []
        
        h x = ["deriving instance UniplateDirect " ++ init x]


prefix =
    ["{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}"
    ,"module HSE.Uniplate(module Data.Generics.Uniplate.Direct) where"
    ,"import Data.Generics.Uniplate.Direct"
    ,"import Language.Haskell.Exts.Annotated"
    ,"type S = SrcSpanInfo"
    ]

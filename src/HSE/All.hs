
module HSE.All(
    module Language.Haskell.Exts,
    module HSE.Util, module HSE.Evaluate,
    module HSE.Bracket, module HSE.Match,
    module HSE.Generics,
    module HSE.NameMatch,
    parseFile, parseString
    ) where

import Language.Haskell.Exts hiding (parse, parseFile, paren)
import qualified Language.Haskell.Exts as HSE

import HSE.Util
import HSE.Evaluate
import HSE.Generics
import HSE.Bracket
import HSE.Match
import HSE.NameMatch
import Util
import System.IO.Unsafe(unsafeInterleaveIO)



-- | Parse a Haskell module
parse :: FilePath -> String -> ParseResult Module
parse file = parseFileContentsWithMode mode
    where
        mode = defaultParseMode
            {parseFilename = file
            ,extensions = extension
            ,fixities = infix_ (-1) ["==>"] ++ baseFixities
            }


-- | On failure returns an empty module and prints to the console
parseFile :: FilePath -> IO Module
parseFile file = unsafeInterleaveIO $ do
    src <- readFile file
    case parse file src of
        ParseOk x -> return x
        ParseFailed src msg -> do
            putStrLn $ showSrcLoc src ++ " Parse failure, " ++ limit 50 msg
            return $ Module nullSrcLoc (ModuleName "") [] Nothing Nothing [] []


-- | On failure crashes
parseString :: String -> String -> Module
parseString file src =
    case parse file src of
        ParseOk x -> x
        _ -> error $ "Parse failure in " ++ file ++ "\n" ++ src



extension =
    [OverlappingInstances, UndecidableInstances, IncoherentInstances, RecursiveDo
    ,ParallelListComp, MultiParamTypeClasses, NoMonomorphismRestriction, FunctionalDependencies
    ,Rank2Types, RankNTypes, PolymorphicComponents, ExistentialQuantification, ScopedTypeVariables
    ,ImplicitParams,FlexibleContexts,FlexibleInstances,EmptyDataDecls
    -- NOT: CPP
    ,KindSignatures,BangPatterns,TypeSynonymInstances,TemplateHaskell
    ,ForeignFunctionInterface,Arrows,Generics,NoImplicitPrelude,NamedFieldPuns,PatternGuards
    ,GeneralizedNewtypeDeriving,ExtensibleRecords,RestrictedTypeSynonyms,HereDocuments
    ,MagicHash,TypeFamilies,StandaloneDeriving,UnicodeSyntax,PatternSignatures,UnliftedFFITypes
    ,LiberalTypeSynonyms,TypeOperators,RecordWildCards,RecordPuns,DisambiguateRecordFields
    ,OverloadedStrings,GADTs,MonoPatBinds,RelaxedPolyRec,ExtendedDefaultRules,UnboxedTuples
    ,DeriveDataTypeable,ConstrainedClassMethods,PackageImports,ImpredicativeTypes
    ,NewQualifiedOperators,PostfixOperators,QuasiQuotes,ViewPatterns
    -- ,TransformListComp - FIXME: Currently screwed up by HSE
    -- NOT: XmlSyntax, RegularPatterns
    ]



module HSE.All(
    module HSE.Util, module HSE.Evaluate,
    module HSE.Bracket, module HSE.Match,
    module HSE.Generics,
    module HSE.NameMatch,
    ParseFlags(..), parseFlags, parseFile, parseString
    ) where

import HSE.Util
import HSE.Evaluate
import HSE.Generics
import HSE.Bracket
import HSE.Match
import HSE.NameMatch
import Data.Char
import Data.List
import Language.Preprocessor.Cpphs


data ParseFlags = ParseFlags
    {cpphs :: Maybe CpphsOptions
    ,implies :: Bool
    }

parseFlags :: ParseFlags
parseFlags = ParseFlags Nothing False


-- | Parse a Haskell module
parseString :: ParseFlags -> FilePath -> String -> ParseResult Module_
parseString flags file = parseFileContentsWithMode mode . maybe id (`runCpphs` file) (cpphs flags)
    where
        mode = defaultParseMode
            {parseFilename = file
            ,extensions = extension
            ,fixities = concat [infix_ (-1) ["==>"] | implies flags] ++ baseFixities
            }


parseFile :: ParseFlags -> FilePath -> IO (ParseResult Module_)
parseFile flags file = do
    src <- readFile file
    return $ parseString flags file src


extension =
    [OverlappingInstances, UndecidableInstances, IncoherentInstances, RecursiveDo
    ,ParallelListComp, MultiParamTypeClasses, NoMonomorphismRestriction, FunctionalDependencies
    ,Rank2Types, RankNTypes, PolymorphicComponents, ExistentialQuantification, ScopedTypeVariables
    ,ImplicitParams,FlexibleContexts,FlexibleInstances,EmptyDataDecls
    -- NOT: CPP
    ,KindSignatures,BangPatterns,TypeSynonymInstances,TemplateHaskell
    ,ForeignFunctionInterface,Generics,NoImplicitPrelude,NamedFieldPuns,PatternGuards
    ,GeneralizedNewtypeDeriving,ExtensibleRecords,RestrictedTypeSynonyms,HereDocuments
    ,MagicHash,TypeFamilies,StandaloneDeriving,UnicodeSyntax,PatternSignatures,UnliftedFFITypes
    ,LiberalTypeSynonyms,TypeOperators,RecordWildCards,RecordPuns,DisambiguateRecordFields
    ,OverloadedStrings,GADTs,MonoPatBinds,RelaxedPolyRec,ExtendedDefaultRules,UnboxedTuples
    ,DeriveDataTypeable,ConstrainedClassMethods,PackageImports,ImpredicativeTypes
    ,NewQualifiedOperators,PostfixOperators,QuasiQuotes,ViewPatterns
    -- NOT: Arrows - steals proc
    -- NOT: TransformListComp - steals the group keyword
    -- NOT: XmlSyntax, RegularPatterns - steals a-b
    ]

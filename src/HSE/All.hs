
module HSE.All(
    module HSE.Util, module HSE.Evaluate,
    module HSE.Bracket, module HSE.Match,
    module HSE.Type, module HSE.Eq,
    module HSE.NameMatch,
    ParseFlags(..), parseFlags, parseFile, parseString
    ) where

import Util
import Data.List
import HSE.Util
import HSE.Evaluate
import HSE.Eq
import HSE.Type
import HSE.Bracket
import HSE.Match
import HSE.NameMatch
import Language.Preprocessor.Cpphs


data ParseFlags = ParseFlags
    {cpphs :: Maybe CpphsOptions
    ,implies :: Bool
    ,encoding :: String
    }

parseFlags :: ParseFlags
parseFlags = ParseFlags Nothing False ""


-- | Parse a Haskell module
parseString :: ParseFlags -> FilePath -> String -> ParseResult Module_
parseString flags file = parseFileContentsWithMode mode . maybe id (`runCpphs` file) (cpphs flags)
    where
        mode = defaultParseMode
            {parseFilename = file
            ,extensions = extension
            ,fixities = concat [infix_ (-1) ["==>"] | implies flags] ++ baseFixities
            ,ignoreLinePragmas = False
            }


parseFile :: ParseFlags -> FilePath -> IO (ParseResult Module_)
parseFile flags file = do
    src <- readFileEncoding (encoding flags) file
    return $ parseString flags file src


extension = knownExtensions \\ badExtensions

badExtensions =
    [CPP
    ,Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ]

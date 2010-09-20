
module HSE.All(
    module HSE.Util, module HSE.Evaluate,
    module HSE.Bracket, module HSE.Match,
    module HSE.Type,
    module HSE.NameMatch,
    ParseFlags(..), parseFlags, parseFlagsNoLocations,
    parseFile, parseString, parseResult
    ) where

import Util
import CmdLine
import Data.Maybe
import HSE.Util
import HSE.Evaluate
import HSE.Type
import HSE.Bracket
import HSE.Match
import HSE.NameMatch
import Language.Preprocessor.Cpphs


data ParseFlags = ParseFlags
    {cppFlags :: CppFlags
    ,language :: [Extension]
    ,encoding :: String
    ,infixes :: [Fixity]
    }

parseFlags :: ParseFlags
parseFlags = ParseFlags NoCpp defaultExtensions "" []

parseFlagsNoLocations :: ParseFlags -> ParseFlags
parseFlagsNoLocations x = x{cppFlags = case cppFlags x of Cpphs y -> Cpphs $ f y; y -> y}
    where f x = x{boolopts = (boolopts x){locations=False}}


runCpp :: CppFlags -> FilePath -> String -> IO String
runCpp NoCpp _ x = return x
runCpp (Cpphs o) file x = runCpphs o file x


-- | Parse a Haskell module
parseString :: ParseFlags -> FilePath -> String -> IO (String, ParseResult Module_)
parseString flags file str = do
        ppstr <- runCpp (cppFlags flags) file str
        return (ppstr, fmap (applyFixity fixity) $ parseFileContentsWithMode mode ppstr)
    where
        fixity = infixes flags ++ baseFixities
        mode = defaultParseMode
            {parseFilename = file
            ,extensions = language flags
            ,fixities = []
            ,ignoreLinePragmas = False
            }


-- resolve fixities later, so we don't ever get uncatchable ambiguity errors
-- if there are fixity errors, just ignore them
applyFixity :: [Fixity] -> Module_ -> Module_
applyFixity fixity modu = descendBi f modu
    where
        f x = fromMaybe x $ applyFixities (extra ++ fixity) x :: Decl_
        extra = concatMap getFixity $ moduleDecls modu


parseFile :: ParseFlags -> FilePath -> IO (String, ParseResult Module_)
parseFile flags file = do
    src <- readFileEncoding (encoding flags) file
    parseString flags file src


-- throw an error if the parse is invalid
parseResult :: IO (String, ParseResult Module_) -> IO Module_
parseResult x = do
    (_, res) <- x
    return $! fromParseResult res

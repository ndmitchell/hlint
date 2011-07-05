
module HSE.All(
    module X,
    ParseFlags(..), parseFlags, parseFlagsNoLocations,
    parseFile, parseString, parseResult
    ) where

import HSE.Util as X
import HSE.Evaluate as X
import HSE.Type as X
import HSE.Bracket as X
import HSE.Match as X
import HSE.NameMatch as X
import Util
import CmdLine
import Data.List
import Data.Maybe
import Language.Preprocessor.Cpphs
import qualified Data.Map as Map


data ParseFlags = ParseFlags
    {cppFlags :: CppFlags
    ,language :: [Extension]
    ,encoding :: Encoding
    ,infixes :: [Fixity]
    }

parseFlags :: ParseFlags
parseFlags = ParseFlags NoCpp defaultExtensions defaultEncoding []

parseFlagsNoLocations :: ParseFlags -> ParseFlags
parseFlagsNoLocations x = x{cppFlags = case cppFlags x of Cpphs y -> Cpphs $ f y; y -> y}
    where f x = x{boolopts = (boolopts x){locations=False}}


runCpp :: CppFlags -> FilePath -> String -> IO String
runCpp NoCpp _ x = return x
runCpp CppSimple _ x = return $ unlines [if "#" `isPrefixOf` ltrim x then "" else x | x <- lines x]
runCpp (Cpphs o) file x = runCpphs o file x


---------------------------------------------------------------------
-- PARSING

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
            ,fixities = Nothing
            ,ignoreLinePragmas = False
            }


parseFile :: ParseFlags -> FilePath -> IO (String, ParseResult Module_)
parseFile flags file = do
    src <- readFileEncoding (encoding flags) file
    parseString flags file src


-- throw an error if the parse is invalid
parseResult :: IO (String, ParseResult Module_) -> IO Module_
parseResult x = do
    (_, res) <- x
    return $! fromParseResult res


---------------------------------------------------------------------
-- FIXITIES

-- resolve fixities later, so we don't ever get uncatchable ambiguity errors
-- if there are fixity errors, try the cheapFixities (which never fails)
applyFixity :: [Fixity] -> Module_ -> Module_
applyFixity base modu = descendBi f modu
    where
        f x = fromMaybe (cheapFixities fixs x) $ applyFixities fixs x :: Decl_
        fixs = concatMap getFixity (moduleDecls modu) ++ base


-- Apply fixities, but ignoring any ambiguous fixity errors and skipping qualified names,
-- local infix declarations etc. Only use as a backup, if HSE gives an error.
--
-- Inspired by the code at:
-- http://hackage.haskell.org/trac/haskell-prime/attachment/wiki/FixityResolution/resolve.hs
cheapFixities :: [Fixity] -> Decl_ -> Decl_
cheapFixities fixs = descendBi (transform f)
    where
        ask = askFixity fixs
    
        f o@(InfixApp s1 (InfixApp s2 x op1 y) op2 z)
                | p1 == p2 && (a1 /= a2 || a1 == AssocNone) = o -- Ambiguous infix expression!
                | p1 > p2 || p1 == p2 && (a1 == AssocLeft || a2 == AssocNone) = o
                | otherwise = InfixApp s1 x op1 (f $ InfixApp s1 y op2 z)
            where
                (a1,p1) = ask op1
                (a2,p2) = ask op2
        f x = x


askFixity :: [Fixity] -> QOp S -> (Assoc, Int)
askFixity xs = \k -> Map.findWithDefault (AssocLeft, 9) (fromNamed k) mp
    where
        mp = Map.fromList [(s,(a,p)) | Fixity a p x <- xs, let s = fromNamed x, s /= ""]

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module HSE.All(
    module X,
    CppFlags(..), ParseFlags(..), defaultParseFlags,
    parseFlagsAddFixities, parseFlagsSetLanguage,
    ParseError(..), ModuleEx(..),
    parseModuleEx, ghcComments,
    freeVars, vars, varss, pvars,
    ghcSpanToHSE, ghcSrcLocToHSE
    ) where

import Language.Haskell.Exts.Util hiding (freeVars, Vars(..))
import qualified Language.Haskell.Exts.Util as X
import HSE.Util as X
import HSE.Reduce as X
import HSE.Type as X
import HSE.Match as X
import HSE.Scope as X
import Util
import Data.Char
import Data.List.Extra
import Data.Maybe
import Timing
import Language.Preprocessor.Cpphs
import Data.Either
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO.Extra
import Data.Functor
import Prelude

import qualified HsSyn
import qualified FastString
import qualified SrcLoc as GHC
import qualified ErrUtils
import qualified Outputable
import qualified Lexer as GHC
import qualified GHC.LanguageExtensions.Type as GHC
import qualified ApiAnnotation as GHC
import qualified BasicTypes as GHC

import GHC.Util
import qualified GHC.Util.Refact.Fixity as GHC

-- | Convert a GHC source loc into an HSE equivalent.
ghcSrcLocToHSE :: GHC.SrcLoc -> SrcLoc
ghcSrcLocToHSE (GHC.RealSrcLoc l) =
  SrcLoc {
      srcFilename = FastString.unpackFS (GHC.srcLocFile l)
    , srcLine = GHC.srcLocLine l
    , srcColumn = GHC.srcLocCol l
    }
ghcSrcLocToHSE (GHC.UnhelpfulLoc _) = noLoc

-- | Convert a GHC source span into an HSE equivalent.
ghcSpanToHSE :: GHC.SrcSpan -> SrcSpan
ghcSpanToHSE (GHC.RealSrcSpan s) =
  SrcSpan {
      srcSpanFilename = FastString.unpackFS (GHC.srcSpanFile s)
    , srcSpanStartLine = GHC.srcSpanStartLine s
    , srcSpanStartColumn = GHC.srcSpanStartCol s
    , srcSpanEndLine = GHC.srcSpanEndLine s
    , srcSpanEndColumn = GHC.srcSpanEndCol s
    }
ghcSpanToHSE (GHC.UnhelpfulSpan _) = mkSrcSpan noLoc noLoc

vars :: FreeVars a => a -> [String]
freeVars :: FreeVars a => a -> Set String
varss, pvars :: AllVars a => a -> [String]
vars  = Set.toList . Set.map prettyPrint . X.freeVars
varss = Set.toList . Set.map prettyPrint . X.free . X.allVars
pvars = Set.toList . Set.map prettyPrint . X.bound . X.allVars
freeVars = Set.map prettyPrint . X.freeVars

-- | What C pre processor should be used.
data CppFlags
    = NoCpp -- ^ No pre processing is done.
    | CppSimple -- ^ Lines prefixed with @#@ are stripped.
    | Cpphs CpphsOptions -- ^ The @cpphs@ library is used.

-- | Created with 'defaultParseFlags', used by 'parseModuleEx'.
data ParseFlags = ParseFlags
    {cppFlags :: CppFlags -- ^ How the file is preprocessed (defaults to 'NoCpp').
    ,hseFlags :: ParseMode -- ^ How the file is parsed (defaults to all fixities in the @base@ package and most non-conflicting extensions).
    }

lensFixities :: [Fixity]
lensFixities = concat
    -- List as provided at https://github.com/ndmitchell/hlint/issues/416
    [infixr_ 4 ["%%@~","<%@~","%%~","<+~","<*~","<-~","<//~","<^~","<^^~","<**~"]
    ,infix_ 4 ["%%@=","<%@=","%%=","<+=","<*=","<-=","<//=","<^=","<^^=","<**="]
    ,infixr_ 2 ["<<~"]
    ,infixr_ 9 ["#."]
    ,infixl_ 8 [".#"]
    ,infixr_ 8 ["^!","^@!"]
    ,infixl_ 1 ["&","<&>","??"]
    ,infixl_ 8 ["^.","^@."]
    ,infixr_ 9 ["<.>","<.",".>"]
    ,infixr_ 4 ["%@~",".~","+~","*~","-~","//~","^~","^^~","**~","&&~","<>~","||~","%~"]
    ,infix_ 4 ["%@=",".=","+=","*=","-=","//=","^=","^^=","**=","&&=","<>=","||=","%="]
    ,infixr_ 2 ["<~"]
    ,infixr_ 2 ["`zoom`","`magnify`"]
    ,infixl_ 8 ["^..","^?","^?!","^@..","^@?","^@?!"]
    ,infixl_ 8 ["^#"]
    ,infixr_ 4 ["<#~","#~","#%~","<#%~","#%%~"]
    ,infix_ 4 ["<#=","#=","#%=","<#%=","#%%="]
    ,infixl_ 9 [":>"]
    ,infixr_ 4 ["</>~","<</>~","<.>~","<<.>~"]
    ,infix_ 4 ["</>=","<</>=","<.>=","<<.>="]
    ,infixr_ 4 [".|.~",".&.~","<.|.~","<.&.~"]
    ,infix_ 4 [".|.=",".&.=","<.|.=","<.&.="]
    ]

otherFixities :: [Fixity]
otherFixities = concat
    -- hspec
    [infix_ 1 ["`shouldBe`","`shouldSatisfy`","`shouldStartWith`","`shouldEndWith`","`shouldContain`","`shouldMatchList`"
              ,"`shouldReturn`","`shouldNotBe`","`shouldNotSatisfy`","`shouldNotContain`","`shouldNotReturn`","`shouldThrow`"]
    -- quickcheck
    ,infixr_ 0 ["==>"]
    ,infix_ 4 ["==="]
    -- esqueleto
    ,infix_ 4 ["==."]
    -- lattices
    ,infixr_ 5 ["\\/"] -- \/
    ,infixr_ 6 ["/\\"] -- /\
    ]

-- Fixites from the `base` package which are currently
-- missing from `haskell-src-exts`'s baseFixities.
-- see https://github.com/haskell-suite/haskell-src-exts/pull/400
baseNotYetInHSE :: [Fixity]
baseNotYetInHSE = concat
    [infixr_ 9 ["`Compose`"]
    ,infixr_ 6 ["<>"]
    ,infixr_ 5 ["<|"]
    ,infixl_ 4 ["<$!>","<$","$>"]
    ,infix_ 4 [":~:", ":~~:"]
    ]

customFixities :: [Fixity]
customFixities =
    infixl_ 1 ["`on`"]
        -- see https://github.com/ndmitchell/hlint/issues/425
        -- otherwise GTK apps using `on` at a different fixity have spurious warnings

-- | Default value for 'ParseFlags'.
defaultParseFlags :: ParseFlags
defaultParseFlags = ParseFlags NoCpp defaultParseMode
    {fixities = Just $ customFixities ++ baseFixities ++ baseNotYetInHSE ++ lensFixities ++ otherFixities
    ,ignoreLinePragmas = False
    ,ignoreFunctionArity = True
    ,extensions = parseExtensions}

parseFlagsNoLocations :: ParseFlags -> ParseFlags
parseFlagsNoLocations x = x{cppFlags = case cppFlags x of Cpphs y -> Cpphs $ f y; y -> y}
    where f x = x{boolopts = (boolopts x){locations=False}}

-- | Given some fixities, add them to the existing fixities in 'ParseFlags'.
parseFlagsAddFixities :: [Fixity] -> ParseFlags -> ParseFlags
parseFlagsAddFixities fx x = x{hseFlags=hse{fixities = Just $ fx ++ fromMaybe [] (fixities hse)}}
    where hse = hseFlags x

parseFlagsSetLanguage :: (Language, [Extension]) -> ParseFlags -> ParseFlags
parseFlagsSetLanguage (l, es) x = x{hseFlags=(hseFlags x){baseLanguage = l, extensions = es}}


runCpp :: CppFlags -> FilePath -> String -> IO String
runCpp NoCpp _ x = return x
runCpp CppSimple _ x = return $ unlines [if "#" `isPrefixOf` trimStart x then "" else x | x <- lines x]
runCpp (Cpphs o) file x = dropLine <$> runCpphs o file x
    where
        -- LINE pragmas always inserted when locations=True
        dropLine (line1 -> (a,b)) | "{-# LINE " `isPrefixOf` a = b
        dropLine x = x

---------------------------------------------------------------------
-- PARSING

-- | A parse error.
data ParseError = ParseError
    { parseErrorLocation :: SrcLoc -- ^ Location of the error.
    , parseErrorMessage :: String  -- ^ Message about the cause of the error.
    , parseErrorContents :: String -- ^ Snippet of several lines (typically 5) including a @>@ character pointing at the faulty line.
    }

-- | Result of 'parseModuleEx', representing a parsed module.
data ModuleEx = ModuleEx {
    hseModule :: Module SrcSpanInfo
  , hseComments :: [Comment]
  , ghcModule :: GHC.Located (HsSyn.HsModule HsSyn.GhcPs)
  , ghcAnnotations :: GHC.ApiAnns
}

-- | Extract a list of all of a parsed module's comments.
ghcComments :: ModuleEx -> [GHC.Located GHC.AnnotationComment]
ghcComments m = concat (Map.elems $ snd (ghcAnnotations m))

-- | Utility called from 'parseModuleEx' and 'hseFailOpParseModuleEx'.
mkMode :: ParseFlags -> String -> ParseMode
mkMode flags file = (hseFlags flags){ parseFilename = file,fixities = Nothing }

-- | Error handler dispatcher. Invoked when HSE parsing has failed.
failOpParseModuleEx :: String
                    -> ParseFlags
                    -> FilePath
                    -> String
                    -> SrcLoc
                    -> String
                    -> Maybe (GHC.SrcSpan, ErrUtils.MsgDoc)
                    -> IO (Either ParseError ModuleEx)
failOpParseModuleEx ppstr flags file str sl msg ghc =
   case ghc of
     Just err ->
       -- GHC error info is available (assumed to have come from a
       -- 'PFailed'). We prefer to construct a 'ParseError' value
       -- using that.
       ghcFailOpParseModuleEx ppstr file str err
     Nothing ->
       -- No GHC error info provided. This is the traditional approach
       -- to handling errors.
       hseFailOpParseModuleEx ppstr flags file str sl msg

-- | An error handler of last resort. This is invoked when HSE parsing
-- has failed but apparently GHC has not!
hseFailOpParseModuleEx :: String
                       -> ParseFlags
                       -> FilePath
                       -> String
                       -> SrcLoc
                       -> String
                       -> IO (Either ParseError ModuleEx)
hseFailOpParseModuleEx ppstr flags file str sl msg = do
    flags <- return $ parseFlagsNoLocations flags
    ppstr2 <- runCpp (cppFlags flags) file str
    let pe = case parseFileContentsWithMode (mkMode flags file) ppstr2 of
               ParseFailed sl2 _ -> context (srcLine sl2) ppstr2
               _ -> context (srcLine sl) ppstr
    return $ Left $ ParseError sl msg pe

-- | The error handler invoked when GHC parsing has failed.
ghcFailOpParseModuleEx :: String
                       -> FilePath
                       -> String
                       -> (GHC.SrcSpan, ErrUtils.MsgDoc)
                       -> IO (Either ParseError ModuleEx)
ghcFailOpParseModuleEx ppstr file str (loc, err) = do
   let sl =
         case loc of
           GHC.RealSrcSpan r ->
             SrcLoc { srcFilename = FastString.unpackFS (GHC.srcSpanFile r)
                     , srcLine = GHC.srcSpanStartLine r
                     , srcColumn = GHC.srcSpanStartCol r }
           GHC.UnhelpfulSpan _ ->
             SrcLoc { srcFilename = file
                     , srcLine = 1 :: Int
                     , srcColumn = 1 :: Int }
       pe = context (srcLine sl) ppstr
       msg = Outputable.showSDoc baseDynFlags $
               ErrUtils.pprLocErrMsg (ErrUtils.mkPlainErrMsg baseDynFlags loc err)
   return $ Left $ ParseError sl msg pe

-- | Produce a pair of lists from a 'ParseFlags' value representing
-- language extensions to explicitly enable/disable.
ghcExtensionsFromParseFlags :: ParseFlags
                             -> ([GHC.Extension], [GHC.Extension])
ghcExtensionsFromParseFlags ParseFlags {hseFlags=ParseMode {extensions=exts}}=
   partitionEithers $ mapMaybe toEither exts
   where
     toEither ke = case ke of
       EnableExtension e  -> Left  <$> readExtension (show e)
       DisableExtension e -> Right <$> readExtension (show e)
       UnknownExtension ('N':'o':e) -> Right <$> readExtension e
       UnknownExtension e -> Left <$> readExtension e

-- A hacky function to get fixities from HSE parse flags suitable for
-- use by our own 'GHC.Util.Refact.Fixity' module.
ghcFixitiesFromParseFlags :: ParseFlags -> [(String, GHC.Fixity)]
ghcFixitiesFromParseFlags ParseFlags {hseFlags=ParseMode{fixities=Just fixities}} =
  concatMap convert fixities
  where
    convert (Fixity (AssocNone _) fix name) = infix_' fix [qNameToStr name]
    convert (Fixity (AssocLeft _) fix name) = infixl_' fix [qNameToStr name]
    convert (Fixity (AssocRight _) fix name) = infixr_' fix [qNameToStr name]

    infixr_', infixl_', infix_' :: Int -> [String] -> [(String,GHC.Fixity)]
    infixr_' = fixity' GHC.InfixR
    infixl_' = fixity' GHC.InfixL
    infix_'  = fixity' GHC.InfixN

    fixity' :: GHC.FixityDirection -> Int -> [String] -> [(String, GHC.Fixity)]
    fixity' a p = map (,GHC.Fixity (GHC.SourceText "") p a)

    qNameToStr :: QName () -> String
    qNameToStr (Special _ Cons{}) = ":"
    qNameToStr (Special _ UnitCon{}) = "()"
    qNameToStr (UnQual _ (X.Ident _ x)) = x
    qNameToStr (UnQual _ (Symbol _ x)) = x
    qNameToStr _ = ""
ghcFixitiesFromParseFlags _ = []

-- | Parse a Haskell module. Applies the C pre processor, and uses
-- best-guess fixity resolution if there are ambiguities.  The
-- filename @-@ is treated as @stdin@. Requires some flags (often
-- 'defaultParseFlags'), the filename, and optionally the contents of
-- that file. This version uses both hs-src-exts AND ghc-lib.
parseModuleEx :: ParseFlags -> FilePath -> Maybe String -> IO (Either ParseError ModuleEx)
parseModuleEx flags file str = timedIO "Parse" file $ do
        str <- case str of
            Just x -> return x
            Nothing | file == "-" -> getContentsUTF8
                    | otherwise -> readFileUTF8' file
        str <- return $ fromMaybe str $ stripPrefix "\65279" str -- remove the BOM if it exists, see #130
        ppstr <- runCpp (cppFlags flags) file str
        let enableDisableExts = ghcExtensionsFromParseFlags flags
            fixities = ghcFixitiesFromParseFlags flags -- Note : Fixities are coming from HSE parse flags.
        dynFlags <- parsePragmasIntoDynFlags baseDynFlags enableDisableExts file ppstr
        case dynFlags of
          Right ghcFlags ->
            case (parseFileContentsWithComments (mkMode flags file) ppstr, parseFileGhcLib file ppstr ghcFlags) of
                (ParseOk (x, cs), GHC.POk pst a) ->
                    let anns =
                          ( Map.fromListWith (++) $ GHC.annotations pst
                          , Map.fromList ((GHC.noSrcSpan, GHC.comment_q pst) : GHC.annotations_comments pst)
                          ) in
                    let (_, a') = GHC.applyFixities Map.empty fixities a in
                    return $ Right (ModuleEx (applyFixity fixity x) cs a' anns)
                -- Parse error if GHC parsing fails (see
                -- https://github.com/ndmitchell/hlint/issues/645).
                (ParseOk _,  GHC.PFailed _ loc err) ->
                    ghcFailOpParseModuleEx ppstr file str (loc, err)
                (ParseFailed sl msg, pfailed) ->
                    failOpParseModuleEx ppstr flags file str sl msg $ fromPFailed pfailed
          Left msg -> do
            -- Parsing GHC flags from dynamic pragmas in the source
            -- has failed. When this happens, it's reported by
            -- exception. It's impossible or at least fiddly getting a
            -- location so we skip that for now. Synthesize a parse
            -- error.
            let loc = SrcLoc file (1 :: Int) (1 :: Int)
            return $ Left (ParseError loc msg (context (srcLine loc) ppstr))

    where
        fromPFailed (GHC.PFailed _ loc err) = Just (loc, err)
        fromPFailed _ = Nothing

        fixity = fromMaybe [] $ fixities $ hseFlags flags

-- | Given a line number, and some source code, put bird ticks around the appropriate bit.
context :: Int -> String -> String
context lineNo src =
    unlines $ dropWhileEnd (all isSpace) $ dropWhile (all isSpace) $
    zipWith (++) ticks $ take 5 $ drop (lineNo - 3) $ lines src ++ ["","","","",""]
    where ticks = ["  ","  ","> ","  ","  "]


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
                | p1 == p2 && (a1 /= a2 || isAssocNone a1) = o -- Ambiguous infix expression!
                | p1 > p2 || p1 == p2 && (isAssocLeft a1 || isAssocNone a2) = o
                | otherwise = InfixApp s1 x op1 (f $ InfixApp s1 y op2 z)
            where
                (a1,p1) = ask op1
                (a2,p2) = ask op2
        f x = x


askFixity :: [Fixity] -> QOp S -> (Assoc (), Int)
askFixity xs = \k -> Map.findWithDefault (AssocLeft (), 9) (fromNamed k) mp
    where
        mp = Map.fromList [(s,(a,p)) | Fixity a p x <- xs, let s = fromNamed $ fmap (const an) x, s /= ""]

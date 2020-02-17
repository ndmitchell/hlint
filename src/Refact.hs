module Refact
    ( toRefactSrcSpan, toRefactSrcSpan'
    , toSS, toSS'
    , toSrcSpan'
    , checkRefactor, refactorPath, runRefactoring
    ) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Version.Extra
import System.Directory.Extra
import System.Exit
import System.IO.Extra
import System.Process.Extra
import qualified Refact.Types as R
import HSE.All

import qualified SrcLoc as GHC

toRefactSrcSpan :: SrcSpan -> R.SrcSpan
toRefactSrcSpan ss = R.SrcSpan (srcSpanStartLine ss)
                               (srcSpanStartColumn ss)
                               (srcSpanEndLine ss)
                               (srcSpanEndColumn ss)

toRefactSrcSpan' :: GHC.SrcSpan -> R.SrcSpan
toRefactSrcSpan' = toRefactSrcSpan . ghcSpanToHSE

toSS :: Annotated a => a S -> R.SrcSpan
toSS = toRefactSrcSpan . srcInfoSpan . ann

-- | Don't crash in case ghc gives us a \"fake\" span,
-- opting instead to show @0 0 0 0@ coordinates.
toSrcSpan' :: GHC.HasSrcSpan a => a -> R.SrcSpan
toSrcSpan' x = case GHC.getLoc x of
    GHC.RealSrcSpan span ->
        R.SrcSpan (GHC.srcSpanStartLine span)
                  (GHC.srcSpanStartCol span)
                  (GHC.srcSpanEndLine span)
                  (GHC.srcSpanEndCol span)
    GHC.UnhelpfulSpan _ ->
        R.SrcSpan 0 0 0 0

toSS' :: GHC.HasSrcSpan e => e -> R.SrcSpan
toSS' = toRefactSrcSpan' . GHC.getLoc

checkRefactor :: Maybe FilePath -> IO FilePath
checkRefactor = refactorPath >=> either error pure

refactorPath :: Maybe FilePath -> IO (Either String FilePath)
refactorPath rpath = do
    let excPath = fromMaybe "refactor" rpath
    mexc <- findExecutable excPath
    case mexc of
        Just exc -> do
            ver <- readVersion . tail <$> readProcess exc ["--version"] ""
            pure $ if versionBranch ver >= [0,1,0,0]
                       then Right exc
                       else Left "Your version of refactor is too old, please upgrade to the latest version"
        Nothing -> pure $ Left $ unlines [ "Could not find refactor", "Tried with: " ++ excPath ]

runRefactoring :: FilePath -> FilePath -> FilePath -> String -> IO ExitCode
runRefactoring rpath fin hints opts =  do
    let args = [fin, "-v0"] ++ words opts ++ ["--refact-file", hints]
    (_, _, _, phand) <- createProcess $ proc rpath args
    try $ hSetBuffering stdin LineBuffering :: IO (Either IOException ())
    hSetBuffering stdout LineBuffering
    -- Propagate the exit code from the spawn process
    waitForProcess phand

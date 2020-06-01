{-# LANGUAGE LambdaCase #-}

module Refact
    ( toRefactSrcSpan
    , toSS
    , checkRefactor, refactorPath, runRefactoring
    ) where

import Control.Exception.Extra
import Control.Monad
import Data.Maybe
import Data.Version.Extra
import GHC.LanguageExtensions.Type
import System.Directory.Extra
import System.Exit
import System.IO.Extra
import System.Process.Extra
import qualified Refact.Types as R

import qualified SrcLoc as GHC

toRefactSrcSpan :: GHC.SrcSpan -> R.SrcSpan
toRefactSrcSpan = \case
    GHC.RealSrcSpan span ->
        R.SrcSpan (GHC.srcSpanStartLine span)
                  (GHC.srcSpanStartCol span)
                  (GHC.srcSpanEndLine span)
                  (GHC.srcSpanEndCol span)
    GHC.UnhelpfulSpan _ ->
        R.SrcSpan (-1) (-1) (-1) (-1)

-- | Don't crash in case ghc gives us a \"fake\" span,
-- opting instead to show @-1 -1 -1 -1@ coordinates.
toSS :: GHC.HasSrcSpan a => a -> R.SrcSpan
toSS = toRefactSrcSpan . GHC.getLoc

checkRefactor :: Maybe FilePath -> IO FilePath
checkRefactor = refactorPath >=> either errorIO pure

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
        Nothing -> pure $ Left $ unlines
                       [ "Could not find 'refactor' executable"
                       , "Tried to find '" ++ excPath ++ "' on the PATH"
                       , "'refactor' is provided by the 'apply-refact' package and has to be installed"
                       , "<https://github.com/mpickering/apply-refact>"
                       ]

runRefactoring :: FilePath -> FilePath -> FilePath -> [Extension] -> [Extension] -> String -> IO ExitCode
runRefactoring rpath fin hints ys ns opts =  do
    let args = [fin, "-v0"] ++ words opts ++ ["--refact-file", hints]
          ++ [yes | e <- ys, yes <- ["-X", show e]] ++ [no | e <- ns, no <- ["-X", "No" ++ show e]]
    (_, _, _, phand) <- createProcess $ proc rpath args
    try $ hSetBuffering stdin LineBuffering :: IO (Either IOException ())
    hSetBuffering stdout LineBuffering
    -- Propagate the exit code from the spawn process
    waitForProcess phand

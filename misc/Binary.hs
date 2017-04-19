-- This script creates a binary distribution at dist/bin/hlint-$ver.zip

import Control.Monad
import Data.List.Extra
import Data.Maybe
import System.IO.Extra
import System.Process.Extra
import System.FilePath
import System.Directory.Extra


main :: IO ()
main = withTempDir $ \tdir -> do
    system_ $ "cabal sdist --output-directory=" ++ tdir
    vname <- ("hlint-" ++) . getVersion <$> readFile' "hlint.cabal"
    withCurrentDirectory tdir $ do
        system_ "cabal install --dependencies"
        system_ "cabal configure --datadir=nul --disable-library-profiling"
        system_ "cabal build"
        let out = "bin" </> vname
        createDirectoryIfMissing True $ out </> "data"
        copyFile "dist/build/hlint/hlint.exe" $ out </> "hlint.exe"
        files <- (["CHANGES.txt","LICENSE","README.md"]++) <$> listFiles "data"
        forM_ files $ \file -> copyFile file $ out </> file
        withCurrentDirectory "bin" $
            system_ $ "zip -r " ++ vname ++ ".zip " ++ vname
    let res = "dist/ver" </> vname <.> "zip"
    createDirectoryIfMissing True $ takeDirectory res
    copyFile (tdir </> "bin" </> vname <.> "zip") res
    putStrLn $ "Completed, produced " ++ res

getVersion :: String -> String
getVersion = head . map trim .  mapMaybe (stripPrefix "version:") . lines

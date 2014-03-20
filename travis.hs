
import Control.Monad
import System.Cmd
import System.Exit


main :: IO ()
main = do
    cmd "hlint test"
    cmd "time hlint src; true"
    cmd "ghc -threaded -rtsopts -isrc -i. src/Paths.hs src/Main.hs --make -O -prof -auto-all -caf-all"
    cmd "src/Main src +RTS -p; true"
    cmd "cat src/Main.prof"

cmd :: String -> IO ()
cmd x = do
    res <- system x
    when (res /= ExitSuccess) $ error $ "Failed in system command: " ++ x

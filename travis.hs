
import Control.Monad
import System.Cmd
import System.Exit


main :: IO ()
main = cmd "hlint_datadir=data hlint test"


cmd :: String -> IO ()
cmd x = do
    res <- system x
    when (res /= ExitSuccess) $ error $ "Failed in system command: " ++ x

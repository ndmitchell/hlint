
import Control.Exception.Extra
import System.Time.Extra
import System.Process.Extra


main :: IO ()
main = do
    retry 3 $ system_ "cabal install QuickCheck"
    system_ "hlint test --typecheck --quickcheck"
    (time,_) <- duration $ system "hlint src"
    putStrLn $ "Running HLint on self took " ++ showDuration time
